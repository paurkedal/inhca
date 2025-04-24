(* Copyright (C) 2021--2025  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Helpers
open Lwt.Infix
open Lwt.Syntax
module H = Tyxml_html

let log_src = Logs.Src.create "inhca.public"
module Log = (val Logs_lwt.src_log log_src)

type ctx = {
  vpaths: Vpaths.t;
  subject_base_dn: X509_dn.t;
  rsa_key_size: int;
  rsa_key_digest: Digestif.hash';
}

let create_ctx ~vpaths = {
  vpaths;
  subject_base_dn = X509_dn.of_string Config.(global.subject_base_dn);
  rsa_key_size = 4096;
  rsa_key_digest = `SHA512;
}

let cacert_handler ~ctx:_ =
  Dream.from_filesystem
    (Filename.dirname Openssl.cacert_path)
    (Filename.basename Openssl.cacert_path)

let crl_handler ~ctx:_ _req =
  (match%lwt Openssl.gencrl () with
   | Error error ->
      let* () = Openssl.log_error error in
      respond_with_error ~status:`Internal_Server_Error "Could not create CRL."
   | Ok crl ->
      let headers = ["Content-Type", "application/x-pem-file"] in
      Dream.respond ~headers crl)

let acquire_token_cookie = "inhca_acquire_token"

let acquire_login_handler ~ctx req =
  let token = Dream.param req "token" in
  let+ resp = Dream.redirect req ctx.vpaths.acquire in
  Dream.set_cookie ?secure:Config.(global.tls_enabled) ~same_site:(Some `Strict)
    resp req acquire_token_cookie token;
  resp

(*
let enrollment_key =
  Context.Key.create ("enrollment", Inhca_shared.Enrollment.sexp_of_t)
*)

let with_enrollment service req =
  (match Dream.cookie req "inhca_acquire_token" with
   | None ->
      respond_with_error ~status:`Bad_Request "Missing enrollment token."
   | Some token ->
      let send_expired () =
        let+ resp =
          respond_with_message ~status:`Forbidden ~title:"Link Expired"
            "This request has expired. Please ask for a new link when \
             you expect to be available to use it."
        in
        Dream.drop_cookie resp req acquire_token_cookie;
        resp
      in
      try%lwt
        let* enrollment_table = Enrollment.ocsipersist_table in
        let* enrollment = Ocsipersist.find enrollment_table token in
        (match Enrollment.state enrollment with
         | Enrollment.Prepared ->
            if Enrollment.has_expired enrollment then send_expired () else
            let enrollment =
              Enrollment.update ~state:Enrollment.Visited enrollment
            in
            let* () = Enrollment.save_exn enrollment in
            service ~enrollment req
         | Enrollment.Visited ->
            if Enrollment.has_expired enrollment then send_expired () else
            service ~enrollment req
         | Enrollment.Acquired ->
            respond_with_page ~status:`Bad_Request
              ~title:"Certificate Already Delivered"
              [ H.txt
                  "According to our records, your certificate has already been \
                   delivered. \
                   If something went wrong while acquiring it, you will need \
                   to request a new link. ";
                H.b [
                  H.txt
                    "If you did not use the link yourself, please let us know \
                     as soon as possible, "
                ];
                H.txt "so that we can revoke the certificate." ]
         | Enrollment.Revoked ->
            respond_with_message ~status:`Forbidden
              ~title:"Enrollment Token Revoked"
              "This enrollment link has been revoked by the administrator. \
               Please ask for a new link if needed."
         | Enrollment.Failed ->
            Log.err (fun f -> f "Enrollment failed.") >>= fun () ->
            respond_with_message ~status:`Internal_Server_Error
              ~title:"Internal Server Error"
              "Something went wrong when delivering the certificate. \
               Please let us know, so that the administrator can look into \
               what happened and provide a new link.")
      with Not_found ->
        let+ resp =
          respond_with_message ~status:`Forbidden ~title:"Unknown Link"
            "The link has been deleted or is invalid. \
             Please ask for a new link if needed."
        in
        Dream.drop_cookie resp req "inhca_acquire_token";
        resp)

let server_generates_form ~ctx req =
  let form_a = [
    H.a_action ctx.vpaths.issued_pkcs12;
    H.a_method `Post;
    H.a_autocomplete `Off;
  ] in
  H.form ~a:form_a [
    csrf_tag req;
    H.p [
      H.txt
        "If generating the secret key in the browser is not supported \
         you can let the server generate it for you and deliver it along \
         with the certificate as a download.  It can typically be imported \
         into the browser from the certificate management settings, or \
         into the keyring of the operating system.";
    ];
    H.p [
      H.txt "Type a password ";
      H.input ~a:[H.a_input_type `Password; H.a_name "password1"] ();
      H.txt " and again ";
      H.input ~a:[H.a_input_type `Password; H.a_name "password2"] ();
      H.txt ", to protect the secret key in the download.";
      H.br ();
      H.txt "Then, ";
      H.input
        ~a:[
          H.a_input_type `Submit;
          H.a_value "download the key and certificate"]
        ();
      H.txt ".";
    ]
  ]

let acquire_handler' ~ctx ?error ~enrollment:_ req =
  let content = [server_generates_form ~ctx req] in
  respond_with_page ~title:"Acquire Certificate"
    (match error with
     | Some error -> H.div ~a:[H.a_class ["error"]] error :: content
     | None -> content)

let acquire_handler ~ctx =
  with_enrollment (acquire_handler' ~ctx ?error:None)

let error_msg_of_string = Result.map_error (fun s -> `Msg s)

let deliver_certificate ~ctx enrollment password =
  let dn = X509_dn.cn (Enrollment.cn enrollment) :: ctx.subject_base_dn in
  let key = `RSA (Mirage_crypto_pk.Rsa.generate ~bits:ctx.rsa_key_size ()) in
  let key_pem = X509.Private_key.encode_pem key in
  let@/? csr = X509.Signing_request.create dn ~digest:ctx.rsa_key_digest key in
  let csr_pem = X509.Signing_request.encode_pem csr in
  let token = Enrollment.token enrollment in
  (match%lwt Openssl.sign_pem ~token csr_pem with
   | Error error ->
      let enrollment = Enrollment.(update ~state:Failed) enrollment in
      Openssl.log_error error >>= fun () ->
      (Enrollment.save enrollment >>= function
       | Ok () -> Lwt.return_unit
       | Error msg ->
          Log.err (fun p ->
            p "Could not mark enrollment %a as failed: %s"
            Enrollment.pp enrollment msg)) >>= fun () ->
      respond_with_error ~status:`Internal_Server_Error
        "Signing failed, please contact site admin."
   | Ok crt_pem ->
      let enrollment = Enrollment.(update ~state:Acquired) enrollment in
      let@*? () = Enrollment.save enrollment >|= error_msg_of_string in
      (match%lwt
        Openssl.export_pkcs12 ~password ~cert:crt_pem ~certkey:key_pem () with
       | Ok pkcs12 ->
          let headers = ["Content-Type", "application/x-pkcs12"] in
          Dream.respond ~headers pkcs12
       | Error error ->
          let* () = Openssl.log_error error in
          respond_with_error ~status:`Internal_Server_Error
            "Failed to deliver key and certificate, \
             please contact side admin."))

let issued_pkcs12_handler ~ctx =
  with_enrollment @@ fun ~enrollment req ->
  (Dream.form req >>= function
   | `Ok ["password1", password; "password2", password'] ->
      if password <> password' then
        let error = [H.txt "Passwords didn't match."] in
        acquire_handler' ~ctx ~error ~enrollment req
      else
        deliver_certificate ~ctx enrollment password
   | `Expired _ ->
      respond_with_message
        ~status:`Request_Timeout ~title: "Session Expired"
        "The session has expired."
   | _ ->
      respond_with_message
        ~status:`Bad_Request ~title:"Bad Request"
        "Wrong form data.")

let routes ~vpaths () =
  let ctx = create_ctx ~vpaths in
  Dream.scope "" [] [
    Dream.get (vpaths.acquire_login ":token") (acquire_login_handler ~ctx);
    Dream.get vpaths.acquire (acquire_handler ~ctx);
    Dream.post vpaths.issued_pkcs12 (issued_pkcs12_handler ~ctx);
    Dream.get vpaths.cacert_pem (cacert_handler ~ctx);
    Dream.get vpaths.crl_pem (crl_handler ~ctx);
  ]
