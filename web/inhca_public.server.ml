(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Inhca_data
open Inhca_prereq
open Lwt.Infix

module Log =
  Inhca_tools.Local_log (struct let section_name = "inhca:public" end)

let base_dn_str = Inhca_config.(global.subject_base_dn)

let base_dn = Dn.of_string base_dn_str

let main_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path []) ~meth:(Get get) ())

let token_r =
  Eliom_reference.eref ~secure:true
    ~scope:Eliom_common.default_session_scope None

let token_login_service =
  let get = Eliom_parameter.(suffix (string "token")) in
  Eliom_service.(create ~path:(Path ["acquire"]) ~meth:(Get get) ())

let acquire_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["acquire"; ""]) ~meth:(Get get) ())

let issue_pkcs12_service =
  let get = Eliom_parameter.unit in
  let post = Eliom_parameter.(string "password" ** string "password'") in
  let open Eliom_service in
  create ~path:(Path ["acquire"; "issued-key-and-cert.p12"])
         ~meth:(Post (get, post)) ()

let cacert_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["cacert.pem"]) ~meth:(Get get) ())

let crl_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["crl.pem"]) ~meth:(Get get) ())

let main_handler () () =
  Lwt.return F.(Inhca_tools.F.page ~title:"Inhca" [
    p [txt "Nothing to see here."]
  ])

let with_enrollment f get post =
  let send_simple_page ?code ~title msg =
    Inhca_tools.F.send_page ?code ~title [F.txt msg]
  in
  match%lwt Eliom_reference.get token_r with
   | None ->
      send_simple_page ~code:404 ~title:"Not Found"
        "The URL of this page is incomplete."
   | Some token ->
      let send_expired () =
        send_simple_page ~code:403 ~title:"Link Expired"
          "This request has expired. \
           Please ask for a new link when you expect to be available to use it."
      in
      try%lwt
        let%lwt enrollment_table = enrollment_table in
        let%lwt enr = Ocsipersist.Polymorphic.find enrollment_table token in
        match Enrollment.state enr with
         | Enrollment.Prepared ->
            if Enrollment.has_expired enr then send_expired () else
            let enr = Enrollment.update ~state:Enrollment.Visited enr in
            Eliom_bus.write edit_bus (`Update enr) >>= fun () ->
            f enr get post
         | Enrollment.Visited ->
            if Enrollment.has_expired enr then send_expired () else
            f enr get post
         | Enrollment.Acquired ->
            Inhca_tools.F.send_page ~code:400
                                    ~title:"Certificate Already Delivered" [
              F.txt
                "According to our records, your certificate has already been \
                 delivered. \
                 If something went wrong while acquiring it, you will need to \
                 request a new link. ";
              F.b [F.txt
                "If you did not use the link yourself, please let us know as \
                 soon as possible, "];
              F.txt "so that we can revoke the certificate."
            ]
         | Enrollment.Revoked ->
            send_simple_page ~code:403 ~title:"Enrollment Token Revoked"
              "This enrollment link has been revoked by the administrator. \
               Please ask for a new link if needed."
         | Enrollment.Failed ->
            send_simple_page ~code:500 ~title:"Internal Server Error"
              "Something went wrong when delivering the certificate. \
               Please let us know, so that the administrator can look into \
               what happened and provide a new link."
      with Not_found ->
        Eliom_reference.set token_r None >>= fun () ->
        send_simple_page ~code:403 ~title:"Unknown Link"
          "The link has been deleted or is invalid. \
           Please ask for a new link if needed."

let server_generates_form =
  F.Form.post_form ~service:issue_pkcs12_service ~a:[F.a_autocomplete `Off] @@
  fun (password, password') -> [
    F.p [
      F.txt
        "If generating the secret key in the browser is not supported \
         you can let the server generate it for you and deliver it along \
         with the certificate as a download.  It can typically be imported \
         into the browser from the certificate management settings, or \
         into the keyring of the operating system.";
    ];
    F.p [
      F.txt "Type a password ";
      F.Form.input ~input_type:`Password ~name:password F.Form.string;
      F.txt " and again ";
      F.Form.input ~input_type:`Password ~name:password' F.Form.string;
      F.txt ", to protect the secret key in the download.";
      F.br ();
      F.txt "Then, ";
      F.Form.input ~input_type:`Submit
        ~value:"download the key and certificate" F.Form.string;
      F.txt ".";
    ]
  ]

let token_login_handler token () =
  Eliom_reference.set token_r (Some token) >>= fun () ->
  Lwt.return (Eliom_registration.Redirection acquire_service)

let acquire_handler ?error =
  with_enrollment @@ fun enrollment () () ->
  let content = [server_generates_form ()] in
  Inhca_tools.F.send_page ~title:"Acquire Certificate"
    (match error with
     | Some error -> F.div ~a:[F.a_class ["error"]] error :: content
     | None -> content)

let issue_pkcs12_handler =
  with_enrollment @@ fun enr () (password, password') ->
  if password <> password' then
    let error = [F.txt "Passwords didn't match."] in
    acquire_handler ~error () () else
  let key_size = 4096 in
  let digest = `SHA512 in
  let dn = Dn.cn (Enrollment.cn enr) :: base_dn in
  let key = `RSA (Mirage_crypto_pk.Rsa.generate ~bits:key_size ()) in
  (match X509.Signing_request.create dn ~digest key with
   | Error (`Msg msg) ->
      Log.error_f "Failed to create CSR: %s" msg >>= fun () ->
      Inhca_tools.F.send_error_page ~code:500
        "Failed to create certificate signing request, \
         please contact site admin."
   | Ok csr ->
      let key_pem = X509.Private_key.encode_pem key in
      let csr_pem = X509.Signing_request.encode_pem csr in
      (match%lwt
        Inhca_openssl.sign_pem ~token:(Enrollment.token enr) csr_pem
       with
       | Error error ->
          let enr = Enrollment.update ~state:Enrollment.Failed enr in
          Eliom_bus.write edit_bus (`Update enr) >>= fun () ->
          Inhca_openssl.log_error error >>= fun () ->
          Inhca_tools.F.send_error_page ~code:500
            "Signing failed, please contact site admin."
       | Ok crt_pem ->
          let enr = Enrollment.update ~state:Enrollment.Acquired enr in
          Eliom_bus.write edit_bus (`Update enr) >>= fun () ->
          (match%lwt
            Inhca_openssl.export_pkcs12
              ~password ~cert:crt_pem ~certkey:key_pem ()
           with
           | Ok pkcs12 ->
              Eliom_registration.String.send
                  ~content_type:"application/x-pkcs12"
                  (pkcs12, "application/x-pkcs12")
                >|= Eliom_registration.cast_unknown_content_kind
           | Error error ->
              Inhca_openssl.log_error error >>= fun () ->
              Inhca_tools.F.send_error_page ~code:500
                "Failed to deliver key and certificate, \
                 please contact side admin.")))

let cacert_handler () () = Lwt.return Inhca_openssl.cacert_path

let crl_handler () () =
  (match%lwt Inhca_openssl.gencrl () with
   | Error error ->
      Inhca_tools.F.send_page ~code:500 ~title:"Internal Server Error"
        [F.txt "Could not generate CRL."]
   | Ok crl ->
      Eliom_registration.String.send (crl, "application/x-pem-file")
        >|= Eliom_registration.cast_unknown_content_kind)

let () =
  let open Eliom_registration in
  let content_type = "text/html" in
  Html.register ~content_type ~service:main_service main_handler;
  Redirection.register ~service:token_login_service token_login_handler;
  Any.register ~content_type ~service:acquire_service acquire_handler;
  Any.register ~service:issue_pkcs12_service issue_pkcs12_handler;
  File.register ~service:cacert_service cacert_handler;
  Any.register ~service:crl_service crl_handler
