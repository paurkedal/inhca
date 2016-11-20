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
open Unprime
open Unprime_char
open Unprime_option
open Unprime_string

let th_p s = F.th [F.pcdata s]

let base_dn_str = Inhca_config.subject_base_dn#get

let base_dn_tup =
  List.map (fun s -> Option.get (String.cut_affix "=" s))
    (String.chop_affix "," base_dn_str)

let base_dn = dn_of_string base_dn_str

let main_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path []) ~meth:(Get get) ())

let token_r =
  Eliom_reference.eref ~secure:true
    ~scope:Eliom_common.default_session_scope None

let token_login_service =
  let get = Eliom_parameter.(suffix (string "token")) in
  Eliom_service.(create ~path:(Path ["acquire"]) ~meth:(Get get) ())
let%client token_login_service = ~%token_login_service

let acquire_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["acquire"; ""]) ~meth:(Get get) ())

let issue_spkac_service =
  let get = Eliom_parameter.unit in
  let post = Eliom_parameter.(string "spkac") in
  let open Eliom_service in
  create ~path:(Path ["acquire"; "issued-cert.der"])
         ~meth:(Post (get, post)) ()

let issue_pkcs12_service =
  let get = Eliom_parameter.unit in
  let post = Eliom_parameter.(string "password" ** string "password'") in
  let open Eliom_service in
  create ~path:(Path ["acquire"; "issued-key-and-cert.p12"])
         ~meth:(Post (get, post)) ()

let main_handler () () =
  Lwt.return F.(Inhca_tools.F.page ~title:"Inhca" [
    p [pcdata "Nothing to see here."]
  ])

let with_enrollment f get post =
  match%lwt Eliom_reference.get token_r with
   | None ->
      Inhca_tools.http_error 403 "No authorization token provided."
   | Some token ->
      let check_expiration enr =
        if Enrollment.has_expired enr then
          Inhca_tools.http_error 403 "Your enrollment token has expired."
        else
          Lwt.return_unit in
      try%lwt
        let%lwt enr = Ocsipersist.find enrollment_table token in
        match Enrollment.state enr with
         | Enrollment.Prepared ->
            check_expiration enr >>
            let enr = Enrollment.update ~state:Enrollment.Visited enr in
            Eliom_bus.write edit_bus (`Update enr) >>
            f enr get post
         | Enrollment.Visited ->
            check_expiration enr >>
            f enr get post
         | Enrollment.Acquired ->
            Inhca_tools.http_error 400
              "Your certificate has already been delivered. \
               If something went wrong, you will need to request a new link."
         | Enrollment.Revoked ->
            Inhca_tools.http_error 403
              "The enrollment token has been revoked."
         | Enrollment.Failed ->
            Inhca_tools.http_error 500
              "Something went wrong when delivering the certifiacte."
      with Not_found ->
        Eliom_reference.set token_r None >>
        Inhca_tools.http_error 403
          "The authorization token has been deleted or is invalid."

let keygen_form enr =
  F.Form.post_form ~service:issue_spkac_service @@
  fun spkac -> [
    F.h2 [F.pcdata "Generate Key and Certificate Request in Browser"];
    F.p [
      F.pcdata
        "Using this page, your browser will request a certificate which \
         will be immediately installed in your current browser, but you \
         can export and import it into another browser if needed. ";
      F.pcdata
        "This method is not supported by recent versions of Chrome and \
         Chromium or any version of Internet Explorer, \
         but should still work for Firefox and Safari.";
    ];
    F.table ~a:[F.a_class ["assoc"]] [
      F.tr [th_p "Full name:"; F.td [F.pcdata (Enrollment.cn enr)]];
      F.tr [th_p "Email:"; F.td [F.pcdata (Enrollment.email enr)]];
      F.tr [
        th_p "Key strength:";
        F.td [
          F.keygen
            ~a:[F.a_name (Eliom_parameter.string_of_param_name spkac)] ()
        ]
      ];
      F.tr ~a:[F.a_class ["submit"]] [
        F.td [];
        F.td [F.Form.input ~input_type:`Submit
                           ~value:"Install certificate" F.Form.string]
      ]
    ]
  ]

let server_generates_form =
  F.Form.post_form ~service:issue_pkcs12_service ~a:[F.a_autocomplete false] @@
  fun (password, password') -> [
    F.h2 [F.pcdata "Let the Server Generate the Key and Request"];
    F.p [
      F.pcdata
        "If generating the secret key in the browser is not supported \
         you can let the server generate it for you and deliver it along \
         with the certificate as a download.  It can typically be imported \
         into the browser from the certificate management settings, or \
         into the keyring of the operating system.";
    ];
    F.p [
      F.pcdata "Type a password ";
      F.Form.input ~input_type:`Password ~name:password F.Form.string;
      F.pcdata " and again ";
      F.Form.input ~input_type:`Password ~name:password' F.Form.string;
      F.pcdata ", to protect the secret key in the download.";
      F.br ();
      F.pcdata "Then, ";
      F.Form.input ~input_type:`Submit
        ~value:"download the key and certificate" F.Form.string;
      F.pcdata ".";
    ]
  ]

let token_login_handler token () =
  Eliom_reference.set token_r (Some token) >>
  Lwt.return (Eliom_registration.Redirection acquire_service)

let acquire_handler ?error =
  with_enrollment @@ fun enrollment () () ->
  Lwt.return @@ Inhca_tools.F.page ~title:"Acquire Certificate" [
    (match error with
     | Some error -> F.div ~a:[F.a_class ["error"]] error
     | None -> F.pcdata "");
    keygen_form enrollment ();
    server_generates_form ();
  ]

let issue_spkac_handler = with_enrollment @@ fun enr () spkac ->
  let spkac = String.filter (not <@ Char.is_space) spkac in
  if spkac = "" then
    let error = [F.pcdata
      "Your web browser did not supply a certificate request. \
       This probably means that it does not support <keygen/>. \
       You may try the alternative method."
    ] in
    Eliom_registration.Html.send =<< acquire_handler ~error () ()
  else begin
    let spkac_req =
      ("SPKAC", spkac) :: ("CN", Enrollment.cn enr) :: base_dn_tup in
    match%lwt
      Inhca_openssl.sign_spkac ~token:(Enrollment.token enr) spkac_req
    with
     | Ok cert ->
        let enr = Enrollment.update ~state:Enrollment.Acquired enr in
        Eliom_bus.write edit_bus (`Update enr) >>
        Eliom_registration.File.send
          ~content_type:"application/x-x509-user-cert" cert
     | Error error ->
        let enr = Enrollment.update ~state:Enrollment.Failed enr in
        Eliom_bus.write edit_bus (`Update enr) >>
        Inhca_openssl.log_error error >>
        Inhca_tools.F.send_error ~code:500
          "Signing failed, please contact site admin."
  end

let issue_pkcs12_handler =
  with_enrollment @@ fun enr () (password, password') ->
  Nocrypto_entropy_lwt.initialize () >>
  if password <> password' then
    let error = [F.pcdata "Passwords didn't match."] in
    Eliom_registration.Html.send =<< acquire_handler ~error () () else
  let key_size = 4096 in
  let digest = `SHA512 in
  let dn = `CN (Enrollment.cn enr) :: base_dn in
  let key = `RSA (Nocrypto.Rsa.generate key_size) in
  let csr = X509.CA.request dn ~digest key in
  let key_pem =
    X509.Encoding.Pem.Private_key.to_pem_cstruct1 key in
  let csr_pem =
    X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
  match%lwt
    Inhca_openssl.sign_pem ~token:(Enrollment.token enr)
                           (Cstruct.to_string csr_pem)
  with
   | Error error ->
      let enr = Enrollment.update ~state:Enrollment.Failed enr in
      Eliom_bus.write edit_bus (`Update enr) >>
      Inhca_openssl.log_error error >>
      Inhca_tools.F.send_error ~code:500
        "Signing failed, please contact site admin."
   | Ok crt_pem ->
      let enr = Enrollment.update ~state:Enrollment.Acquired enr in
      Eliom_bus.write edit_bus (`Update enr) >>
      match%lwt
        Inhca_openssl.export_pkcs12 ~password ~cert:crt_pem
                                    ~certkey:(Cstruct.to_string key_pem) ()
      with
       | Ok pkcs12 ->
          Eliom_registration.String.send
              ~content_type:"application/x-pkcs12"
              (pkcs12, "application/x-pkcs12")
            >|= Eliom_registration.cast_unknown_content_kind
       | Error error ->
          Inhca_openssl.log_error error >>
          Inhca_tools.F.send_error ~code:500
            "Failed to deliver key and certificate, please contact side admin."

let () =
  let open Eliom_registration in
  let content_type = "text/html" in
  Html.register ~content_type ~service:main_service main_handler;
  Redirection.register ~service:token_login_service token_login_handler;
  Html.register ~content_type ~service:acquire_service acquire_handler;
  Any.register ~service:issue_pkcs12_service issue_pkcs12_handler;
  Any.register ~service:issue_spkac_service issue_spkac_handler
