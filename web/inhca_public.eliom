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

let cacert_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["cacert.pem"]) ~meth:(Get get) ())

let crl_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path ["crl.pem"]) ~meth:(Get get) ())

let main_handler () () =
  Lwt.return F.(Inhca_tools.F.page ~title:"Inhca" [
    p [pcdata "Nothing to see here."]
  ])

let with_enrollment f get post =
  let send_simple_page ?code ~title msg =
    Inhca_tools.F.send_page ?code ~title [F.pcdata msg]
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
        let%lwt enr = Ocsipersist.find enrollment_table token in
        match Enrollment.state enr with
         | Enrollment.Prepared ->
            if Enrollment.has_expired enr then send_expired () else
            let enr = Enrollment.update ~state:Enrollment.Visited enr in
            Eliom_bus.write edit_bus (`Update enr) >>
            f enr get post
         | Enrollment.Visited ->
            if Enrollment.has_expired enr then send_expired () else
            f enr get post
         | Enrollment.Acquired ->
            Inhca_tools.F.send_page ~code:400
                                    ~title:"Certificate Already Delivered" [
              F.pcdata
                "According to our records, your certificate has already been \
                 delivered. \
                 If something went wrong while acquiring it, you will need to \
                 request a new link. ";
              F.b [F.pcdata
                "If you did not use the link yourself, please let us know as \
                 soon as possible, "];
              F.pcdata "so that we can revoke the certificate."
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
        Eliom_reference.set token_r None >>
        send_simple_page ~code:403 ~title:"Unknown Link"
          "The link has been deleted or is invalid. \
           Please ask for a new link if needed."

let keygen_form enr =
  F.Form.post_form ~service:issue_spkac_service @@
  fun spkac -> [
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
  let content =
    if Inhca_config.enable_keygen_cp#get then [
      F.h2 [F.pcdata "Generate Key and Certificate Request in Browser"];
      keygen_form enrollment ();
      F.h2 [F.pcdata "Let the Server Generate the Key and Request"];
      server_generates_form ();
    ] else [
      server_generates_form ();
    ]
  in
  Inhca_tools.F.send_page ~title:"Acquire Certificate"
    (match error with
     | Some error -> F.div ~a:[F.a_class ["error"]] error :: content
     | None -> content)

let issue_spkac_handler = with_enrollment @@ fun enr () spkac ->
  let spkac = String.filter (not <@ Char.is_space) spkac in
  if spkac = "" then
    let error = [F.pcdata
      "Your web browser did not supply a certificate request. \
       This probably means that it does not support <keygen/>. \
       You may try the alternative method."
    ] in
    acquire_handler ~error () ()
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
    acquire_handler ~error () () else
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

let cacert_handler () () = Lwt.return Inhca_openssl.cacert_path

let crl_handler () () =
  (match%lwt Inhca_openssl.gencrl () with
   | Error error ->
      Inhca_tools.F.send_page ~code:500 ~title:"Internal Server Error"
        [F.pcdata "Could not generate CRL."]
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
  Any.register ~service:issue_spkac_service issue_spkac_handler;
  File.register ~service:cacert_service cacert_handler;
  Any.register ~service:crl_service crl_handler
