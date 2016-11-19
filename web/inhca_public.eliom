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

let acquire_service =
  let get = Eliom_parameter.(suffix (string "request_id")) in
  Eliom_service.(create ~path:(Path ["acquire"]) ~meth:(Get get) ())
let%client acquire_service = ~%acquire_service

let issue_spkac_service =
  let get = Eliom_parameter.(string "request_id") in
  let post = Eliom_parameter.(string "spkac") in
  let open Eliom_service in
  create ~path:(Path ["acquire"; "issued-cert.der"])
         ~meth:(Post (get, post)) ()

let issue_pkcs12_service =
  let get = Eliom_parameter.(string "request_id") in
  let post = Eliom_parameter.(string "password" ** string "password'") in
  let open Eliom_service in
  create ~path:(Path ["acquire"; "issued-key-and-cert.p12"])
         ~meth:(Post (get, post)) ()

let main_handler () () =
  Lwt.return F.(Inhca_tools.F.page ~title:"Inhca" [
    p [pcdata "Nothing to see here."]
  ])

let with_request f request_id post =
  try%lwt
    let%lwt request = Ocsipersist.find request_table request_id in
    f request_id request post
  with Not_found ->
    Inhca_tools.http_error 404 "No such certificate request."

let keygen_form req =
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
      F.tr [th_p "Full name:"; F.td [F.pcdata req.request_cn]];
      F.tr [th_p "Email:"; F.td [F.pcdata req.request_email]];
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

let acquire_handler ?(error = []) = with_request @@ fun request_id request () ->
  Lwt.return @@ Inhca_tools.F.page ~title:"Acquire Certificate" [
    F.div ~a:[F.a_class ["error"]] error;
    keygen_form request request_id;
    server_generates_form request_id;
  ]

let issue_spkac_handler = with_request @@ fun request_id req spkac ->
  let spkac = String.filter (not <@ Char.is_space) spkac in
  if spkac = "" then
    let error = [F.pcdata
      "Your web browser did not supply a certificate request. \
       This probably means that it does not support <keygen/>. \
       You may try the alternative method."
    ] in
    Eliom_registration.Html.send =<< acquire_handler ~error request_id ()
  else begin
    Eliom_bus.write edit_bus (`remove req) >>
    let spkac_req = ("SPKAC", spkac) :: ("CN", req.request_cn) :: base_dn_tup in
    match%lwt Inhca_openssl.sign_spkac request_id spkac_req with
     | Ok cert ->
        Eliom_registration.File.send
          ~content_type:"application/x-x509-user-cert" cert
     | Error error ->
        Inhca_openssl.log_error error >>
        Inhca_tools.F.send_error ~code:500
          "Signing failed, please contact site admin."
  end

let issue_pkcs12_handler =
  with_request @@ fun request_id req (password, password') ->
  Nocrypto_entropy_lwt.initialize () >>
  if password <> password' then
    let error = [F.pcdata "Passwords didn't match."] in
    Eliom_registration.Html.send =<< acquire_handler ~error request_id () else
  Eliom_bus.write edit_bus (`remove req) >>
  let key_size = 4096 in
  let digest = `SHA512 in
  let dn = `CN req.request_cn :: base_dn in
  let key = `RSA (Nocrypto.Rsa.generate key_size) in
  let csr = X509.CA.request dn ~digest key in
  let key_pem =
    X509.Encoding.Pem.Private_key.to_pem_cstruct1 key in
  let csr_pem =
    X509.Encoding.Pem.Certificate_signing_request.to_pem_cstruct1 csr in
  match%lwt Inhca_openssl.sign_pem ~request_id (Cstruct.to_string csr_pem) with
   | Error error ->
      Inhca_openssl.log_error error >>
      Inhca_tools.F.send_error ~code:500
        "Signing failed, please contact site admin."
   | Ok crt_pem ->
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
  Html.register ~content_type ~service:acquire_service acquire_handler;
  Any.register ~service:issue_pkcs12_service issue_pkcs12_handler;
  Any.register ~service:issue_spkac_service issue_spkac_handler
