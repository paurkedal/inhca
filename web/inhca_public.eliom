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
open Lwt.Infix
open Unprime
open Unprime_char
open Unprime_option
open Unprime_string

let th_p s = F.th [F.pcdata s]

let base_dn =
  List.map (fun s -> Option.get (String.cut_affix "=" s))
    (String.chop_affix "," Inhca_config.subject_base_dn#get)

let main_service =
  let get = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path []) ~meth:(Get get) ())

let keygen_service =
  let get = Eliom_parameter.(suffix (string "requset_id")) in
  Eliom_service.(create ~path:(Path ["req"]) ~meth:(Get get) ())
let%client keygen_service = ~%keygen_service

let signing_service =
  let get = Eliom_parameter.(suffix (string "request_id")) in
  let post = Eliom_parameter.(string "spkac") in
  Eliom_service.(create ~path:(Path ["req"]) ~meth:(Post (get, post)) ())

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

let keygen_handler = with_request @@ fun request_id req () ->
  Lwt.return @@ Inhca_tools.F.page ~title:"Fetch Certificate" [
    F.Form.post_form ~service:signing_service
      (fun spkac -> [
        F.p [
          F.pcdata
            "Using this page, your browser will request a certificate which
             will be immediately installed in your current browser. \
             You only get one certificate, but you can export it and import \
             it into another browser or computer using e.g. SSH or a private \
             USB stick for secure transport.";
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
      ])
      request_id
  ]

let signing_handler = with_request @@ fun request_id req spkac ->
  Eliom_bus.write edit_bus (`remove req) >>
  let spkac = String.filter (not <@ Char.is_space) spkac in
  let spkac_req = ("SPKAC", spkac) :: ("CN", req.request_cn) :: base_dn in
  match%lwt Inhca_openssl.sign_spkac request_id spkac_req with
   | Ok cert ->
      Eliom_registration.File.send
        ~content_type:"application/x-x509-user-cert" cert
   | Error error ->
      Inhca_openssl.log_error error >>
      Inhca_tools.F.send_error ~code:500
        "Signing failed, please contact site admin."

let () =
  let open Eliom_registration in
  let content_type = "text/html" in
  Html.register ~content_type ~service:main_service main_handler;
  Html.register ~content_type ~service:keygen_service keygen_handler;
  Any.register ~service:signing_service signing_handler
