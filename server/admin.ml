(* Copyright (C) 2014--2025  Petter A. Urkedal <paurkedal@gmail.com>
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
open Printf
open Unprime_list
module H = Tyxml_html

open Admin_core

let pp_jwt =
  Fmt.(using (fun jwt -> Yojson.Safe.to_string jwt.Jose.Jwt.payload) string)

let authorize_admin handler req =
  let unauthorized msg = respond_with_error ~status:`Unauthorized msg in
  let check_user user =
    if List.mem user Config.(global.authz_admins) then
      let* () = Log.info (fun f -> f "Authorized %s." user) in
      handler req
    else
      respond_with_error ~status:`Forbidden "Admin access required."
  in
  (match Config.(global.authn_bearer_jwk, global.authn_http_header) with
   | None, None ->
      if Config.(global.authz_admins) = [] then
        handler req
      else
        respond_with_error ~status:`Internal_Server_Error
          "Authorization is not correctly configured."
   | Some jwk, _ ->
      (match Dream.header req "Authorization" with
       | None -> unauthorized "Missing authorization header."
       | Some data ->
          (match String.split_on_char ' ' data |> List.filter ((<>) "") with
           | ["Bearer"; token] ->
              let now = Ptime_clock.now () in
              (match Jose.Jwt.of_string ~jwk ~now token with
               | Ok jwt ->
                  Log.debug (fun f -> f "JWT: %a" pp_jwt jwt) >>= fun () ->
                  (match Jose.Jwt.get_string_claim jwt "sub" with
                   | None -> unauthorized "JWT is missing the sub claim"
                   | Some user -> check_user user)
               | Error `Expired ->
                  unauthorized "The bearer token has expired."
               | Error `Invalid_signature ->
                  unauthorized "The signature of the bearer token is invalid."
               | Error (`Msg msg) ->
                  unauthorized ("Bad bearer token: " ^ msg)
               | Error `Not_json ->
                  unauthorized "Bearer token data is not JSON."
               | Error `Not_supported ->
                  unauthorized "Bearer token format unsupported.")
           | _ ->
              unauthorized "Authorization header is not a bearer token."))
   | None, Some h ->
      (match Dream.param req h with
       | exception Not_found -> unauthorized "Missing authentication header."
       | user -> check_user user))

module Down_init = Admin_rpc.Make_down (Rpc_lwt.GenClient ())
module Down = struct
  let sink = Lwt_condition.create ()

  include Admin_rpc.Make_down (Idl.Exn.GenClient (struct
    let rpc call =
      Lwt_condition.broadcast sink (Jsonrpc.string_of_call call);
      Rpc.success Null
  end))
end

let idl_server =
  let ( ~@ ) = Rpc_lwt.T.put in
  let module Up = Admin_rpc.Make_up (Rpc_lwt.GenServer ()) in
  let rewrite_openssl_error =
    Fun.flip Lwt_result.bind_error @@ fun err ->
      Openssl.log_error err >|= fun () ->
      let msg = "The openssl command failed, see server log for details." in
      Error (Idl.DefaultError.InternalError msg)
  in
  Up.list_enrollments (fun () -> ~@(
    Enrollment.all ()
  ));
  Up.add_enrollment (fun cn email -> ~@(
    let enrollment = Enrollment.create ~cn ~email () in
    let+? () = Enrollment.save enrollment in
    Down.enrollment_added enrollment
  ));
  Up.delete_enrollment (fun enrollment -> ~@(
    let+? () = Enrollment.delete enrollment in
    Down.enrollment_deleted enrollment
  ));
  Up.updatedb (fun () -> ~@(
    Openssl.updatedb () |> rewrite_openssl_error
  ));
  Up.revoke_serial (fun serial -> ~@(
    Openssl.revoke_serial serial |> rewrite_openssl_error
  ));
  Rpc_lwt.server Up.implementation

let admin_rpc_up_handler =
  authorize_admin @@ fun req ->
  let* call = Jsonrpc.call_of_string =|< Dream.body req in
  let* resp = idl_server call in
  Dream.json (Jsonrpc.string_of_response resp)

let admin_rpc_down_handler =
  authorize_admin @@ fun _req ->
  let headers = ["Content-Type", "text/event-stream"] in
  Dream.stream ~headers @@ fun stream ->
  let send_string call_str =
    let* () = Dream.write stream "data: " in
    let* () = Dream.write stream call_str in
    let* () = Dream.write stream "\n\n" in
    Dream.flush stream
  in
  let send_rpc call =
    let+ () = send_string (Jsonrpc.string_of_call call) in
    Rpc.success Null
  in
  let rec monitor () =
    Lwt_condition.wait Down.sink >>= send_string >>= monitor
  in
  (Enrollment.all () >>= function
   | Ok enrollments ->
      let send_down enrollment =
        let* resp =
          Rpc_lwt.T.get (Down_init.enrollment_added send_rpc enrollment)
        in
        (match resp with
         | Ok () ->
            Lwt.return_unit
         | Error (Idl.DefaultError.InternalError msg) ->
            Log.err (fun f -> f "Failed to send initial enrollment: %s" msg))
      in
      Lwt_list.iter_s send_down enrollments >>= monitor
   | Error _error ->
      failwith "Failed to gather initial enrollments to send.")

let admin_handler =
  authorize_admin @@ fun _req ->

  let cn_input : Html_types.input H.elt =
    H.input ~a:[H.a_id "inhca-enrollment-cn"; H.a_input_type `Text] () in
  let email_input : Html_types.input H.elt =
    H.input ~a:[H.a_id "inhca-enrollment-email"; H.a_input_type `Text] () in
  let add_button =
    H.button
      ~a:[H.a_id "inhca-enrollment-add"; H.a_button_type `Button]
      [H.txt "add"]
  in
  let enr_table =
    H.table ~a:[H.a_id "inhca-enrollment-table"; H.a_class ["std"]] [
      H.tr [
        H.th [H.txt "Id"];
        H.th [H.txt "State"];
        H.th [H.txt "Expiration"];
        H.th [H.txt "CN"];
        H.th [H.txt "Email"];
      ];
      H.tr [
        H.td [];
        H.td [];
        H.td [];
        H.td [cn_input];
        H.td [email_input];
        H.td [add_button];
      ]
    ]
  in

  let updatedb_button =
    H.button
      ~a:[H.a_id "inhca-updatedb"; H.a_button_type `Button]
      [H.txt "update"]
  in

  let issue_header_tr =
    H.tr [
      H.th [H.txt "SN"];
      H.th [H.txt "State"];
      H.th [H.txt "Expired"];
      H.th [H.txt "Revoked"];
      H.th [H.txt "DN"];
      H.td [updatedb_button];
    ]
  in

  let issue_tr issue =
    let open Openssl in

    let serial = Issue.serial issue in
    let state = Issue.state issue in

    let control =
      (match state with
       | `Revoked -> []
       | `Expired | `Valid ->
          [ H.button
              ~a:[
                H.a_class ["inhca-revoke-serial"];
                H.a_user_data "serial" (string_of_int serial);
                H.a_button_type `Button;
              ]
              [H.txt "revoke"]
          ])
    in

    H.tr [
      H.td [H.txt (sprintf "%02x" serial)];
      H.td [H.txt (Issue.string_of_state state)];
      H.td [H.txt (string_of_ptime (Issue.expired issue))];
      H.td
        (match Issue.revoked issue with
         | None -> []
         | Some d -> [H.txt (string_of_ptime d)]);
      H.td [H.txt (Issue.dn issue)];
      H.td control;
    ]
  in

  let%lwt issue_trs = Lwt_stream.to_list @@
    Lwt_stream.map issue_tr (Openssl.Issue.load_all ()) in

  respond_with_page ~status:`OK ~title:"Pending Certificate Requests" [
    H.h2 [H.txt "Pending Requests"];
    enr_table;
    H.h2 [H.txt "Issued Certificates"];
    H.table ~a:[H.a_class ["std"]] (issue_header_tr :: issue_trs);
  ]

let routes ~(vpaths : Vpaths.t) () =
  Dream.scope "" [] [
    Dream.get vpaths.admin admin_handler;
    Dream.post vpaths.admin_rpc_up admin_rpc_up_handler;
    Dream.get vpaths.admin_rpc_down admin_rpc_down_handler;
  ]
