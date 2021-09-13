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
open Opium
module H = Tyxml_html

open Admin_core

let authorize_admin handler req =
  (match Config.(global.authn_http_header) with
   | None ->
      if Config.(global.authz_admins) = [] then
        handler req
      else
        Lwt.return @@ respond_with_error ~status:`Internal_server_error
          "Authorization is not correctly configured."
   | Some h ->
      (match Router.param req h with
       | exception Not_found ->
          Lwt.return @@ respond_with_error ~status:`Unauthorized
            "Missing authentication header."
       | user ->
          if List.mem user Config.(global.authz_admins) then
            let* () = Log.info (fun f -> f "Authorized %s." user) in
            handler req
          else
            Lwt.return @@ respond_with_error ~status:`Forbidden
              "Admin access required."))

let respond_with_openssl_result = function
 | Ok () ->
    Lwt.return @@ respond_with_json_ok `Null
 | Error error ->
    Openssl.log_error error >>= fun () ->
    Lwt.return @@
      respond_with_json_error "openssl command failed, see log for details."

let revoke_serial =
  authorize_admin @@ fun req ->
  (match int_of_string (Router.param req "serial") with
   | exception Not_found | exception Failure _ ->
      Lwt.return @@
        respond_with_error ~status:`Bad_request
          "Missing or invalid serial parameter."
   | serial ->
      Openssl.revoke_serial serial >>= respond_with_openssl_result)

let idl_server =
  let ( ~@ ) = Rpc_lwt.T.put in
  let module R = Admin_rpc.Make (Rpc_lwt.GenServer ()) in
  let rewrite_openssl_error =
    Fun.flip Lwt_result.bind_error @@ fun err ->
      Openssl.log_error err >|= fun () ->
      let msg = "The openssl command failed, see server log for details." in
      Error (Idl.DefaultError.InternalError msg)
  in
  R.list_enrollments (fun () -> ~@(
    Enrollment.all ()
  ));
  R.add_enrollment (fun cn email -> ~@(
    let enrollment = Enrollment.create ~cn ~email () in
    Enrollment.save enrollment
  ));
  R.delete_enrollment (fun enr -> ~@(
    Enrollment.delete enr
  ));
  R.updatedb (fun () -> ~@(
    Openssl.updatedb () |> rewrite_openssl_error
  ));
  R.revoke_serial (fun serial -> ~@(
    Openssl.revoke_serial serial |> rewrite_openssl_error
  ));
  Rpc_lwt.server R.implementation

let admin_api_handler =
  authorize_admin @@ fun req ->
  let* call = Jsonrpc.call_of_string =|< Request.to_plain_text req in
  let+ resp = idl_server call in
  Response.of_plain_text (Jsonrpc.string_of_response resp)

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

  Lwt.return @@
    respond_with_page ~status:`OK ~title:"Pending Certificate Requests" [
      H.h2 [H.txt "Pending Requests"];
      enr_table;
      H.h2 [H.txt "Issued Certificates"];
      H.table ~a:[H.a_class ["std"]] (issue_header_tr :: issue_trs);
    ]

let add_routes ~(vpaths : Vpaths.t) app =
  app
    |> App.get vpaths.admin admin_handler
    |> App.post vpaths.admin_api admin_api_handler
