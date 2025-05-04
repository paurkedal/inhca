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

open Lwt.Infix
open Lwt.Syntax
open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Unprime_list
open Admin_core

module Log = (val Logs.src_log (Logs.Src.create "inhca.admin"))

module H = Tyxml_js.Html

module Enrollment = Enrollment_core

module Up = Admin_rpc.Make_up (Rpc_lwt.GenClient ())

let report_error msg =
  Dom_html.window##alert (Js.string msg); (* FIXME *)
  Lwt.return_unit

let report_result_box box =
  Rpc_lwt.T.get box >>= function
   | Ok () -> Lwt.return_unit
   | Error (Idl.DefaultError.InternalError msg) -> report_error msg

let unbox box =
  Rpc_lwt.T.get box >>= function
   | Ok x -> Lwt.return x
   | Error (Idl.DefaultError.InternalError msg) -> Lwt.fail_with msg

module ElementById = struct

  let get_element id coerce =
    (match Dom_html.getElementById_coerce id coerce with
     | Some elem -> elem
     | None -> failwith ("Cannot find the input element with id " ^ id ^ "."))

  let table id = get_element id Dom_html.CoerceTo.table
  let input id = get_element id Dom_html.CoerceTo.input
  let button id = get_element id Dom_html.CoerceTo.button
end

let tds_of_enrollment enrollment_link delete_handler enr =
  let expiration_class =
    if Enrollment.has_expired enr then "invalid" else "valid"
  in
  [
    H.td [enrollment_link enr];
    H.td [H.txt Enrollment.(string_of_state (state enr))];
    H.td ~a:[H.a_class [expiration_class]] [
      H.txt (string_of_ptime (Enrollment.expiration_time enr))
    ];
    H.td [H.txt Enrollment.(cn enr)];
    H.td [H.txt Enrollment.(email enr)];
    H.td [
      H.button ~a:[H.a_button_type `Button; H.a_onclick (delete_handler enr)] [
        H.txt "delete"
      ]
    ];
  ]

let tr_of_enrollment enrollment_link delete_handler enr =
  H.tr (tds_of_enrollment enrollment_link delete_handler enr)

module Enrollment_set = Prime_enumset.Make (Enrollment)

let site_prefix =
  let loc = Js.to_string (Dom_html.window##.location##.pathname) in
  let rec strip_loop = function
   | "admin" :: pfx -> String.concat "/" (List.rev pfx)
   | _ :: pfx -> strip_loop pfx
   | [] -> Log.err (fun f -> f "Cannot determine site prefix"); ""
  in
  strip_loop (List.rev (String.split_on_char '/' loc))

let vpaths = Vpaths.create site_prefix

let rpc call =
  let+ {code; content; _} =
    XmlHttpRequest.perform_raw_url
      ~content_type:"application/json"
      ~contents:(`String (Jsonrpc.string_of_call call))
      vpaths.Vpaths.admin_rpc_up
  in
  if code <> 200 then
    let msg = Printf.sprintf "HTTP request failed with status %d." code in
    Rpc.failure (Rpc.String msg)
  else
    Jsonrpc.response_of_string content

let admin_handler_client _ev =
  let enr_table_dom = ElementById.table "inhca-enrollment-table" in
  let static_row_count = 2 in

  let add_button = ElementById.button "inhca-enrollment-add" in
  add_button##.onclick := Dom.handler begin fun _ev ->
    let cn_input = ElementById.input "inhca-enrollment-cn" in
    let email_input = ElementById.input "inhca-enrollment-email" in
    let cn = Js.to_string cn_input##.value in
    let email = Js.to_string email_input##.value in
    if cn <> "" then
      begin
        cn_input##.value := Js.string "";
        email_input##.value := Js.string "";
        Lwt.async (fun () -> Up.add_enrollment rpc cn email |> report_result_box)
      end;
    Js._false
  end;

  let updatedb_button = ElementById.button "inhca-updatedb" in
  updatedb_button##.onclick := Dom.handler begin fun _ev ->
    Lwt.async begin fun () ->
      Up.updatedb rpc () |> report_result_box >|= fun () ->
      Dom_html.window##.location##reload
    end;
    Js._false
  end;

  let instrument_revoke_serial_button button =
    let serial =
      button##getAttribute (Js.string "data-serial")
        |> Fun.flip Js.Opt.get
            (fun () -> failwith "Missing data on revoke button.")
        |> Js.to_string
        |> int_of_string
    in
    button##.onclick := Dom.handler begin fun _ev ->
      Lwt.async begin fun () ->
        Up.revoke_serial rpc serial |> report_result_box >|= fun () ->
        Dom_html.window##.location##reload
      end;
      Js._false
    end
  in
  Dom_html.document##getElementsByClassName (Js.string "inhca-revoke-serial")
    |> Dom.list_of_nodeList
    |> List.iter instrument_revoke_serial_button;

  let delete_handler enr (ev : Dom_html.mouseEvent Js.t) =
    (Js.Unsafe.coerce (Dom.eventTarget ev)
      :> Dom_html.inputElement Js.t)##.disabled := Js._true;
    Lwt.async (fun () -> Up.delete_enrollment rpc enr |> report_result_box);
    false
  in

  let enrollment_link enr =
    let token = Enrollment.token enr in
    let href = vpaths.Vpaths.acquire_login token in
    H.a ~a:H.[a_href href; a_class ["inhca-base64-token"]] [H.txt token]
  in

  let enr_set = ref Enrollment_set.empty in

  let remove_enrollment enr =
    (match Enrollment_set.locate enr !enr_set with
     | false, _ ->
        Log.err (fun f -> f "Can't find the request to delete.")
     | true, i ->
        enr_set := Enrollment_set.remove enr !enr_set;
        enr_table_dom##deleteRow (static_row_count + i))
  in

  let add_enrollment enr =
    let is_present, pos = Enrollment_set.locate enr !enr_set in
    if is_present then
      enr_table_dom##deleteRow (static_row_count + pos)
    else
      enr_set := Enrollment_set.add enr !enr_set;
    let row = enr_table_dom##insertRow (static_row_count + pos) in
    tds_of_enrollment enrollment_link delete_handler enr
      |> List.iter (fun c -> Dom.appendChild row (Tyxml_js.To_dom.of_td c))
  in

  let () =
    let idl_server =
      let module Down = Admin_rpc.Make_down (Idl.Exn.GenServer ()) in
      Down.enrollment_added add_enrollment;
      Down.enrollment_deleted remove_enrollment;
      Idl.Exn.server Down.implementation
    in
    let event_source =
      new%js EventSource.eventSource (Js.string vpaths.Vpaths.admin_rpc_down)
    in
    event_source##.onerror := Dom.handler begin fun event ->
      Console.console##error ("Failed to receive event:", event);
      Js._true
    end;
    event_source##.onmessage := Dom.handler begin fun event ->
      let _, _, call =
        Jsonrpc.version_id_and_call_of_string_option (Js.to_string event##.data)
      in
      let _resp : Rpc.response = idl_server call in
      Js._true
    end
  in

  Lwt.return_unit

let () = Lwt.async (fun () -> Lwt_js_events.onload () >>= admin_handler_client)
