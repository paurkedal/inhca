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

[%%shared
  open Eliom_content.Html
  open Lwt.Infix
  open Inhca_admin_services
  open Inhca_data
  open Inhca_prereq
  open Printf
  open Unprime_list
  open Unprime_option
]

let reword_openssl_error = function
 | Ok () ->
    Lwt.return (Ok ())
 | Error error ->
    Inhca_openssl.log_error error >>
    Lwt.return (Error "openssl command failed, see log for details.")

let revoke_serial_sf serial =
  Inhca_tools.authorize_admin () >>
  Inhca_openssl.revoke_serial serial >>= reword_openssl_error

let%client revoke_serial =
  ~%(Eliom_client.server_function [%json: int] revoke_serial_sf)

let updatedb_sf () =
  Inhca_tools.authorize_admin () >>
  Inhca_openssl.updatedb () >>= reword_openssl_error

let%client updatedb =
  ~%(Eliom_client.server_function [%json: unit] updatedb_sf)

let%client report_error msg =
  Dom_html.window##alert (Js.string msg); (* FIXME *)
  Lwt.return_unit

let%client refresh_page () =
  Eliom_client.change_page ~service:admin_service ~replace:true () ()

[%%client
  let tds_of_request request_link delete_handler req =
    let render_step = function
      | `generate_key -> D.pcdata "K"
      | `moderate -> D.pcdata "M"
      | `validate_email -> D.pcdata "V"
      | `fetch_certificate -> D.pcdata "F" in
    [ D.td [request_link req];
      D.td (List.map render_step req.request_pending);
      D.td [D.pcdata req.request_cn];
      D.td [D.pcdata req.request_email];
      D.td [D.button ~a:[D.a_button_type `Button;
                         D.a_onclick (delete_handler req)]
                     [D.pcdata "delete"]];
    ]
  let tr_of_request request_link delete_handler req =
    D.tr (tds_of_request request_link delete_handler req)

  module Request_set = Prime_enumset.Make
    (struct
      type t = request
      let compare r0 r1 =
        let o = compare r0.request_cn r1.request_cn in
        if o <> 0 then o else
        let o = compare r0.request_email r1.request_email in
        if o <> 0 then o else
        compare r0.request_id r1.request_id
    end)

]

let admin_handler () () =
  Inhca_tools.authorize_admin () >>

  let cn_input : Html_types.input elt =
    D.input ~a:[D.a_input_type `Text] () in
  let email_input : Html_types.input elt =
    D.input ~a:[D.a_input_type `Text] () in

  let add_handler = [%client fun ev ->
    let cn_input = To_dom.of_input ~%(cn_input : [`Input] elt) in
    let email_input = To_dom.of_input ~%(email_input : [`Input] elt) in
    let cn = Js.to_string cn_input##.value in
    let email = Js.to_string email_input##.value in
    if cn <> "" then begin
      cn_input##.value := Js.string "";
      email_input##.value := Js.string "";
      Lwt.async (fun () ->
        Lwt_log_js.error_f "Creating link for %s <%s>." cn email >>
        create_request (cn, email))
    end
  ] in
  let add_button =
    D.button ~a:[D.a_onclick add_handler; D.a_button_type `Button]
             [D.pcdata "add"] in
  let req_table =
    D.table ~a:[D.a_class ["std"]] [
      D.tr [D.th [D.pcdata "Id"]; D.th [D.pcdata "Pending"];
            D.th [D.pcdata "CN"]; D.th [D.pcdata "Email"]];
      D.tr [D.td []; D.td [];
            D.td [cn_input]; D.td [email_input];
            D.td [add_button]]
    ] in
  Inhca_tools.ignore_cv [%client

    let static_row_count = 2 in

    let delete_handler req (ev : Dom_html.mouseEvent Js.t) =
      (Js.Unsafe.coerce (Dom.eventTarget ev)
        :> Dom_html.inputElement Js.t)##.disabled := Js._true;
      Lwt.async (fun () ->
        delete_request (req.request_id, req.request_cn, req.request_email)) in

    let request_link req =
      D.a ~service:Inhca_public.acquire_service [D.pcdata req.request_id]
          req.request_id in

    Lwt.ignore_result begin
      let req_table_elem =
        To_dom.of_table ~%(req_table : Html_types.table elt) in
      let%lwt request_list = list_requests () in
      let request_set =
        ref (List.fold Request_set.add request_list Request_set.empty) in

      (* Populate the request table. *)
      Request_set.iter (fun req ->
        ignore (req_table_elem##appendChild
                  ((To_dom.of_tr
                      (tr_of_request request_link delete_handler req)
                    :> Dom.node Js.t))))
        !request_set;

      (* Keep the request table up to data. *)
      let update = function
        | `remove req ->
          Eliom_lib.debug "Deleting %s (%s, %s)."
                          req.request_id req.request_cn req.request_email;
          begin match Request_set.locate req !request_set with
          | false, _ -> Eliom_lib.error "Can't find the request to delete."
          | true, i ->
            request_set := Request_set.remove req !request_set;
            req_table_elem##deleteRow (static_row_count + i)
          end
        | `add req ->
          Eliom_lib.debug "Adding %s." req.request_id;
          let row =
            match Request_set.locate req !request_set with
            | false, i ->
              request_set := Request_set.add req !request_set;
              req_table_elem##insertRow (static_row_count + i)
            | true, i ->
              Js.Opt.get (req_table_elem##.rows##item (static_row_count + i))
                         (fun () -> failwith "Js.Opt.get") in
          List.iter
            (fun cell ->
              ignore (row##appendChild((To_dom.of_td cell :> Dom.node Js.t))))
            (tds_of_request request_link delete_handler req) in
      Lwt.async
        (fun () -> Lwt_stream.iter update (Eliom_bus.stream ~%edit_bus));

      Lwt.return_unit
    end
  ];

  let updatedb_button =
    let h = fun%client _ -> Lwt.async @@ fun () ->
      match%lwt updatedb () with
       | Ok () -> refresh_page ()
       | Error msg -> report_error msg in
    F.button ~a:[F.a_button_type `Button; F.a_onclick h] [F.pcdata "update"] in

  let issue_header_tr =
    F.tr [
      F.th [F.pcdata "SN"];
      F.th [F.pcdata "State"];
      F.th [F.pcdata "Expired"];
      F.th [F.pcdata "Revoked"];
      F.th [F.pcdata "DN"];
      F.td [updatedb_button];
    ] in

  let issue_tr issue =
    let open Inhca_openssl in

    let serial = Issue.serial issue in
    let state = Issue.state issue in

    let control =
      match state with
       | `Revoked -> []
       | `Expired | `Valid ->
          let h = fun%client _ -> Lwt.async @@ fun () ->
            match%lwt revoke_serial ~%serial with
             | Ok () -> refresh_page ()
             | Error msg -> report_error msg in
          [F.button ~a:[F.a_button_type `Button; F.a_onclick h]
                    [F.pcdata "revoke"]] in

    F.tr [
      F.td [F.pcdata (sprintf "%02x" serial)];
      F.td [F.pcdata (Issue.string_of_state state)];
      F.td [F.pcdata (Time_format.to_string (Issue.expired issue))];
      F.td
        (match Issue.revoked issue with
         | None -> []
         | Some d -> [F.pcdata (Time_format.to_string d)]);
      F.td [F.pcdata (Issue.dn issue)];
      F.td control;
    ] in

  let%lwt issue_trs = Lwt_stream.to_list @@
    Lwt_stream.map issue_tr (Inhca_openssl.Issue.load_all ()) in

  Lwt.return
    (Inhca_tools.F.page ~title:"Pending Certificate Requests" [
      F.h2 [F.pcdata "Pending Requests"];
      req_table;
      F.h2 [F.pcdata "Issued Certificates"];
      F.table ~a:[F.a_class ["std"]] (issue_header_tr :: issue_trs);
    ])

let () =
  Inhca_app.register ~service:admin_service admin_handler
