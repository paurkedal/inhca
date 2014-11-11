{shared{
  open Eliom_content
  open Inhca_data
  open Unprime_list
  open Unprime_option
}}

open Inhca_services

module Main_app =
  Eliom_registration.App (struct let application_name = "admin" end)

let main_service =
  Eliom_service.App.service ~path:["admin"] ~get_params:Eliom_parameter.unit ()

{client{

  let tds_of_request request_link delete_handler req =
    let open Html5.D in
    let render_step = function
      | `generate_key -> pcdata "K"
      | `moderate -> pcdata "M"
      | `validate_email -> pcdata "V"
      | `fetch_certificate -> pcdata "F" in
    [td [request_link req];
     td (List.map render_step req.request_pending);
     td [pcdata req.request_cn];
     td [pcdata req.request_email];
     td [button ~a:[a_onclick (delete_handler req)]
		~button_type:`Button [pcdata "delete"]];
     ]
  let tr_of_request request_link delete_handler req =
    Html5.D.tr (tds_of_request request_link delete_handler req)

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

}}

let main_handler () () =
  Inhca_tools.authorize_admin () >>

  let cn_input = Html5.D.input ~input_type:`Text () in
  let email_input = Html5.D.input ~input_type:`Text () in

  let add_handler = {{fun ev ->
    let cn_input = Html5.To_dom.of_input %cn_input in
    let email_input = Html5.To_dom.of_input %email_input in
    let cn = Js.to_string cn_input##value in
    let email = Js.to_string email_input##value in
    if cn <> "" then begin
      cn_input##value <- Js.string "";
      email_input##value <- Js.string "";
      Lwt.async (fun () ->
	Eliom_client.call_ocaml_service ~service:%create_request_service
	  () (cn, email))
    end
  }} in
  let add_button = Html5.D.(button ~a:[a_onclick add_handler]
				   ~button_type:`Button [pcdata "add"]) in
  let req_table = Html5.D.(
    table ~a:[a_class ["std"]]
      [tr [th [pcdata "Id"]; th [pcdata "Pending"];
	   th [pcdata "CN"]; th [pcdata "Email"]];
       tr [td []; td []; td [cn_input]; td [email_input]; td [add_button]]]
  ) in
  ignore {unit{

    let static_row_count = 2 in

    let delete_handler req (ev : Dom_html.mouseEvent Js.t) =
      (Js.Unsafe.coerce (Dom.eventTarget ev)
	:> Dom_html.inputElement Js.t)##disabled <- Js._true;
      Lwt.async (fun () ->
	Eliom_client.call_ocaml_service ~service:%delete_request_service ()
	  (req.request_id, (req.request_cn, req.request_email))) in

    let request_link req =
      Html5.D.(a ~service:%Inhca_public.keygen_service [pcdata req.request_id]
		 req.request_id) in

    Lwt.async_exception_hook := begin fun xc ->
      Eliom_lib.error "Inhca_ca client: %s" (Printexc.to_string xc)
    end;

    Lwt.ignore_result begin
      let req_table_elem = Html5.To_dom.of_table %req_table in
      lwt request_list =
	Eliom_client.call_ocaml_service ~service:%list_requests_service () () in
      let request_set =
	ref (List.fold Request_set.add request_list Request_set.empty) in

      (* Populate the request table. *)
      Request_set.iter (fun req ->
	ignore (req_table_elem##appendChild
		  ((Html5.To_dom.of_tr
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
	      Js.Opt.get (req_table_elem##rows##item (static_row_count + i))
			 (fun () -> failwith "Js.Opt.get") in
	  List.iter
	    (fun cell ->
	      ignore (row##appendChild((Html5.To_dom.of_td cell
					:> Dom.node Js.t))))
	    (tds_of_request request_link delete_handler req) in
      Lwt.async
	(fun () -> Lwt_stream.iter update (Eliom_bus.stream %edit_bus));

      Lwt.return_unit
    end
  }};
  Lwt.return
    (Inhca_tools.F.page ~title:"Pending Certificate Requests" [req_table])

let () =
  Main_app.register ~service:main_service main_handler
