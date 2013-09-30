{shared{
  open Eliom_content
  open Inhca_data
  open Unprime_list
  open Unprime_option
}}

let request_table : Inhca_data.request Ocsipersist.table =
  Ocsipersist.open_table "requests"

module Main_app =
  Eliom_registration.App (struct let application_name = "src-inhca_admin" end)

let main_service =
  Eliom_service.service ~path:["admin"] ~get_params:Eliom_parameter.unit ()

let req_list_service =
  Eliom_registration.Ocaml.register_coservice'
    (* ~csrf_safe:true *)
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      Ocsipersist.fold_step (fun k r rs -> Lwt.return (r :: rs))
			    request_table [])

{shared{
  type edit_message =
    [ `remove of request
    | `add of request ]
    deriving (Json)
}}

let edit_bus = Eliom_bus.create Json.t<edit_message>

{client{

  let tds_of_request r =
    let open Html5.D in
    let render_step = function
      | `generate_key -> pcdata "K"
      | `moderate -> pcdata "M"
      | `validate_email -> pcdata "V"
      | `fetch_certificate -> pcdata "F" in
    [td [pcdata r.request_id];
     td (List.map render_step r.request_pending);
     td [pcdata r.request_cn];
     td [pcdata r.request_email]]
  let tr_of_request r = Html5.D.tr (tds_of_request r)

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
  let cn_input = Html5.D.input ~input_type:`Text () in
  let email_input = Html5.D.input ~input_type:`Text () in
  let add_handler = {{fun ev ->
    let req = {
      request_id = fresh_request_id ();
      request_cn = Js.to_string (Html5.To_dom.of_input %cn_input)##value;
      request_email = Js.to_string (Html5.To_dom.of_input %email_input)##value;
      request_spac = None;
      request_pending = [`generate_key; `fetch_certificate];
    } in
    Lwt.async (fun () -> Eliom_bus.write %edit_bus (`add req))
  }} in
  let add_button = Html5.D.(button ~a:[a_onclick add_handler]
				   ~button_type:`Button [pcdata "add"]) in
  let req_table = Html5.D.(
    table ~a:[a_class ["std"]]
      (tr [th [pcdata "Id"]; th [pcdata "Pending"];
	   th [pcdata "Name"]; th [pcdata "Email"]])
      [tr [td []; td []; td [cn_input]; td [email_input]; td [add_button]]]
  ) in
  ignore {unit{
    let static_row_count = 2 in
    Lwt.ignore_result begin
      Lwt.async_exception_hook := begin fun xc ->
	Eliom_lib.error "Inhca_ca client: %s" (Printexc.to_string xc)
      end;
      let req_table_elem = Html5.To_dom.of_table %req_table in
      lwt request_list =
	Eliom_client.call_caml_service ~service:%req_list_service () () in
      let request_set =
	ref (List.fold Request_set.add request_list Request_set.empty) in
      Request_set.iter (fun r ->
	ignore (req_table_elem##appendChild
		  ((Html5.To_dom.of_tr (tr_of_request r) :> Dom.node Js.t))))
	!request_set;
      let update = function
	| `remove req ->
	  Eliom_lib.debug "Deleting %s." req.request_id;
	  begin match Request_set.locate req !request_set with
	  | None -> Eliom_lib.error "Can't find the request to delete."
	  | Some i ->
	    request_set := Request_set.remove req !request_set;
	    req_table_elem##deleteRow (static_row_count + i)
	  end
	| `add req ->
	  Eliom_lib.debug "Adding %s." req.request_id;
	  let row =
	    match Request_set.locate req !request_set with
	    | None ->
	      request_set := Request_set.add req !request_set;
	      let i = Option.get (Request_set.locate req !request_set) in
	      req_table_elem##insertRow (static_row_count + i)
	    | Some i ->
	      Js.Opt.get (req_table_elem##rows##item (static_row_count + i))
			 (fun () -> failwith "Js.Opt.get") in
	  List.iter
	    (fun cell ->
	      ignore (row##appendChild((Html5.To_dom.of_td cell
					:> Dom.node Js.t))))
	    (tds_of_request req) in
      Lwt.async
	(fun () -> Lwt_stream.iter update (Eliom_bus.stream %edit_bus));
      Lwt.return_unit
    end
  }};
  Lwt.return
    (Inhca_tools.F.page ~title:"Pending Certificate Requests" [req_table])

let _ =
  Lwt_stream.iter_s
    begin function
    | `add req -> Ocsipersist.add request_table req.request_id req
    | `remove req -> Ocsipersist.remove request_table req.request_id
    end
    (Eliom_bus.stream edit_bus)

let () =
  Main_app.register ~service:main_service main_handler
