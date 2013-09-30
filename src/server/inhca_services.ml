open Inhca_data

let list_requests_service =
  Eliom_registration.Ocaml.register_coservice'
    (* ~csrf_safe:true *)
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      Ocsipersist.fold_step (fun k r rs -> Lwt.return (r :: rs))
			    request_table [])

let create_request_service =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(string "cn" ** string "email")
    (fun () (cn, email) ->
      let req = {
	request_id = fresh_request_id ();
	request_cn = cn;
	request_email = email;
	request_spkac = None;
	request_pending = [`generate_key; `fetch_certificate];
      } in
      Eliom_bus.write edit_bus (`add req);
      Lwt.return_unit)

let delete_request_service =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:Eliom_parameter.(string "request_id" **
				  string "cn" ** string "email")
    (fun () (request_id, (cn, email)) ->
      let req = {
	request_id = request_id;
	request_cn = cn;
	request_email = email;
	request_spkac = None; (* dummy for `remove *)
	request_pending = []; (* dummy for `remove *)
      } in
      Eliom_bus.write edit_bus (`remove req);
      Lwt.return_unit)
