open Printf

{shared{
  type request_id = int * int

  type request_step =
    [ `generate_key
    | `moderate
    | `validate_email
    | `fetch_certificate ]
    deriving (Json)

  type request = {
    request_id : string;
    request_cn : string;
    request_email : string;
    request_spkac : string option;
    request_pending : request_step list;
    (*
    request_preparation_time : float option;
    request_key_generation_time : float option;
    request_moderation_time : float option;
    request_email_validation_time : float option;
    *)
  } deriving (Json)

  type edit_message =
    [ `remove of request
    | `add of request ]
    deriving (Json)
}}

{server{
  let request_table : request Ocsipersist.table =
    Ocsipersist.open_table "requests"

  let edit_bus = Eliom_bus.create Json.t<edit_message>

  let fresh_request_id () = Ocsigen_lib.make_cryptographic_safe_string ()

  let () =
    Lwt.async (fun () ->
      Lwt_stream.iter_s
	(function
	  | `add req -> Ocsipersist.add request_table req.request_id req
	  | `remove req -> Ocsipersist.remove request_table req.request_id)
	(Eliom_bus.stream edit_bus))
}}
