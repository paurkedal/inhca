open Printf

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
  request_spac : string option;
  request_pending : request_step list;
  (*
  request_preparation_time : float option;
  request_key_generation_time : float option;
  request_moderation_time : float option;
  request_email_validation_time : float option;
  *)
} deriving (Json)

let fresh_request_id () =
  (* FIXME: Make sure the random numbers are really unpredictable. *)
  let code = Random.int64 Int64.max_int in
  sprintf "%0Lx" code

let () = Random.self_init ()
