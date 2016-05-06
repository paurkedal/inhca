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

open Printf

[%%shared
  type request_id = int * int

  type request_step =
    [ `generate_key
    | `moderate
    | `validate_email
    | `fetch_certificate ]
    [@@deriving json]

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
  } [@@deriving json]

  type edit_message =
    [ `remove of request
    | `add of request ]
    [@@deriving json]
]

[%%server
  let request_table : request Ocsipersist.table =
    Ocsipersist.open_table "requests"

  let edit_bus = Eliom_bus.create [%json: edit_message]

  let fresh_request_id () =
    String.map (function '/' -> '-' | c -> c)
      (Ocsigen_lib.make_cryptographic_safe_string ())

  let () =
    Lwt.async (fun () ->
      Lwt_stream.iter_s
        (function
          | `add req -> Ocsipersist.add request_table req.request_id req
          | `remove req -> Ocsipersist.remove request_table req.request_id)
        (Eliom_bus.stream edit_bus))
]
