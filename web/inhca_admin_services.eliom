(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared.start]

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
      Eliom_bus.write edit_bus (`add req))

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
      Eliom_bus.write edit_bus (`remove req))

let admin_service =
  Eliom_service.App.service ~path:["admin"] ~get_params:Eliom_parameter.unit ()

let admin_certificates_service =
  Eliom_service.App.service ~path:["admin"; "certificates"]
                            ~get_params:Eliom_parameter.unit ()
