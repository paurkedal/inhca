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

open Lwt.Infix
open Inhca_data

let admin_service =
  let open Eliom_service in
  let get = Eliom_parameter.unit in
  create ~meth:(Get get) ~path:(Path ["admin"; ""]) ()
let%client admin_service = ~%admin_service

let admin_server_function json f =
  Eliom_client.server_function json
    (fun args -> Inhca_tools.authorize_admin () >> f args)

let list_enrollments () =
  enrollment_table >>= fun enrollment_table ->
  Ocsipersist.fold_step (fun k r rs -> Lwt.return (r :: rs))
                        enrollment_table []

let%client list_enrollments =
  ~%(admin_server_function [%json: unit] list_enrollments)

let create_enrollment (cn, email) =
  Lwt_log.info_f "Creating requset for %s <%s>." cn email >>
  let enr = Enrollment.create ~cn ~email () in
  Eliom_bus.write edit_bus (`Add enr)

let%client create_enrollment =
  ~%(admin_server_function [%json: string * string] create_enrollment)

let delete_enrollment enr =
  Lwt_log.info_f "Deleting enrollment for %s <%s>."
    (Enrollment.cn enr) (Enrollment.email enr) >>
  Eliom_bus.write edit_bus (`Remove enr)

let%client delete_enrollment =
  ~%(admin_server_function [%json: Enrollment.t] delete_enrollment)
