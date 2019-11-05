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

[%%shared.start]
module Time = CalendarLib.Calendar

module Enrollment_base = struct

  type state =
    | Prepared
    | Visited
    | Acquired
    | Failed
    | Revoked
    [@@deriving json]

  let string_of_state = function
   | Prepared -> "prepared"
   | Visited -> "visited"
   | Acquired -> "acquired"
   | Failed -> "failed"
   | Revoked -> "revoked"

  type t = {
    token : string;     (* key *)
    cn : string;        (* key *)
    email : string;     (* key *)
    expiration : float;
    state : state;
  } [@@deriving json]

  let compare enr1 enr2 =
    let o = compare enr1.cn enr2.cn in if o <> 0 then o else
    let o = compare enr1.email enr2.email in if o <> 0 then o else
    compare enr1.token enr2.token

  let token enr = enr.token
  let state enr = enr.state
  let expiration enr = enr.expiration
  let expiration_time enr = Time.from_unixfloat enr.expiration
  let cn enr = enr.cn
  let email enr = enr.email
  let has_expired enr = enr.expiration < Unix.time ()
end

module%client Enrollment = Enrollment_base

module%server Enrollment = struct
  include Enrollment_base

  let default_expiration = Inhca_config.enrollment_expiration_cp#get

  let create ~cn ~email () =
    let token =
      String.map (function '/' -> '-' | c -> c)
        (Ocsigen_lib.make_cryptographic_safe_string ()) in
    let expiration = Unix.time () +. default_expiration in
    {token; state = Prepared; expiration; cn; email}

(*
  let create_dummy ~cn ~email ~token () =
    {token; state = Prepared; expiration = 0.0; cn; email}
*)

  let update ~state enr = {enr with state}
end

type edit_message =
  [ `Remove of Enrollment.t
  | `Update of Enrollment.t
  | `Add of Enrollment.t ]
  [@@deriving json]

[%%server.start]
open Lwt.Infix

let enrollment_table : Enrollment.t Ocsipersist.table Lwt.t =
  Ocsipersist.open_table "enrollment"

let edit_bus = Eliom_bus.create [%json: edit_message]

let () = Lwt.async @@ fun () ->
  enrollment_table >>= fun enrollment_table ->
  Lwt_stream.iter_s
    (function
     | `Add enr ->
        Ocsipersist.add enrollment_table (Enrollment.token enr) enr
     | `Update enr ->
        Ocsipersist.add enrollment_table (Enrollment.token enr) enr
     | `Remove enr ->
        Ocsipersist.remove enrollment_table (Enrollment.token enr))
    (Eliom_bus.stream edit_bus)
