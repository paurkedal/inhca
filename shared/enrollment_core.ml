(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@warning "-39"] (* deriving *)

type state =
  | Prepared
  | Visited
  | Acquired
  | Failed
  | Revoked
[@@deriving rpcty]

let string_of_state = function
 | Prepared -> "prepared"
 | Visited -> "visited"
 | Acquired -> "acquired"
 | Failed -> "failed"
 | Revoked -> "revoked"

type t = {
  token: string;     (* key *)
  cn: string;        (* key *)
  email: string;     (* key *)
  expiration: float;
  state: state;
}
[@@deriving rpcty]

type t_list = t list
[@@deriving rpcty]

let compare enr1 enr2 =
  let o = compare enr1.cn enr2.cn in if o <> 0 then o else
  let o = compare enr1.email enr2.email in if o <> 0 then o else
  compare enr1.token enr2.token

let token enr = enr.token
let state enr = enr.state
let expiration enr = enr.expiration
let expiration_time enr =
  (match Ptime.of_float_s enr.expiration with
   | None -> invalid_arg "Expiration time is out of range."
   | Some t -> t)

let has_expired enr =
  Ptime.compare (expiration_time enr) (Ptime_clock.now ()) < 0

let cn enr = enr.cn
let email enr = enr.email

(*
type edit_message =
  [ `Remove of Enrollment.t
  | `Update of Enrollment.t
  | `Add of Enrollment.t ]
  [@@deriving json]
*)
