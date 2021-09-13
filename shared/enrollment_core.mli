(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

type state =
  | Prepared
  | Visited
  | Acquired
  | Failed
  | Revoked
(*
[@@deriving rpcty]
*)

val string_of_state : state -> string

type t = {
  token: string;     (* key *)
  cn: string;        (* key *)
  email: string;     (* key *)
  expiration: float;
  state: state;
}
(*
[@@deriving rpcty]
*)
val t : t Rpc.Types.def

val t_list : t list Rpc.Types.def

val compare : t -> t -> int

val token : t -> string

val expiration : t -> float

val expiration_time : t -> Ptime.t

val has_expired : t -> bool

val cn : t -> string

val email : t -> string

val state : t -> state

(*
type edit_message =
  [ `Remove of Enrollment.t
  | `Update of Enrollment.t
  | `Add of Enrollment.t ]
  [@@deriving json]
*)
