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

[%%shared.start]

module Enrollment_base : sig

  type state =
    | Prepared
    | Visited
    | Acquired
    | Failed
    | Revoked
    [@@deriving json]

  val string_of_state : state -> string

  type t [@@deriving json]

  val compare : t -> t -> int

  val token : t -> string

  val expiration : t -> float

  val expiration_time : t -> Ptime.t

  val cn : t -> string

  val email : t -> string

  val has_expired : t -> bool

  val state : t -> state
end

[%%client.start]
module Enrollment = Enrollment_base

[%%server.start]
module Enrollment : sig
  include module type of Enrollment_base
    with type t = Enrollment_base.t

  val create : cn: string -> email: string -> unit -> t

  val update : state: state -> t -> t
end

[%%shared.start]
type edit_message =
  [ `Remove of Enrollment.t
  | `Update of Enrollment.t
  | `Add of Enrollment.t ]
  [@@deriving json]

[%%server.start]

val enrollment_table : Enrollment.t Ocsipersist.table Lwt.t

val edit_bus : (edit_message, edit_message) Eliom_bus.t
