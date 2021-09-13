(* Copyright (C) 2021--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

include module type of (struct include Enrollment_core end)

val create : cn: string -> email: string -> unit -> t

val update : state: state -> t -> t

val has_expired : t -> bool

val ocsipersist_table : t Ocsipersist.table Lwt.t

val save : t -> (unit, 'err) result Lwt.t

val save_exn : t -> unit Lwt.t

val delete : t -> (unit, 'err) result Lwt.t

val all : unit -> (t list, 'err) result Lwt.t

(* FIXME
val edit_bus : (edit_message, edit_message) Eliom_bus.t
*)
