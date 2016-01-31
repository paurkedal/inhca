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

(** Interface to openssl commands. *)

val get_cadir : unit -> string
val get_capath : string -> string
(** Paths to the CA. *)

val get_tmpdir : unit -> string
val get_tmppath : string -> string
(** Paths to use for temporary files. *)

val openssl : string -> string list -> unit Lwt.t
(** [openssl cmd args] runs the openssl sub-command [cmd] with the given
    arguments along with a -config option pointing to the configuration file
    in the CA directory. *)

val sign_spkac : ?days: int -> string -> (string * string) list -> string Lwt.t
(** [sign_spkac request_id spkac] signs the SPKAC (Signed Public Key and
    Challenge) given by [spkac] and returns the certificate.  The [request_id]
    is only used for a temporary path.

    @param days The number of days the certificate will be valid, 365 by
    default. *)
