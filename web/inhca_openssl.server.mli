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

type error = Lwt_process.command * Unix.process_status * string list

val log_error : error -> unit Lwt.t

val get_cadir : unit -> string
val get_capath : string -> string
(** Paths to the CA. *)

val get_tmpdir : unit -> string
val get_tmppath : string -> string
(** Paths to use for temporary files. *)

(** Read-only interface to the index.txt file of the CA. *)
module Issue : sig
  type state = [`Revoked | `Expired | `Valid]

  val string_of_state : state -> string

  type t
  (** Information about an issued certificate. *)

  val state : t -> state
  (** The current state of an issued certificate. *)

  val expired : t -> CalendarLib.Calendar.t
  (** The expiration time of the certificate. *)

  val revoked : t -> CalendarLib.Calendar.t option
  (** The time the certificate was revoked if any. *)

  val serial : t -> int
  (** The serial number of the certificate. *)

  val dn : t -> string
  (** The distinguished name of the certificate. *)

  val load_all : unit -> t Lwt_stream.t
  (** The current index.txt entries of the CA. *)
end

val sign_spkac : ?days: int -> request_id: string -> (string * string) list ->
                 (string, error) result Lwt.t
(** [sign_spkac spkac] signs the SPKAC (Signed Public Key and Challenge) given
    by [spkac] and returns the certificate.

    @param days
      The number of days the certificate will be valid, 365 by default.
    @param request_id
      Used for constructing a temporary path. *)

val revoke_serial : int -> (unit, error) result Lwt.t
(** [revoke_serial n] revokes the certificate with serial number [n].  *)

val updatedb : unit -> (unit, error) result Lwt.t
(** [updatedb ()] purges expired certificates from the index. *)

val sign_pem : ?days: int -> request_id: string -> string ->
               (string, error) result Lwt.t
(** [sign_pem request_id csr] signs the given PEM formatted CSR [csr] returning
    the certificate in PEM format.

    @param request_id
      Used for constructing a temporary path. *)

val export_pkcs12 :
  ?name: string -> password: string ->
  cert: string -> certkey: string ->
  unit -> (string, error) result Lwt.t
