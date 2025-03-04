(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

module Decode = Decoders_yojson.Basic.Decode

type t = {
  subject_base_dn: string;
  (** Subject base DN. *)

  auth_http_header: string option;
  (** HTTP header used to identify a logged-in user (e.g. SSL_CLIENT_S_DN). *)

  auth_admins: string list;
  (** List of values for the given HTTP header which grant admin access. *)

  enrollment_expiration_time: float;
  (** Time in seconds before a new enrollment expires. *)
}

let decoder =
  let open Decode in
  let* subject_base_dn = field "subject_base_dn" string in
  let* auth_http_header = field_opt "auth_http_header" string in
  let* auth_admins = field_opt_or ~default:[] "auth_admins" (list string) in
  let+ enrollment_expiration_time =
    field_opt_or ~default:259200.0 "enrollment_expiration_time" float in
  { subject_base_dn; auth_http_header; auth_admins; enrollment_expiration_time }

let global = Lwt_main.run begin
  let open Lwt.Syntax in
  let path =
    try Sys.getenv "INHCA_CONFIG" with Not_found -> "/etc/inhca.json"
  in
  let+ content = Lwt_io.with_file ~mode:Lwt_io.input path Lwt_io.read in
  (match Decode.decode_value decoder (Yojson.Basic.from_string content) with
   | Ok v -> v
   | Error msg ->
      Fmt.failwith "Cannot load %s: %a" path Decode.pp_error msg
   | exception Yojson.Json_error msg ->
      Fmt.failwith "Cannot load %s: %s" path msg)
end
