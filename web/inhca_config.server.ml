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

  site_prefix: string list;
  (** A prefix for services to work around ocsigen/eliom#636. *)

  authn_bearer_jwk: Jose.Jwk.public Jose.Jwk.t option;
  (** JWK for validating bearer JWT. *)

  authn_http_header: string option;
  (** Trusted HTTP header used to identify a logged-in user.  This is an
      alternative to authn_bearer_jwk, which is suitable for testing or when
      proxying through a frontend running on the same server. *)

  authz_admins: string list;
  (** List of values for the given HTTP header which grant admin access. *)

  enrollment_expiration_time: float;
  (** Time in seconds before a new enrollment expires. *)
}

let bearer_jwk_decoder json =
  let conv_error = function
   | `Json_parse_failed msg -> Decoders.Error.make ("JSON parse error: " ^ msg)
   | `Msg msg -> Decoders.Error.make ("JWK parse error: " ^ msg)
   | `Unsupported_kty -> Decoders.Error.make "Unsupported JWK key type."
  in
  Jose.Jwk.of_pub_json (json : Yojson.Basic.t :> Yojson.Safe.t)
    |> Result.map_error conv_error

let decoder =
  let open Decode in
  let* subject_base_dn = field "subject_base_dn" string in
  let* site_prefix = field_opt_or ~default:[] "site_prefix" (list string) in
  let* authn_bearer_jwk = field_opt "authn_bearer_jwk" bearer_jwk_decoder in
  let* authn_http_header = field_opt "authn_http_header" string in
  let* authz_admins = field_opt_or ~default:[] "authz_admins" (list string) in
  let+ enrollment_expiration_time =
    field_opt_or ~default:259200.0 "enrollment_expiration_time" float in
  { subject_base_dn; site_prefix;
    authn_bearer_jwk; authn_http_header; authz_admins;
    enrollment_expiration_time }

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
