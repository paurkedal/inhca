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

open Eliom_content
open Printf
open Lwt.Infix

let ignore_cv (x : unit Eliom_client_value.t) = ignore x

module F = struct

  let page ~title contents =
    (Eliom_tools.F.html ~title ~css:[["inhca.css"]]
      (Html.F.body (Html.F.h1 [Html.F.txt title] :: contents)))

  let error_page ~code msg =
    let hdr = sprintf "Error %d" code in
    Eliom_tools.F.html ~title:hdr ~css:[["inhca.css"]]
      Html.F.(body [h1 [txt hdr]; p [txt msg]])

  let send_error_page ~code msg =
    Eliom_registration.Html.send ~code (error_page ~code msg)

  let send_page ?code ~title contents =
    let title =
      (match code with
       | None -> title
       | Some code -> sprintf "Error %d, %s" code title)
    in
    Eliom_registration.Html.send ?code (page ~title contents)
end

let authorization_hdr = Ocsigen_header.Name.of_string "Authorization"

let authenticate () =
  let request = Eliom_request_info.get_ri () in
  (match Inhca_config.(global.authn_bearer_jwk),
         Ocsigen_request.header request authorization_hdr with
   | Some jwk, Some data ->
      (match String.split_on_char ' ' data |> List.filter ((<>) "") with
       | ["Bearer"; token] ->
          let now = Ptime_clock.now () in
          (match Jose.Jwt.of_string ~jwk ~now token with
           | Ok jwt ->
              Ok (Jose.Jwt.get_string_claim jwt "sub")
           | Error `Expired ->
              Error "The bearer token has expired."
           | Error `Invalid_signature ->
              Error "The signature of the bearer token is invalid."
           | Error (`Msg msg) ->
              Error ("Bad bearer token: " ^ msg)
           | Error `Not_json ->
              Error "Bearer token data is not JSON."
           | Error `Not_supported ->
              Error "Bearer token format unsupported.")
       | _ ->
          Error "Authorization header is not a bearer token.")
   | None, _ | _, None ->
      (match Inhca_config.(global.authn_http_header) with
       | None ->
          Error "Authentication not configured."
       | Some header_name ->
          let header_name = Ocsigen_header.Name.of_string header_name in
          Ok (Ocsigen_request.header request header_name)))

let authorize_admin' ~on_error handler =
  (match authenticate () with
   | Ok (Some user) ->
      if List.mem user Inhca_config.(global.authz_admins) then
        Lwt_log.info_f "Allowing admin acces for %s." user >>= fun () ->
        handler ()
      else
        Lwt_log.info_f "Denying admin access for %s." user >>= fun () ->
        on_error ~code:403 "Admin access required."
   | Ok None ->
      on_error ~code:401 "Not authenticated."
   | Error msg ->
      on_error ~code:401 msg)

let authorize_admin handler =
  let on_error ~code msg = Lwt.return (F.error_page ~code msg) in
  authorize_admin' ~on_error handler

let authorize_admin_exn handler =
  let on_error ~code msg =
    (match Cohttp.Code.status_of_code code with
     | #Cohttp.Code.status as status ->
        Lwt.fail (Ocsigen_cohttp.Ext_http_error (status, Some msg, None))
     | `Code _ -> assert false)
  in
  authorize_admin' ~on_error handler

module Local_log (Spec : sig val section_name : string end) = struct
  let section = Lwt_log.Section.make Spec.section_name

  let debug = Lwt_log.debug ~section
  let debug_f = Lwt_log.debug_f ~section
  let ign_debug = Lwt_log.ign_debug ~section
  let ign_debug_f = Lwt_log.ign_debug_f ~section
  let info = Lwt_log.info ~section
  let info_f = Lwt_log.info_f ~section
  let ign_info = Lwt_log.ign_info ~section
  let ign_info_f = Lwt_log.ign_info_f ~section
  let notice = Lwt_log.notice ~section
  let notice_f = Lwt_log.notice_f ~section
  let ign_notice = Lwt_log.ign_notice ~section
  let ign_notice_f = Lwt_log.ign_notice_f ~section
  let warning = Lwt_log.warning ~section
  let warning_f = Lwt_log.warning_f ~section
  let ign_warning = Lwt_log.ign_warning ~section
  let ign_warning_f = Lwt_log.ign_warning_f ~section
  let error = Lwt_log.debug ~section
  let error_f = Lwt_log.debug_f ~section
  let ign_error = Lwt_log.ign_debug ~section
  let ign_error_f = Lwt_log.ign_error_f ~section
  let fatal = Lwt_log.debug ~section
  let fatal_f = Lwt_log.debug_f ~section
  let ign_fatal = Lwt_log.ign_debug ~section
  let ign_fatal_f = Lwt_log.ign_fatal_f ~section
end
