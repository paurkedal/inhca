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

let ignore_cv (x : unit Eliom_client_value.t) = ignore x

module F = struct

  let page ~title contents =
    (Eliom_tools.F.html ~title ~css:[["inhca.css"]]
      (Html.F.body (Html.F.h1 [Html.F.txt title] :: contents)))

  let send_error ~code msg =
    let hdr = sprintf "Error %d" code in
    Eliom_registration.Html.send ~code
      (Eliom_tools.F.html ~title:hdr ~css:[["inhca.css"]]
        Html.F.(body [h1 [txt hdr]; p [txt msg]]))

  let send_page ?code ~title contents =
    let title =
      (match code with
       | None -> title
       | Some code -> sprintf "Error %d, %s" code title) in
    Eliom_registration.Html.send ?code (page ~title contents)
end

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))
let http_error_f code fmt = ksprintf (http_error code) fmt

let authorize_admin () =
  match Inhca_config.auth_http_header_cp#get with
  | None -> Lwt.return_unit
  | Some h ->
    let ri = Eliom_request_info.get_ri () in
    let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
    let%lwt user =
      try Lwt.return (Ocsigen_headers.find h frame)
      with Not_found -> http_error 500 "Missing authentication header." in
    if List.mem user Inhca_config.auth_admins_cp#get
    then Lwt_log.info_f "Authorized %s." user
    else http_error 403 "Admin access required."

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
