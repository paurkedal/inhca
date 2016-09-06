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

module F = struct

  let page ~title contents =
    (Eliom_tools.F.html ~title ~css:[["css"; "inhca.css"]]
      (Html5.F.body (Html5.F.h1 [Html5.F.pcdata title] :: contents)))

  let send_error ~code msg =
    let hdr = sprintf "Error %d" code in
    Eliom_registration.Html5.send ~code
      (Eliom_tools.F.html ~title:hdr ~css:[["css"; "inhca.css"]]
        Html5.F.(body [h1 [pcdata hdr]; p [pcdata msg]]))

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
    then Lwt.return_unit
    else http_error 403 "Admin access required."