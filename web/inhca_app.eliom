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

open Unprime_option

include Eliom_registration.App (struct
  let application_name = "inhca"
  let global_data_path = None
end)

let () =
  let lwt_log =
    try Some (Sys.getenv "LWT_LOG") with Not_found -> None in
  let lwt_log_js =
    try Some (Sys.getenv "LWT_LOG_JS") with Not_found -> lwt_log in
  Option.iter Eliom_lib.Lwt_log.load_rules lwt_log;
  Option.iter
    (fun rules -> Inhca_tools.ignore_cv [%client Lwt_log_js.load_rules ~%rules])
    lwt_log_js
