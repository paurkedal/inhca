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

let group = new Config_file.group

let subject_base_dn =
  new Config_file.string_cp ~group ["subject"; "base_dn"]
                            "OU=Inhca,O=Example" "Subject base DN"

let auth_http_header_cp =
  new Config_file.option_cp Config_file.string_wrappers ~group
                            ["auth"; "http_header"]
    None "HTTP header used to identify a logged-in user (e.g. SSL_CLIENT_S_DN)."
let auth_admins_cp =
  new Config_file.list_cp Config_file.string_wrappers ~group ["auth"; "admins"]
    [] "List of values for the given HTTP header which grant admin access."

let enrollment_expiration_cp =
  new Config_file.float_cp
    ~group ["enrollment"; "expiration_time"] 259200.0
    "Time in seconds before a new enrollment expires."

let () =
  group#read (Filename.concat (Ocsigen_config.get_datadir ()) "inhca.conf")
