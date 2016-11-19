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

[%%shared.start]
open Unprime_string

module Date = CalendarLib.Date
module Date_format = CalendarLib.Printer.Date
module Time = CalendarLib.Calendar
module Time_format = CalendarLib.Printer.Calendar

[%%server.start]
let dnc_of_string s =
  match String.cut_affix "=" s with
   | None -> invalid_arg "dnc_of_string"
   | Some (at, av) ->
      match String.lowercase_ascii at with
       | "cn" -> `CN av
       | "serialnumber" -> `Serialnumber av
       | "c" -> `C av
       | "l" -> `L av
       | "sp" -> `SP av
       | "o" -> `O av
       | "ou" -> `OU av
       | "t" -> `T av
       | "dnq" -> `DNQ av
       | "mail" -> `Mail av
       | "dc" -> `DC av
       | "given_name" -> `Given_name av (* ? *)
       | "surname" -> `Surname av
       | "initials" -> `Initials av
       | "pseudonym" -> `Pseudonym av
       | "generation" -> `Generation av
       | _ -> failwith ("Unsupported DN component " ^ at ^ ".")

let dn_of_string s = List.map dnc_of_string (String.chop_affix "," s)
let string_of_dn = X509.distinguished_name_to_string
