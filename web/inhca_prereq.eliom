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

module Dn = struct
  include X509.Distinguished_name
  let cn s = Relative_distinguished_name.singleton (CN s)
end

let rdn_of_string s =
  (* FIXME: Expand escapes. *)
  (match String.cut_affix "=" s with
   | None -> invalid_arg "rdn_of_string"
   | Some (at, av) ->
      let open X509.Distinguished_name in
      Relative_distinguished_name.singleton @@
      (match String.lowercase_ascii at with
       | "cn" -> CN av
       | "serialnumber" -> Serialnumber av
       | "c" -> C av
       | "l" -> L av
       | "st" -> ST av
       | "o" -> O av
       | "ou" -> OU av
       | "t" -> T av
       | "dnq" -> DNQ av
       | "mail" -> Mail av
       | "dc" -> DC av
       | "given_name" -> Given_name av
       | "surname" -> Surname av
       | "initials" -> Initials av
       | "pseudonym" -> Pseudonym av
       | "generation" -> Generation av
       | "street" -> Street av
       | "userid" -> Userid av
       | _ -> failwith ("Unsupported DN component " ^ at ^ ".")))

let dn_of_string s =
  (* FIXME: Escaped or quoted commas. *)
  List.map rdn_of_string (String.chop_affix "," s)

let string_of_dn = Fmt.to_to_string X509.Distinguished_name.pp
