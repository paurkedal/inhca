(* Copyright (C) 2023--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

let string_of_ptime t =
  let tz_offset_s = Ptime_clock.current_tz_offset_s () in
  let pp = Ptime.pp_human ?tz_offset_s () in
  Fmt.to_to_string pp t

let ( let*? ) = Lwt_result.Syntax.( let* )
let ( let+? ) = Lwt_result.Syntax.( let+ )
let ( >>=? ) = Lwt_result.Infix.( >>= )
let ( >|=? ) = Lwt_result.Infix.( >|= )
