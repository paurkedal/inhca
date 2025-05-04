(* Copyright (C) 2021--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Lwt.Syntax

include Enrollment_core

let updates, send_update = React.E.create ()

let default_expiration = Config.(global.enrollment_expiration_time)

let random_token () =
  let data = Mirage_crypto_rng.generate 15 in
  Base64.(encode_string ~alphabet:uri_safe_alphabet) data

let create ~cn ~email () =
  let token = random_token () in
  let expiration = Unix.time () +. default_expiration in
  {token; state = Prepared; expiration; cn; email}

let update ~state enr = {enr with state}

let ocsipersist_table : t Ocsipersist.table Lwt.t =
  Ocsipersist.open_table "enrollment"

(* TODO: Utilize the result return types for save, delete, and all. *)

let save_exn enr =
  let* tbl = ocsipersist_table in
  Ocsipersist.add tbl (token enr) enr

let save enr =
  let+ () = save_exn enr in
  send_update (Add enr);
  Ok ()

let delete enr =
  let* tbl = ocsipersist_table in
  let+ () = Ocsipersist.remove tbl (token enr) in
  send_update (Remove enr);
  Ok ()

let all () =
  let* tbl = ocsipersist_table in
  Ocsipersist.fold_table (fun _ enr acc -> Lwt.return (enr :: acc)) tbl []
    >|= Result.ok (* TODO *)
