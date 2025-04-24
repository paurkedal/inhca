(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cmdliner

let serve_cmd =
  let doc = "InhCA web server" in
  let info = Cmd.info ~doc "serve" in
  Cmd.v info
    Term.(const Result.ok $ (const Inhca_web_server.Main.serve $ const ()))

let main_cmd =
  let info = Cmd.info "inhca" in
  Cmd.group info [
    serve_cmd;
  ]

let () = exit (Cmd.eval_result main_cmd)
