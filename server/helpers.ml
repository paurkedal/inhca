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

open Lwt.Syntax
module H = Tyxml_html

let log_src = Logs.Src.create "inhca"
module Log = (val Logs_lwt.src_log log_src)

let respond_with_error ~status msg =
  let headers = [
    "Content-Type", "text/plain";
  ] in
  Dream.respond ~status ~headers msg

let pp_html = H.pp ()

let respond_with_page ?status ~title content =
  let page =
    H.html
      (H.head (H.title (H.txt title)) [
        H.link ~rel:[`Stylesheet] ~href:"static/inhca.css" ();
        H.script ~a:[H.a_src "static/inhca_client.bc.js"] (H.txt ""); (* FIXME *)
      ])
      (H.body (H.h1 [H.txt title] :: content))
  in
  let content = Format.asprintf "%a" pp_html page in
  let headers = [
    "Content-Type", "text/html";
  ] in
  Dream.respond ?status ~headers content

let respond_with_message ?status ~title msg =
  respond_with_page ?status ~title [H.p [H.txt msg]]

let respond_with_internal_server_error msg =
  let now = lazy (Ptime_clock.now ()) in
  let* () = Log.err (fun f -> let _ = Lazy.force now in f "%s" msg) in
  let msg' = Fmt.str
    "Something went wrong while handling the request. \
     A more specific error was logged at %a for inspection by the operators."
    Ptime.pp (Lazy.force now)
  in
  respond_with_page
    ~status:`Internal_Server_Error ~title:"Internal Server Error"
    [H.p [H.txt msg']]

let ( let@/? ) r f =
  (match r with
   | Ok x -> f x
   | Error (`Msg msg) -> respond_with_internal_server_error msg)

let ( let@*? ) m f =
  (match%lwt m with
   | Ok x -> f x
   | Error (`Msg msg) -> respond_with_internal_server_error msg)

let csrf_tag req =
  let token = Dream.csrf_token req in
  H.input ~a:[H.a_name "dream.csrf"; H.a_input_type `Hidden; H.a_value token] ()
