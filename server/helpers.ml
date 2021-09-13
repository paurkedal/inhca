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
open Opium
module H = Tyxml_html

let log_src = Logs.Src.create "inhca"
module Log = (val Logs_lwt.src_log log_src)

let respond_with_error ~status msg =
  let body = Body.of_string msg in
  let headers = Headers.of_list [
    "Content-Type", "text/plain";
  ] in
  Response.make ~status ~headers ~body ()

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
  let body = Body.of_string (Format.asprintf "%a" pp_html page) in
  let headers = Headers.of_list [
    "Content-Type", "text/html";
  ] in
  Response.make ?status ~headers ~body ()

let respond_with_message ?status ~title msg =
  respond_with_page ?status ~title [H.p [H.txt msg]]

let respond_with_internal_server_error msg =
  let now = lazy (Ptime_clock.now ()) in
  let+ () = Log.err (fun f -> let _ = Lazy.force now in f "%s" msg) in
  let msg' = Fmt.str
    "Something went wrong while handling the request. \
     A more specific error was logged at %a for inspection by the operators."
    Ptime.pp (Lazy.force now)
  in
  respond_with_page
    ~status:`Internal_server_error ~title:"Internal Server Error"
    [H.p [H.txt msg']]

let respond_with_json_ok value =
  Response.of_json
    (`Assoc [("status", `String "ok"); ("data", value)])

let respond_with_json_error msg =
  Response.of_json
    (`Assoc [("status", `String "error"); ("message", `String msg)])

let json_handler decode encode h req =
  let* req_body = Body.to_string req.Request.body in
  (match Yojson.Basic.from_string req_body with
   | exception Yojson.Json_error msg ->
      let msg = "Request is not valid JSON: " ^ msg in
      Lwt.return (respond_with_error ~status:`Bad_request msg)
   | input ->
      (match decode input with
       | Error msg ->
          let msg = "Cannot decode request: " ^ msg in
          Lwt.return (respond_with_error ~status:`Bad_request msg)
       | Ok input ->
          let+ output = h input in
          respond_with_json_ok (encode output)))

let ( let@/? ) r f =
  (match r with
   | Ok x -> f x
   | Error (`Msg msg) -> respond_with_internal_server_error msg)

let ( let@*? ) m f =
  (match%lwt m with
   | Ok x -> f x
   | Error (`Msg msg) -> respond_with_internal_server_error msg)
