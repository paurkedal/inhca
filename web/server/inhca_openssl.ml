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

open Printf

exception Openssl_failed of Unix.process_status

let get_cadir () = Filename.concat (Ocsigen_config.get_datadir ()) "CA"
let get_capath fp = Filename.concat (get_cadir ()) fp
let get_tmpdir () = Filename.concat (Ocsigen_config.get_datadir ()) "tmp"
let get_tmppath fp = Filename.concat (get_tmpdir ()) fp
let () =
  if not (Sys.file_exists (get_tmpdir ())) then Unix.mkdir (get_tmpdir ()) 0o700

let openssl cmd args =
  let config = get_capath "openssl.cnf" in
  let argv = Array.of_list ("openssl" :: cmd :: "-config" :: config :: args) in
  match_lwt Lwt_process.exec ("openssl", argv) with
  | Unix.WEXITED 0 -> Lwt.return ()
  | st -> Lwt.fail (Openssl_failed st)

let save_spkac comps fp = Lwt_io.with_file Lwt_io.output fp
  begin fun oc ->
    let comp_counters = Hashtbl.create 8 in
    Lwt_list.iter_s
      begin fun (k, v) ->
	let i = try Hashtbl.find comp_counters k with Not_found -> 0 in
	Hashtbl.replace comp_counters k (i + 1);
	Lwt_io.fprintf oc "%d.%s=%s\n" i k v
      end
      comps
  end

let sign_spkac ?(days = 365) request_id comps =
  let workdir = get_tmppath request_id in
  if not (Sys.file_exists workdir) then Unix.mkdir workdir 0o700;
  let spkac_path = Filename.concat workdir "inhclient.spkac" in
  save_spkac comps spkac_path >>
  let cert_path = Filename.concat workdir "inhclient.pem" in
  openssl "ca" ["-days"; string_of_int days; "-notext"; "-batch";
	        "-spkac"; spkac_path; "-out"; cert_path] >>
  Lwt.return cert_path
