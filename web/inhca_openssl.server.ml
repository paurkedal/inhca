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

open Lwt.Infix
open Printf

module Log =
  Inhca_tools.Local_log (struct let section_name = "inhca:openssl" end)

module Time = CalendarLib.Calendar
module Time_format = CalendarLib.Printer.Calendar

let string_of_command command =
  String.concat " " (Array.to_list (snd command))

type error = Lwt_process.command * Unix.process_status * string list

let log_error (command, pst, msgs) =
  (match pst with
   | Unix.WEXITED i ->
      Log.error_f "Command exited with %d: %s." i (string_of_command command)
   | Unix.WSIGNALED i ->
      Log.error_f "Command terminated by signal %d: %s"
                  i (string_of_command command)
   | Unix.WSTOPPED i ->
      Log.error_f "Command stopped by signal %d: %s"
                  i (string_of_command command))
   >> Lwt_list.iter_s (Log.error_f "Stderr: %s") msgs

let get_cadir () = Filename.concat (Ocsigen_config.get_datadir ()) "CA"
let get_capath fp = Filename.concat (get_cadir ()) fp
let get_tmpdir () = Filename.concat (Ocsigen_config.get_datadir ()) "tmp"
let get_tmppath fp = Filename.concat (get_tmpdir ()) fp
let () =
  if not (Sys.file_exists (get_tmpdir ())) then Unix.mkdir (get_tmpdir ()) 0o700

let option_of_string f = function "" -> None | s -> Some (f s)

let time_of_string s =
  let s = "20" ^ s in (* Two-digit years! *)
  let n = String.length s in
  match n, s.[n - 1] with
  | 15, 'Z' -> Time_format.from_fstring "%Y%m%d%H%M%SZ" s
  | 17, _   -> Time_format.from_fstring "%Y%m%d%H%M%S%z" s
  | 13, 'Z' -> Time_format.from_fstring "%Y%m%d%H%MZ" s
  | 15, _   -> Time_format.from_fstring "%Y%m%d%H%M%z" s
  | _ -> invalid_arg "Invalid time in index.txt."

module Issue = struct

  type state = [`Revoked | `Expired | `Valid]

  type t = {
    state : state;
    expired : Time.t;
    revoked : Time.t option;
    serial : int;
    dn : string;
  }

  let state ici = ici.state
  let expired ici = ici.expired
  let revoked ici = ici.revoked
  let serial ici = ici.serial
  let dn ici = ici.dn

  let state_of_string = function
   | "V" -> `Valid
   | "R" -> `Revoked
   | "E" -> `Expired
   | _ -> invalid_arg "Invalid state in index.txt."

  let parse_index_line s =
    match Prime_string.chop_affix "\t" s with
     | [s_state; s_expired; s_revoked; s_serial; s_dn] ->
        Lwt.wrap @@ fun () -> {
          state = state_of_string s_state;
          expired = time_of_string s_expired;
          revoked = option_of_string time_of_string s_revoked;
          serial = int_of_string ("0x" ^ s_serial);
          dn = s_dn;
        }
     | _ ->
        ksprintf Lwt.fail_with "Wrong number of fields in index.txt."

  let load_all () =
    let fp = get_capath "index.txt" in
    Lwt_io.lines_of_file fp |> Lwt_stream.map_s parse_index_line
end

(* Process Helpers *)

let save_file content fp =
  Lwt_io.with_file Lwt_io.output fp (fun oc -> Lwt_io.write oc content)

let openssl_command cmd args =
  let config = get_capath "openssl.cnf" in
  let argv = Array.of_list ("openssl" :: cmd :: "-config" :: config :: args) in
  ("openssl", argv)

let read_lines ic =
  let rec loop lines =
    match%lwt Lwt_io.read_line_opt ic with
     | None -> Lwt.return (List.rev lines)
     | Some line -> loop (line :: lines) in
    loop []

let exec_openssl subcommand args =
  let command = openssl_command subcommand args in
  Log.debug_f "Exec: openssl %s" (string_of_command command) >>
  match%lwt
    Lwt_process.with_process_full command @@ fun proc ->
      Lwt_io.close proc#stdout >>
      let%lwt stderr = read_lines proc#stderr
          and status = proc#status in
      Lwt.return (status, stderr)
  with
   | Unix.WEXITED 0, stderr ->
      Lwt_list.iter_s Log.info stderr >>
      Lwt.return (Ok ())
   | pst, stderr ->
      Lwt.return (Error (command, pst, stderr))

(* CA Interface *)

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

let sign_spkac ?(days = 365) ~request_id comps =
  let workdir = get_tmppath request_id in
  if not (Sys.file_exists workdir) then Unix.mkdir workdir 0o700;
  let spkac_path = Filename.concat workdir "inhclient.spkac" in
  save_spkac comps spkac_path >>
  let cert_path = Filename.concat workdir "inhclient.pem" in
  match%lwt
    exec_openssl "ca"
      ["-days"; string_of_int days; "-notext"; "-batch";
       "-spkac"; spkac_path; "-out"; cert_path]
  with
   | Ok () -> Lwt.return (Ok cert_path)
   | Error error -> Lwt.return (Error error)

