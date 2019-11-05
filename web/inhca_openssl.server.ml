(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let string_of_command command =
  let quoted s =
    if String.length s > 0 && s.[0] = '-' then s else
    "\"" ^ String.escaped s ^ "\"" in
  String.concat " " (List.map quoted (Array.to_list (snd command)))

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
   >>= fun () -> Lwt_list.iter_s (Log.error_f "Stderr: %s") msgs

let get_cadir () = Filename.concat (Ocsigen_config.get_datadir ()) "CA"
let get_capath fp = Filename.concat (get_cadir ()) fp
let get_newcertpath i =
  Filename.concat (get_capath "newcerts") (sprintf "%02X.pem" i)
let get_tmpdir () = Filename.concat (Ocsigen_config.get_datadir ()) "tmp"
let get_tmppath fp = Filename.concat (get_tmpdir ()) fp
let () =
  if not (Sys.file_exists (get_tmpdir ())) then Unix.mkdir (get_tmpdir ()) 0o700

let mk_tmpdir token =
  let dir = get_tmppath token in
  (if Sys.file_exists dir then Lwt.return_unit else Lwt_unix.mkdir dir 0o700)
    >>= fun () ->
  Lwt.return dir

let option_of_string f = function "" -> None | s -> Some (f s)

let time_of_string s =
  let field i = int_of_string (String.sub s (2 * i) 2) in
  (match String.length s with
   | 13 when s.[12] = 'Z' ->
      (match let date = (2000 + field 0, field 1, field 2) in
             let time = (field 3, field 4, field 5) in
             Ptime.of_date_time (date, (time, 0))
       with
       | Some t -> t
       | None -> invalid_arg "Invalid time in index.txt"
       | exception Failure _ -> invalid_arg "Invalid time in index.txt")
   | _ -> invalid_arg "Invalid time in index.txt")

module Issue = struct

  type state = [`Revoked | `Expired | `Valid]

  type t = {
    state : state;
    expired : Ptime.t;
    revoked : Ptime.t option;
    serial : int;
    dn : string;
  }

  let state ici = ici.state
  let expired ici = ici.expired
  let revoked ici = ici.revoked
  let serial ici = ici.serial
  let dn ici = ici.dn

  let string_of_state = function
   | `Valid -> "valid"
   | `Revoked -> "revoked"
   | `Expired -> "expired"

  let state_of_string = function
   | "V" -> `Valid
   | "R" -> `Revoked
   | "E" -> `Expired
   | _ -> invalid_arg "Invalid state in index.txt."

  let parse_index_line s =
    match Prime_string.chop_affix "\t" s with
     | [s_state; s_expired; s_revoked; s_serial; _; s_dn] ->
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
  let argv = Array.of_list ("openssl" :: cmd :: args) in
  ("openssl", argv)

let read_lines ic =
  let rec loop lines =
    match%lwt Lwt_io.read_line_opt ic with
     | None -> Lwt.return (List.rev lines)
     | Some line -> loop (line :: lines) in
    loop []

let exec_openssl subcommand args =
  let command = openssl_command subcommand args in
  Log.debug_f "Exec: openssl %s" (string_of_command command) >>= fun () ->
  match%lwt
    Lwt_process.with_process_full command @@ fun proc ->
      Lwt_io.close proc#stdout >>= fun () ->
      let%lwt stderr = read_lines proc#stderr
          and status = proc#status in
      Lwt.return (status, stderr)
  with
   | Unix.WEXITED 0, stderr ->
      Lwt_list.iter_s Log.info stderr >>= fun () ->
      Lwt.return (Ok ())
   | pst, stderr ->
      Lwt.return (Error (command, pst, stderr))

let pread_openssl subcommand args =
  let command = openssl_command subcommand args in
  Log.debug_f "Exec: openssl %s" (string_of_command command) >>= fun () ->
  match%lwt
    Lwt_process.with_process_full command @@ fun proc ->
      let%lwt stdout = Lwt_io.read proc#stdout
          and stderr = read_lines proc#stderr
          and status = proc#status in
      Lwt.return (status, stdout, stderr)
  with
   | Unix.WEXITED 0, stdout, stderr ->
      Lwt_list.iter_s Log.info stderr >>= fun () ->
      Lwt.return (Ok stdout)
   | pst, _, stderr ->
      Lwt.return (Error (command, pst, stderr))

let pmap_openssl subcommand args input =
  let command = openssl_command subcommand args in
  Log.debug_f "Exec: openssl %s" (string_of_command command) >>= fun () ->
  match%lwt
    Lwt_process.with_process_full command @@ fun proc ->
      let%lwt () =
          Lwt_io.write proc#stdin input >>= fun () -> Lwt_io.close proc#stdin
        and stdout = Lwt_io.read proc#stdout
        and stderr = read_lines proc#stderr
        and status = proc#status in
      Lwt.return (status, stdout, stderr)
  with
   | Unix.WEXITED 0, stdout, stderr ->
      Lwt_list.iter_s Log.info stderr >>= fun () ->
      Lwt.return (Ok stdout)
   | pst, _, stderr ->
      Lwt.return (Error (command, pst, stderr))


(* CA Commands *)

let cacert_path = get_capath "cacert.pem"

let gencrl () =
  let config = get_capath "openssl.cnf" in
  pread_openssl "ca" ["-config"; config; "-gencrl"]

let exec_openssl_ca args =
  let config = get_capath "openssl.cnf" in
  exec_openssl "ca" ("-config" :: config :: args)

let pread_openssl_ca args =
  let config = get_capath "openssl.cnf" in
  pread_openssl "ca" ("-config" :: config :: args)

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

let sign_spkac ?(days = 365) ~token comps =
  let%lwt workdir = mk_tmpdir token in
  let spkac_path = Filename.concat workdir "inhclient.spkac" in
  save_spkac comps spkac_path >>= fun () ->
  let cert_path = Filename.concat workdir "inhclient.pem" in
  match%lwt
    exec_openssl_ca
      ["-days"; string_of_int days; "-notext"; "-batch";
       "-spkac"; spkac_path; "-out"; cert_path]
  with
   | Ok () -> Lwt.return (Ok cert_path)
   | Error error -> Lwt.return (Error error)

let revoke_serial serial = exec_openssl_ca ["-revoke"; get_newcertpath serial]
let updatedb () = exec_openssl_ca ["-updatedb"]

let sign_pem ?(days = 365) ~token csr =
  let%lwt workdir = mk_tmpdir token in
  let csr_path = Filename.concat workdir "inhclient.csr" in
  save_file csr csr_path >>= fun () ->
  pread_openssl_ca
    ["-days"; string_of_int days; "-notext"; "-batch";
     "-in"; csr_path]


(* PKCS12 Commands *)

let default_pkcs12_name = "Key and Certificate from the Inhca Web CA"

let export_pkcs12 ?(name = default_pkcs12_name) ~password ~cert ~certkey () =
  let pwfd_in, pwfd_out = Lwt_unix.pipe_out () in
  let pwarg = sprintf "fd:%d" (Fd_send_recv.int_of_fd pwfd_in) in
  let%lwt () =
    Lwt_unix.write_string pwfd_out password 0 (String.length password)
      >>= fun _ ->
    Lwt_unix.write_string pwfd_out "\n" 0 1 >>= fun _ ->
    Lwt_unix.close pwfd_out
  and result =
    pmap_openssl "pkcs12"
      ["-export"; "-name"; name; "-passout"; pwarg] (certkey ^ cert) in
  Lwt.return result
