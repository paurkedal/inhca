open Printf

exception Openssl_failed of Unix.process_status

let get_cadir () = Filename.concat (Ocsigen_config.get_datadir ()) "CA"
let get_capath fp = Filename.concat (get_cadir ()) fp

let with_out f fp =
  let oc = open_out fp in
  (try f oc with xc -> close_out oc; raise xc);
  close_out oc

let openssl cmd args =
  let config = get_capath "openssl.cnf" in
  let argv = Array.of_list ("openssl" :: cmd :: "-config" :: config :: args) in
  match_lwt Lwt_process.exec ("openssl", argv) with
  | Unix.WEXITED 0 -> Lwt.return ()
  | st -> Lwt.fail (Openssl_failed st)

let sign_spkac ?(days = 365) request_id comps =
  let workdir = Filename.concat (Ocsigen_config.get_datadir ()) request_id in
  if not (Sys.file_exists workdir) then Unix.mkdir workdir 0o700;
  let spkac_path = Filename.concat workdir "inhclient.spkac" in
  with_out (fun spkac_out ->
	      List.iter (fun (k, v) -> fprintf spkac_out "%s=%s\n" k v) comps)
	   spkac_path;
  let cert_path = Filename.concat workdir "inhclient.crt" in
  openssl "ca" ["-days"; string_of_int days; "-notext"; "-batch";
	        "-spkac"; spkac_path; "-out"; cert_path] >>
  Lwt.return cert_path
