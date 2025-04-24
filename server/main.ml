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

let lwt_reporter ~write_log ?(write_app = write_log) () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> write_app (app_flush ())
      | _ -> write_log (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let routes ~vpaths () = [
  let static_dir = Config.(global.static_dir) in
  Dream.scope "" [] [
    Dream.get (Filename.concat vpaths.Vpaths.static "**")
      (Dream.static static_dir);
    Public.routes ~vpaths ();
    Admin.routes ~vpaths ();
  ]
]

let init = lazy begin
  let write_log = Lwt_io.write Lwt_io.stderr in
  let write_app = Lwt_io.write Lwt_io.stdout in
  Logs.set_reporter (lwt_reporter ~write_log ~write_app ());
  (* TODO: Setup log levels from configuration. *)
  Logs.set_level (Some Logs.Debug);
  Mirage_crypto_rng_unix.use_default ()
end

let serve () =
  Lazy.force init;
  let vpaths = Vpaths.create Config.(global.site_prefix) in
  Dream.run
    ~interface:Config.(global.listen_interface)
    ~port:Config.(global.listen_port)
    ?tls:Config.(global.tls_enabled)
    ?certificate_file:Config.(global.tls_certificate_file)
    ?key_file:Config.(global.tls_key_file)
    @@
  Dream.logger @@
  Dream.memory_sessions @@
  Dream.router (routes ~vpaths ())
