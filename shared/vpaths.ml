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

type t = {

  (* public *)
  static: string;
  acquire_login: string -> string;
  acquire: string;
  issued_pkcs12: string;
  cacert_pem: string;
  crl_pem: string;

  (* admin *)
  admin: string; (* GET *)
  admin_rpc_up: string;
  admin_rpc_down: string;
}

let ( ^/ ) = Filename.concat

let create root = {

  (* public *)
  static = root ^/ "static";
  acquire_login = (fun token -> root ^/ "acquire" ^/ token);
  acquire = root ^/ "acquire";
  issued_pkcs12 = root ^/ "acquire/issued-key-and-cert.p12";
  cacert_pem = root ^/ "cacert.pem";
  crl_pem = root ^/ "crl.pem";

  (* admin *)
  admin = root ^/ "admin";
  admin_rpc_up = root ^/ "admin/rpc/up";
  admin_rpc_down = root ^/ "admin/rpc/down";
}
