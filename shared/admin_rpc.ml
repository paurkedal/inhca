(* Copyright (C) 2022--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

let err_p = Idl.DefaultError.err
let unit_p = Idl.Param.mk Rpc.Types.unit
let int_p = Idl.Param.mk Rpc.Types.int
let cn_p = Idl.Param.mk Rpc.Types.string
let email_p = Idl.Param.mk Rpc.Types.string
let enrollment_p = Idl.Param.mk Enrollment_core.t
let enrollment_list_p = Idl.Param.mk Enrollment_core.t_list

module Make (R : Idl.RPC) = struct
  open R

  let list_enrollments = declare "list_enrollments"
    ["List enrollments."]
    (unit_p @-> returning enrollment_list_p err_p)

  let add_enrollment = declare "add_enrollment"
    ["Create an enrollment."]
    (cn_p @-> email_p @-> returning unit_p err_p)

  let delete_enrollment = declare "delete_enrollment"
    ["Delete an enrollement."]
    (enrollment_p @-> returning unit_p err_p)

  let updatedb = declare "updatedb"
    ["Update expiration status in the OpenSSL index."]
    (unit_p @-> returning unit_p err_p)

  let revoke_serial = declare "revoke_serial"
    ["Revoke the certificate with the given serial number."]
    (int_p @-> returning unit_p err_p)

  let implementation = implement {
    Idl.Interface.name = "Inhca_admin";
    namespace = None;
    description = ["InhCA Administrative Web Protocol"];
    version = (1, 0, 0);
  }
end

(*
let admin_update = Server_function.create [%of_yojson: update] [%to_yojson: unit]
*)
