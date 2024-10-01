open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/inhca.byte";
]

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | After_rules ->
      flag ["ocaml"; "compile"] & S[A"-w"; A"+K-39"]
   | Before_options ->
      Options.make_links := false
   | _ -> ())
