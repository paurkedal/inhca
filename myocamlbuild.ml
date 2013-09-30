(*# require ocamlbuild-eliom *)
(*# require oasis.base *)

open Ocamlbuild_plugin
open Ocamlbuild_eliom
open Ocamlbuild_ocsigen

let project_name = "inhca"
let server_packages = ["prime"; "prime.testing"; "config-file"]

let setup_data = lazy (BaseEnvLight.load ())
let setup_get k = BaseEnvLight.var_get k (Lazy.force setup_data)
let test_params = Hashtbl.create 17
let production_params = Hashtbl.create 17

let rec catfilename = function
  | [] -> "." | [x] -> x
  | x :: xs -> Filename.concat x (catfilename xs)

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true;
    Options.make_links := false

  | After_options ->
    let localstatedir = setup_get "localstatedir" in
    let staticdir = catfilename [setup_get "datadir"; project_name; "static"] in
    let libdir = Filename.concat (setup_get "libdir") project_name in
    set_ocsigen_params ~project_name ~packages:server_packages
      ~libdir ~localstatedir ~staticdirs:[staticdir] production_params;
    set_ocsigen_test_params ~project_name ~packages:server_packages
      ~libdir:"_build/src/server" test_params

  | After_rules ->
    enable_eliom_rules ();
    Pathname.define_context "src/server" ["src"];
    Pathname.define_context "src/client" ["src"];
    flag ["ocaml"; "link"; "library"; "thread"] & A"-thread";

    ocsigen_conf_rule ~create_dirs:true ~params:test_params
                      ~prod:(project_name ^ "-test.conf")
                      ~dep:(project_name ^ ".conf.in") ();
    ocsigen_conf_rule production_params (project_name ^ ".conf") ()

  | e -> ()
end
