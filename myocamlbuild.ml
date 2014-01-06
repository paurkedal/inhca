(*# require ocamlbuild-eliom *)
(*# require oasis.base *)

open Ocamlbuild_plugin
open Ocamlbuild_eliom
open Ocamlbuild_ocsigen

let project_name = "inhca"
let server_packages = ["prime"; "prime.testing"; "config-file"]

let setup_data = lazy (BaseEnvLight.load ())
let setup_get k = BaseEnvLight.var_get k (Lazy.force setup_data)

let rec catfilename = function
  | [] -> "." | [x] -> x
  | x :: xs -> Filename.concat x (catfilename xs)

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true;
    Options.make_links := false

  | After_rules ->
    enable_eliom_rules ();
    Pathname.define_context "src/server" ["src"];
    Pathname.define_context "src/client" ["src"];
    flag ["ocaml"; "link"; "library"; "thread"] & A"-thread";
    enable_ocsigen_conf_rules ~server_packages ~server_subdir:"src/server" ()

  | e -> ()
end
