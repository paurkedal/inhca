#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = [Pkg.std_file "COPYING"]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir
        %% on (Conf.debug c) (of_list ["-tag"; "debug"])
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe ~licenses ~build "inhca" @@ fun c ->
  Ok [
    Pkg.mllib ~api:[] "web/server/inhca.mllib";
    Pkg.share ~dst:"static/css/" "web/static/css/inhca.css";
    Pkg.share ~dst:"static/" "web/client/admin.js";
  ]
