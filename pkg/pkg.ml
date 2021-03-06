#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = [Pkg.std_file "COPYING"]
let opams = [Pkg.opam_file "inhca.opam"]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        %% on (Conf.debug c) (of_list ["-tag"; "debug"])
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe ~licenses ~opams ~build "inhca" @@ fun c ->
  Ok [
    Pkg.mllib ~api:[] "web/server/inhca.mllib";
    Pkg.share ~dst:"static/" "web/static/inhca.css";
    Pkg.share ~dst:"static/" "web/client/inhca.js";
  ]
