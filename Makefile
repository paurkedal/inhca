.PHONY: test all doc install uninstall clean distclean

prefix = $(shell opam config var prefix)

OCAMLBUILD_PLUGINS = -plugin-tag 'package(ocamlbuild-eliom-dev)'
OCAMLBUILD = ocamlbuild -use-ocamlfind $(OCAMLBUILD_PLUGINS)

all:
	ocaml pkg/pkg.ml build

test:
	$(OCAMLBUILD) tests/testsuite.native
	_build/tests/testsuite.native

doc:
	OCAMLPATH=. topkg doc

clean:
	$(OCAMLBUILD) -clean

install:
	opam-installer --prefix $(prefix) inhca.install

uninstall:
	opam-installer --prefix $(prefix) -u inhca.install

distclean: clean
	rm -f inhca.install
