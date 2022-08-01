.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

zip:
	rm -f finalproject.zip
	zip -r finalproject.zip . -x@exclude.lst

clean:
	bisect-clean
	dune clean
	rm -f finalproject.zip

docs:
	dune build @doc
