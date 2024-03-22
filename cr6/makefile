all: cr6 cr6_tests 

cr6: cr6.ml
	ocamlbuild -use-ocamlfind cr6.byte

cr6_tests: cr6_tests.ml
	ocamlbuild -use-ocamlfind cr6_tests.byte

clean:
	rm -rf _build *.byte