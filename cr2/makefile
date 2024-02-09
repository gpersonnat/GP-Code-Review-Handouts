all: cr2 cr2_tests 

cr2: cr2.ml
	ocamlbuild -use-ocamlfind cr2.byte

cr2_tests: cr2_tests.ml
	ocamlbuild -use-ocamlfind cr2_tests.byte

clean:
	rm -rf _build *.byte