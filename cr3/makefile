all: cr3 cr3_tests 

cr3: cr3.ml
	ocamlbuild -use-ocamlfind cr3.byte

cr3_tests: cr3_tests.ml
	ocamlbuild -use-ocamlfind cr3_tests.byte

clean:
	rm -rf _build *.byte