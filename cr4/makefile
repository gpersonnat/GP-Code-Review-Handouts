all: cr4 stack

cr4: cr4.ml
	ocamlbuild -use-ocamlfind cr4.byte

stack: stack.ml
	ocamlbuild -use-ocamlfind stack.byte

clean:
	rm -rf _build *.byte