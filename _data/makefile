all: inference

inference: inference.ml
	ocamlbuild -use-ocamlfind inference.byte

clean:
	rm -rf _build *.byte