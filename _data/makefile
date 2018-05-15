all: board geometry graphicstest writetest inference

inference: Inference.ml
	ocamlbuild -use-ocamlfind Inference.byte

clean:
	rm -rf _build *.byte