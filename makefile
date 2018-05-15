all: board geometry graphicstest writetest inference

board: Board.ml
	ocamlbuild -use-ocamlfind Board.byte

inference: Inference.ml
	ocamlbuild -use-ocamlfind Inference.byte

geometry: Geometry.ml
	ocamlbuild -use-ocamlfind Geometry.byte

graphicstest: GraphicsTest.ml
	ocamlbuild -use-ocamlfind GraphicsTest.byte

writetest: WriteTest.ml
	ocamlbuild -use-ocamlfind WriteTest.byte

clean:
	rm -rf _build *.byte