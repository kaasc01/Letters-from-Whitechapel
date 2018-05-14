all: board geometry graphicstest

board: Board.ml
	ocamlbuild -use-ocamlfind Board.byte

geometry: Geometry.ml
	ocamlbuild -use-ocamlfind Geometry.byte

graphicstest: GraphicsTest.ml
	ocamlbuild -use-ocamlfind GraphicsTest.byte

clean:
	rm -rf _build *.byte