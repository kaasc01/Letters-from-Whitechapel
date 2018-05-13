all: board geometry

board: Board.ml
	ocamlbuild -use-ocamlfind Board.byte

geometry: Geometry.ml
	ocamlbuild -use-ocamlfind Geometry.byte

clean:
	rm -rf _build *.byte