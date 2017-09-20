all: byte native

.PHONY: all byte native install clean test
.SECONDARY:

byte: bN128Elements.cmo bN128Curve.cmo bN128Pairing.cmo
	ocamlfind c -a $^ -o bn128.cma -package zarith
native: bN128Elements.cmx bN128Curve.cmx bN128Pairing.cmx
	ocamlfind opt -a $^ -o bn128.cmxa -package zarith

install: all
	ocamlfind install bn128 META bn128.cma bn128.cmxa bn128.a *.cmx *.cmo *.cmi *.mli

test: bn128Test.cmx native
	ocamlfind opt -o test bN128Elements.cmx bN128Curve.cmx bN128Pairing.cmx bn128Test.cmx -package zarith -linkpkg
	./test

%.cmo: %.ml %.cmi
	ocamlfind c -c $< -package zarith

%.cmx: %.ml %.cmi 
	ocamlfind opt -c $< -package zarith

%.cmi: %.mli
	ocamlfind c -c $< -package zarith

clean:
	rm -f *.cmo *.cmx *.cmi *.cma *.cmxa *.a *.o test

bN128Curve.cmo bN128Curve.cmx: bN128Elements.cmi
bN128Pairing.cmo bN128Pairing.cmx: bN128Curve.cmi
bN128Test.cmx: bN128Pairing.cmi

