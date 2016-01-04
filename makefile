all:
	ocamlc -c -I +camlp4 -pp camlp4of.opt run.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt trans.ml 

clean:
	rm *.cmo *.cmi
