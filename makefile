all:
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/run_static.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/run_dynamic.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/run_except.ml
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/trans_static.ml 
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/trans_dynamic.ml 
	ocamlc -c -I +camlp4 -pp camlp4of.opt parser/trans_except.ml 
	mv parser/*.cm[io] .

clean:
	rm *.cmo *.cmi
