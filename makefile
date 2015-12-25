all:
	ocamlc -c -I +camlp4 -pp camlp4of.opt run.ml

run: all
	camlp4 run.cmo < test.fn

clean:
	'rm' *.cmo *.cmi
