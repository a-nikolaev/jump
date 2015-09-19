OC=ocamlopt
OL=ocamllex
OP=menhir --explain --infer 

all: jump

jump: base.cmx ast.cmx parser.cmx lexer.cmx eval.cmx main.ml
	$(OC) -o $@ $^

%.cmx: %.ml
	$(OC) -c $^

parser.ml: parser.mly ast.ml
	$(OP) $<
	$(OC) -c parser.mli

lexer.ml: lexer.mll ast.ml parser.mly 
	$(OL) $<

clean:
	-rm -f jump *.cmi *.cmx *.o parser.mli parser.conflicts parser.ml lexer.ml
