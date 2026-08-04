OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex
OCAMLFLAGS = -cclib -lstr
SOURCES = ast.ml grammar.mly lexer.mll token.ml typecheck.ml interpreter.ml typecheck_tests.ml main.ml

all: main.native

clean:
	rm -f *.cmi *.cmo *.cmx *.o parser.ml parser.mli lexer.ml grammar.ml grammar.mli interpreter.cmi interpreter.cmo
	rm -f main.native

grammar.ml grammar.mli: grammar.mly
	$(OCAMLYACC) grammar.mly

lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

grammar.cmo: grammar.ml grammar.mli ast.cmo
	$(OCAMLC) -c grammar.mli
	$(OCAMLC) -c grammar.ml

token.cmo: token.ml grammar.cmi ast.cmo
	$(OCAMLC) -c token.ml

lexer.cmo: lexer.ml grammar.cmi ast.cmo
	$(OCAMLC) -c lexer.ml

typecheck.cmo: typecheck.ml ast.cmo
	$(OCAMLC) -c typecheck.ml

interpreter.cmo: interpreter.ml ast.cmo
	$(OCAMLC) -c interpreter.ml

typecheck_tests.cmo: typecheck_tests.ml ast.cmo typecheck.cmo
	$(OCAMLC) -c typecheck_tests.ml

main.cmo: main.ml ast.cmo grammar.cmo lexer.cmo token.cmo typecheck.cmo interpreter.cmo
	$(OCAMLC) -c main.ml

main.native: ast.cmo grammar.cmo token.cmo lexer.cmo typecheck.cmo interpreter.cmo main.cmo
	$(OCAMLC) -o main.native $^

tests.native: ast.cmo typecheck.cmo typecheck_tests.cmo
	$(OCAMLC) -o tests.native ast.cmo typecheck.cmo typecheck_tests.cmo

.PHONY: all clean test

test: tests.native
	./tests.native