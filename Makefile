# Modified from: https://github.com/gasche/manual-ocamlbuild/blob/master/examples/05-lex-yacc/Makefile
#
# TODO: create repl.ml which houses a REPL
#
# Pure OCaml, package from Opam, two directories
#
# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

# - we are using menhir, the modern replacement for OCamlYacc
# OCB_FLAGS = -use-ocamlfind             -I src -I lib # uses ocamlyacc

.PHONY: 	all clean byte native profile debug test sanity

OCB_FLAGS   = -use-menhir -I src -I lib # uses menhir
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) test.native
	$(OCB) repl.native

byte: sanity
	$(OCB) test.byte
	$(OCB) repl.byte

debug: sanity
	$(OCB) -tag debug test.debug.byte
	$(OCB) -tag debug repl.debug.byte

# check that menhir is installed, use "opam install menhir"
sanity:
	which menhir

test: native
	./test.native


