# OCaml Toy

This repository contains a basic interpreter suitable as a starting point for a toy language.

It is derived from the Cornell University (I think) CS3110 lab described [here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html)
but has been heavily modified with inspiration from "Essentials of Programming Languages" by Friedman and Wand.  The
example code from the book can be found [here](https://github.com/mwand/eopl3).

The original files can be downloaded [here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec-code.zip).

Prerequisites:

- `opam init`
- `opam setup -a`
- `opam install menhir`
- `opam install linenoise`
- `opam install unix`
- `opam install dune`

To build, run `dune build` from the project's root directory.  To build and test, run `dune runtest`.

I also recommend:

- `opam install merlin utop ocp-indent tuareg`

## TODO

- Use `ounit2` instead of whatever is going on in `test/test.ml`
- Add the language features of your choice.

Tested with `ocamlc` 4.07.1.

## Helpful links
 
- [OCaml Cheat Sheets](https://ocaml.org/docs/cheat_sheets.html)
- [OUnit Documentation](http://ounit.forge.ocamlcore.org/api-ounit/index.html)
- [OCaml Stack Overflow](https://stackoverflow.com/questions/tagged/ocaml)i
- [Menhir reference manual](http://gallium.inria.fr/~fpottier/menhir/manual.pdf)
- [Lexing module documentation](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html)
- [opam packaging](https://opam.ocaml.org/doc/Packaging.html) (Describies files like `toy_lang.opam`)
- [Starting a new project with dune](https://medium.com/@bobbypriambodo/starting-an-ocaml-app-project-using-dune-d4f74e291de8)
- [Testing with dune](https://jbuilder.readthedocs.io/en/latest/tests.html)
