# OCaml Toy

This repository contains a basic interpreter suitable as a starting point for a toy language.

It is derived from the Cornell University (I think) CS3110 lab described [here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html).  The original files can be downloaded [here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec-code.zip).

The following has been added:

- A `.merlin` file.
- A `Makefile` from this [ocamlbuild example](https://github.com/ocaml/ocamlbuild/tree/master/examples/05-lex-yacc), modified to also build `test.ml`.
- A `buildme` script which can be used to run `make` if processes spawned from your editor can't read the `opam` environment variables.
- The `main.ml` file was renamed to `interp.ml` because it does not appear to have been intended as the starting point of the interpreter.

To build, run `make` or `buildme`.  To test, build and then run `test.byte` or `test.native`.  To include debug info, run `make debug`.

Prerequisites: 

- `opam init`
- `opam setup -a`
- `opam install menhir`

I also recommend:

- `opam install merlin utop ocp-indent tuareg`

## TODO

- Use `ounit2` instead of whatever is going on in `test.ml`
- Add the language features of your choice.

Tested with `ocamlc` 4.07.0.

## Helpful links
 
- [OCaml Cheat Sheets](https://ocaml.org/docs/cheat_sheets.html)
- [OUnit Documentation](http://ounit.forge.ocamlcore.org/api-ounit/index.html)
- [OCaml Stack Overflow](https://stackoverflow.com/questions/tagged/ocaml)i


