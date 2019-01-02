# OCaml Toy Language

This repository contains a basic interpreter suitable as a starting point for a 
toy language that has a syntax that is similar to OCaml and is dynamically 
typed.

It is originally derived from the Cornell University (I think) CS3110 lab 
described 
[here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec.html)
but has been heavily modified with inspiration from "Essentials of Programming 
Languages" by Friedman and Wand.  The example code from the book can be found 
[here](https://github.com/mwand/eopl3).

The original files for CS3110 can be downloaded 
[here](https://www.cs.cornell.edu/courses/cs3110/2015fa/l/12-interp/rec-code.zip).

## Non-Goals

Being performant. This toy language exists to be a test bed to experiment and 
learn about programming language implementation. To that
end, readability and matinatinability are more important than just about
anything else.  Performance is not a consideration except in the most 
egregious situations.

## Features

- Supported data types:
  - int
  - bool
  - functions
- Comments:
  - Single-line: `//`
  - Multi-line: `(* ... *)`
- `if`/`then`/`else`
- Variables (`let`)
- Operators: `+`, `-`, `*`, `/`, `%`, `=`
- Functions & closures
- Recursion (`let rec`)
- A REPL

### Examples

A recursive factorial function:

```
(* Introduce a recursive let binding having type `(int -> int)` (that is, a 
   function that accepts an integer as its single argument and returns an 
   integer.)
let rec factorial: (int -> int) = 
  (* Return the value of a function constructor with the same type as the 
     let binding. *)
  func(n: int -> int) -> 
    (* Calculate the factorial of n. *)
    if n = 0 then 1 else n * factorial(n - 1) in 
  (* invokes the function *)
  factorial(8)
```

There is a fair amount of repetition between the data type of the let binding 
and the function signature--this is temporary and will be resolved by letting
the type of the let binding become the same type as the value being assigned.

See [the tests](./test/test.ml) for more examples.

## Building 

Prerequisites:

- `ocamlc` 4.07.1 (not tested with earlier versions)
- `opam init`
- `opam setup -a`
- `opam install menhir linenoise ounit unix dune`

To build, run `dune build` from the project's root directory.  To build and test, run `dune runtest`.

I also recommend:

- `opam install merlin utop ocp-indent tuareg`

## REPL

After your build is working, you can execute `dune exec bin/repl.exe` from the project's root directory to
start the REPL:

```
? dune exec bin/repl.exe
OCAML toy language REPL - type "?exit" to exit
> let foo = func x -> x(1) in foo(func y -> y + 1)
2
> ?exit
```

## TODO

- Mutually recursive functions, i.e. `let rec x = ... and y = xxx`
- A foreign function interface to OCaml
- Add the language features of your choice.

## Reference Material

### Working With Menhir 

If after modifying `parser.mly` a message appears about shift/reduce conflicts, it can be hard to diagnose
what's going on.  To get `menhir` to tell you about the shift/reduce conflicts, execute:

```
menhir --explain lib/parser.mly
``

Then, you can examine the file `lib/parser.conflicts` to read about the first shift/reduce conflict detected.

### Helpful links

- [Spacemacs OCaml Layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/ocaml)
- [OCaml Cheat Sheets](https://ocaml.org/docs/cheat_sheets.html)
- [OCaml Stack Overflow](https://stackoverflow.com/questions/tagged/ocaml)
- [OUnit Documentation](http://ounit.forge.ocamlcore.org/api-ounit/index.html)
- [Parsing with ocamllex and Menhir](https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html) (Real World OCaml)
- [Menhir reference manual](http://gallium.inria.fr/~fpottier/menhir/manual.pdf)
- [Lexing module documentation](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html)
- [opam packaging](https://opam.ocaml.org/doc/Packaging.html) (Describies files like `toy_lang.opam`)
- [Starting a new project with dune](https://medium.com/@bobbypriambodo/starting-an-ocaml-app-project-using-dune-d4f74e291de8)
- [Testing with dune](https://jbuilder.readthedocs.io/en/latest/tests.html)

