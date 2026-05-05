# Naive JSON with Functional Programming

A naive JSON (subset) implementation for the sake of learning ~~OCaml~~ Functional Programming.
Prepare to be horrified!

Much of the appeal of Rust is its type system, inspired by OCaml and friends.
Hey how about I learn a bit about my favorite programming language's favorite programming language.

What the subset do not include:

- Validation
- Floating point numbers
- Scientific notion
- Anything relating to the backslash character

If you are curious, JSON parsing is actually a nice programming exercise.
[Look how concise it is!](https://www.json.org/json-en.html)

## OCaml

Run using

```shell
cd naive-json-ocaml
dune build
dune exec naive-json
```

## Haskell

Installing a system `ghc` proved challenging, so I recommend using `ghcup` to install.

```shell
cd naive-json-haskell
cabal build
cabal run
```

