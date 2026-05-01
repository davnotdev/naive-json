# Naive JSON with Functional Programming

A naive JSON (subset) implementation for the sake of learning ~~OCaml~~ Function Programming.
Prepare to be horrified!

I realize that the much of the appeal of rust is the type system, inspired by OCaml and friends.
Hey how about I learn a bit about my favorite programming language's favorite programming language.

What the subset do not include:

- [ ] Validation
- [ ] Floating point numbers
- [ ] Scientific notion
- [ ] Anything relating to backslash

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

```shell
cd naive-json-haskell
stack build
stack run
```

