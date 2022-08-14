# bowser

Toy browser implementation, following https://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html.

## Setup/run

```
opam install base
opam install stdio
opam install menhir


dune build
dune exec bowser
```

Build parser and see any conflicts:
```
menhir lib/html_parser.mly --explain
```

## References
- https://github.com/sampsyo/lang-start