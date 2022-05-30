# bowser

Toy browser implementation, following https://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html.

## Setup/run

```
opam install base
opam install stdio
opam install menhir


dune build bowser.exe
cat webpages/page_simple.html | dune exec bin/bowser.exe
```

## References
- https://github.com/sampsyo/lang-start