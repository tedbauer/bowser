# bowser

Toy browser implementation, following https://limpet.net/mbrubeck/2014/08/08/toy-layout-engine-1.html.

## Setup/run

Generate the OPAM file and install dependencies:
```
dune build @check
opam install . --deps-only
```

On MacOS, [install XQuartz](https://www.xquartz.org/).

Run the browser:
```
dune exec bowser
```

Press any key to close the window.

Build parser and see any conflicts:
```
menhir lib/html_parser.mly --explain
```

## References
- https://github.com/sampsyo/lang-start