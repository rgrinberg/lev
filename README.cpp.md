# Lev - OCaml bindings to libev

## Abstract

[libev](libev) by Marc Lehmann is a minimal & portable event loop library. Lev
offers low level bindings to this library. The bindings are designed to be
minimal, low overhead, and easily embeddable in larger projects. The API is
callback based so you need to BYOC (bring your own concurrency).

## Example

```ocaml
#include "examples/readme.ml"
```

## Documentation

Lev's API is a thin wrapper around libev itself. So you should first and
foremost refer to libev's extensive [documentation](libevdoc). Lev itself will
document where it differs from libev's conventions.

## License

`src/` is offered under ISC (see src/LICENSE.md).
`vendor/` is under Marc Lehmann's original terms (see vendor/LICENSE).

[libev]: http://software.schmorp.de/pkg/libev.html
[libevdoc]: http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod
