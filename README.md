# zlist: Lazy lists for OCaml

%%VERSION%%

`zlist` consists of the definition of a lazy list type and a number of useful functions for manipulating and constructing lazy lists.

## About

`master`: [![build status](https://gitlab.com/jhaberku/Zlist/badges/master/build.svg)](https://gitlab.com/jhaberku/Zlist/commits/master)

`develop`: [![build status](https://gitlab.com/jhaberku/Zlist/badges/develop/build.svg)](https://gitlab.com/jhaberku/Zlist/commits/develop)

Development is hosted at [GitLab](https://gitlab.com/jhaberku/Zlist) and mirrored at [GitHub](https://github.com/hakuch/Zlist).

API documentation can be found [online](http://jhaberku.gitlab.io/Zlist/zlist/Zlist).

## Inspiration

This implementation is heavily inspired by "Functional Programming in Scala", by Chiusano and Bjarnason (2014).

## Installing

The easiest way to install `zlist` is through the `opam` repository:

```bash
$ opam install zlist
```

## Building

Alternatively, you can build and install `zlist` from its sources.

First, pin the sources using `opam`:

```bash
$ opam pin add -n zlist <TOP-LEVEL SOURCE DIRECTORY>
$ opam install zlist -t -d --deps-only
```

then build the package:

```bash
$ dune build
```

After the package is built, `zlist`'s test suite is run by invoking

```bash
$ dune runtest
```

## License

`zlist` is copyright 2016 by Jesse Haber-Kucharsky.

`zlist` is released under the terms of the Apache license, version 2.0. See
`/LICENSE` for more information.
