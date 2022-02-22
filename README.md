# Prime - OCaml Library Supplements

## Synopsis

This is a small OCaml library, mostly adding functionality to existing types
from the standard library.  The main focus is to amend the container
structures in a uniform way, keeping functions as composable as possible.

For details see the [API reference](http://paurkedal.github.io/ocaml-prime/).

## Installation

The library is available in a custom OPAM repository:

    opam repo add paurkedal https://github.com/paurkedal/opam-repo-paurkedal.git
    opam install prime

You can also install a development version with

    opam pin prime git+https://github.com/paurkedal/ocaml-prime.git

To build form sources, first install the dependencies listed in
[prime.opam](prime.opam).
