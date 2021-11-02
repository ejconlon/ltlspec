# ltlspec

Exploring runtime verification of distributed systems with LTL

## How to build and run

This repo is setup so you never have to `cd` out of the root directory.

To run the Haskell programs, you need `stack` installed on your system. [This](https://docs.haskellstack.org/en/stable/README/) is an easy way to do so, but you can also check your package manager or use [ghcup](https://www.haskell.org/ghcup/).

Most of the interesting stuff is going to be run in the test suite. Run it with `make test`. `stack` will get the appropriate Haskell compiler and package dependencies, and it will compile the project before running the test suite.

If you have Docker installed and running (with about 4G of RAM allocated to it), you can run `make docker-test` to build and run tests in a container instead of installing `stack` locally.

## Development

VS Code has a great [plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) that will download and use the latest `haskell-language-server`.

The `Makefile` has useful targets - `make build`, `make test`, and `make docs` are the main ones.

If you want more utilities for linting and formatting, run `make deps`. Then you can run `make lint` or `make format`.
