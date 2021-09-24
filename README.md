# ltlspec

## How to build and run

You need `stack` installed on your system. [This](https://docs.haskellstack.org/en/stable/README/) is an easy way to do so, but you can also check your package manager or use [ghcup](https://www.haskell.org/ghcup/).

VS Code has a great [plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) that will download and use the latest `haskell-language-server`.

The `Makefile` has useful targets - `make build`, `make test`, and `make docs` are the main ones.

If you want more utilities for linting and formatting, run `make deps`. Then you can run `make lint` or `make format`.
