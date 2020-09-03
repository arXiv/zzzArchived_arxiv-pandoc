# arxiv-pandoc
Pandoc-based library and Python API wrappers for extracting text from documents (and maybe, performing doc conversions)

## Building

First, load the necessary build tools:

```
$ nix-shell -p haskellPackages.hpack haskellPackages.Cabal cabal2nix
```

Then, build the project

```
$ hpack
$ cabal2nix --shell . > package-shell.nix
$ nix-shell package-shell.nix
$ cabal build
```

## Installing

E.g.

```
cabal install --overwrite-policy=always --installdir=$HOME/.local/bin
```

## Packaging

## Docker

Build the image if necessary:

```
docker build -t arxiv-pandoc .
```

```
DHUB_PREFIX="" ./run-docker-env.sh bash
```

## packaging with nix-bundle (WIP)

In addition to the build instructions:

Add `nix-bundle` to the initial list of packages to load.

```
cabal2nix . > package.nix
```

Test the build of the package if desired using `nix-build` directly:

```
nix-build -E "with import <nixpkgs> {}; pkgs.haskell.packages.ghc865.callPackage ./package.nix {}"
```

You should be able to see where the binary is located in the filesystem
now, e.g.:

```
/nix/store/2sk6g3qgd7zmii11jl7gaw09c9y17cmv-arxiv-pandoc-0.1.0.0/
```

The next step [currently fails](https://github.com/matthewbauer/nix-bundle/issues/55 ):

```
$ nix-bundle '(pkgs.haskell.packages.ghc865.callPackage ./package.nix {})' /bin/arxiv-pandoc-exe
```

## Packaging with static-haskell-nix (WIP)

I haven't tried [static-haskell-nix](https://github.com/nh2/static-haskell-nix) yet, partly as it looks like it
would require manual editing of the generated `package.nix` (though
at some point such support might be integrated).
