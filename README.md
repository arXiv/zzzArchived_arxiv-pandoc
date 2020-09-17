# arxiv-pandoc

[Pandoc](https://pandoc.org/)-based executable for converting
LaTeX-based arXiv documents to text.


## Usage

`arxiv-pandoc-static` emits the extracted text to `stdout`.

If given no arguments, `arxiv-pandoc-static` assumes it is in a directory with
the extracted source files.

```
arxiv-pandoc-static > out.txt
```

However, the path to the extracted directory can also be given, e.g.

```
arxiv-pandoc-static /path/to/some_arXiv_doc > out.txt
```

### Export LANG

If you encounter an error along the lines of
`<stdout>: commitAndReleaseBuffer: invalid argument (invalid character)`,
you [may need to](https://github.com/simonmichael/hledger/blob/0751536d255cddc80513283f57f6c8f9f85f85d5/hledger/hledger.info#L914-L919)
set `LANG=C.UTF-8` before running, e.g.:

```
LANG=C.UTF-8 /path/to/arxiv-pandoc-static > out.txt
```

or just `export LANG=C.UTF-8` once.

## Building

### Docker

See the *Packaging* section below on how to get the appropriate docker
container running. Once int he container:

```
hpack
cabal build
```

### Nix

First, load the necessary build tools:

```
$ nix-shell -p haskellPackages.hpack haskellPackages.Cabal cabal2nix
```

Then, build the project

```
$ hpack
$ cabal2nix --shell . > package-shell.nix
$ nix-shell package-shell.nix
$ cabal build arxiv-pandoc-exe
```
We currently omit doing a full `cabal build` in Nix due to the
static build having problems there.

## Installing

E.g.

```
cabal install --overwrite-policy=always --installdir=$HOME/.local/bin
```

## Packaging

### Docker

Build the image if necessary:

```
docker build -t arxiv-pandoc .
```

(Set `DHUB_PREFIX` as below to use the locally built docker image.)

```
DHUB_PREFIX="" ./run-docker-env.sh bash
cabal clean
cabal install --install-method=copy --enable-executable-static --overwrite-policy=always --installdir=./bin
```

`/bin` should now contain two executables, one statically linked,
the other dynamically linked.

To avoid permission issues, run `cabal clean` again from the Docker
environment after the build has succeeded.

### packaging with nix-bundle (WIP)

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

### Packaging with static-haskell-nix (WIP)

I haven't tried [static-haskell-nix](https://github.com/nh2/static-haskell-nix) yet, partly as it looks like it
would require manual editing of the generated `package.nix` (though
at some point such support might be integrated).
