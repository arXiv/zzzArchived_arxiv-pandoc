{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hpack, pandoc, stdenv, text }:
      mkDerivation {
        pname = "arxiv-pandoc";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base pandoc text ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base pandoc text ];
        testHaskellDepends = [ base pandoc text ];
        prePatch = "hpack";
        homepage = "https://github.com/githubuser/arxiv-pandoc#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
