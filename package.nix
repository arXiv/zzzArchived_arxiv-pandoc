{ mkDerivation, base, bytestring, directory, exceptions, hpack
, pandoc, path, path-io, stdenv, text, turtle
}:
mkDerivation {
  pname = "arxiv-pandoc";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory exceptions pandoc path path-io text
    turtle
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring directory exceptions pandoc path path-io text
    turtle
  ];
  testHaskellDepends = [
    base bytestring directory exceptions pandoc path path-io text
    turtle
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/arxiv-pandoc#readme";
  license = stdenv.lib.licenses.bsd3;
}
