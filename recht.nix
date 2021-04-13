{ mkDerivation, async, base, binary, blessings, bytestring
, data-default, directory, filepath, megaparsec
, optparse-applicative, pandoc, random, safe, scalpel, stdenv, text
, time
}:
mkDerivation {
  pname = "recht";
  version = "0.3.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base binary blessings bytestring data-default directory
    filepath megaparsec optparse-applicative pandoc random safe scalpel
    text time
  ];
  license = stdenv.lib.licenses.mit;
}
