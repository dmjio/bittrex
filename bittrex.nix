{ mkDerivation, aeson, base, bytestring, http-client-tls, lens
, lens-aeson, scientific, SHA, split, stdenv, text, time, wreq
}:
mkDerivation {
  pname = "bittrex";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring http-client-tls lens lens-aeson scientific
    SHA split text time wreq
  ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/dmjio/bittrex";
  description = "API bindings to bittrex.com";
  license = stdenv.lib.licenses.bsd3;
}
