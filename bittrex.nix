{ mkDerivation, aeson, base, bytestring, flow, http-client-tls
, lens, lens-aeson, scientific, SHA, split, stdenv, text, time
, turtle, wreq
}:
mkDerivation {
  pname = "bittrex";
  version = "0.6.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring flow http-client-tls lens lens-aeson
    scientific SHA split text time wreq
  ];
  executableHaskellDepends = [ base text turtle ];
  homepage = "https://github.com/dmjio/bittrex";
  description = "Bindings for the Bittrex API";
  license = stdenv.lib.licenses.bsd3;
}
