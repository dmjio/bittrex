{ mkDerivation, aeson, base, bytestring, http-client-tls, lens
, lens-aeson, scientific, SHA, split, stdenv, text, time, wreq
}:
mkDerivation {
  pname = "bittrex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client-tls lens lens-aeson scientific
    SHA split text time wreq
  ];
  homepage = "https://github.com/dmjio/bittrex";
  description = "API bindings to bittrex.com";
  license = stdenv.lib.licenses.bsd3;
}
