{ mkDerivation, aeson, base, base16-bytestring, bytestring
, http-client-tls, lens, lens-aeson, SHA, split, stdenv, text, time
, wreq
}:
mkDerivation {
  pname = "bittrex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring http-client-tls lens
    lens-aeson SHA split text time wreq
  ];
  homepage = "https://github.com/dmjio/bittrex";
  description = "API bindings to bittrex.com";
  license = stdenv.lib.licenses.bsd3;
}
