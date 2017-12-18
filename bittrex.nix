{ mkDerivation, aeson, base, bytestring, cryptohash-sha512
, http-client-tls, stdenv, text, time, wreq
}:
mkDerivation {
  pname = "bittrex";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring cryptohash-sha512 http-client-tls text time
    wreq
  ];
  homepage = "https://github.com/dmjio/bittrex";
  description = "API bindings to bittrex.com";
  license = stdenv.lib.licenses.bsd3;
}
