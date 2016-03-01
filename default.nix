{ mkDerivation, base, bytestring, cprng-aes, crypto-random, ether
, exceptions, monad-network-class, mtl, network, newtype-generics
, stdenv, vstls, transformers
}:
mkDerivation {
  pname = "monad-network-instances";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cprng-aes crypto-random ether exceptions
    monad-network-class mtl network newtype-generics vstls transformers
  ];
  homepage = "http://github.com/vladsor/monad-network-instances";
  description = "Instances MonadConnection for Network & TLS";
  license = stdenv.lib.licenses.bsd3;
}
