{ mkDerivation, aeson, amazonka, amazonka-s3, base, bytestring
, conduit, containers, cryptohash-sha256, cryptonite, directory
, file-embed, filepath, http-types, jwt, lib, lucid, network-uri
, scotty, sqlite-simple, string-interpolate, text, time, wai
, wai-extra, warp
}:
mkDerivation {
  pname = "todou";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-s3 base bytestring conduit containers
    cryptohash-sha256 cryptonite directory file-embed filepath
    http-types jwt lucid network-uri scotty sqlite-simple
    string-interpolate text time wai wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "todou";
}
