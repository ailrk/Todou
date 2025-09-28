{ mkDerivation, aeson, amazonka, amazonka-s3, async, base
, bytestring, conduit, containers, directory, file-embed, filepath
, http-types, lib, lucid, network-uri, scotty, sqlite-simple
, string-interpolate, text, time, wai, wai-extra, warp
}:
mkDerivation {
  pname = "todou";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-s3 async base bytestring conduit containers
    directory file-embed filepath http-types lucid network-uri scotty
    sqlite-simple string-interpolate text time wai wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "todou";
}
