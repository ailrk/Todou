{ mkDerivation, base, bytestring, effectful, lib, log-effectful
, sqlite-simple, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "todou";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring effectful log-effectful sqlite-simple text wai
    wai-extra warp
  ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "todou";
}
