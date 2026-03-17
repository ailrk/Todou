{
  description = "todou";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/6c5e707c6b5339359a9a9e215c5e66d6d802fd7a";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {

              todou = (hfinal.callPackage ./default.nix {}).overrideAttrs (old: {
                nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ final.upx ];

                # Run upx after install
                postInstall = ''
                  echo "Running UPX on: $out/bin/todou"
                  upx --best --lzma $out/bin/todou || true
                '';
              });
            };
        };

        todou =
          with final.haskell.lib.compose;
          overrideCabal (drv: {
            disallowGhcReference = false;
            enableSeparateDataOutput = false;
            configureFlags = drv.configureFlags or [] ++ [
              "--ghc-options=-optc-O2"
              "--ghc-options=-O2"
              "--ghc-options=-optl=-s"  # Strip via linker
              "--ghc-options=-split-sections"
              "--ghc-options=-fomit-interface-pragmas"
            ];
          }) (justStaticExecutables final.haskellPackages.todou);
        };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
            config.allowBroken    = true;
            config.stripDebugInfo = true; # strip all debug infos
          };
          hspkgs = pkgs.haskellPackages;
        in
        {
          nixosModules = rec {
            todou = import ./nix/modules self;
            default = todou;
          };

          devShells = rec {
            default = filehub-shell;
            filehub-shell = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          };

          packages = rec {
            default = todou;
            todou= pkgs.todou;
          };

          checks = {
            todou-tests = pkgs.haskell.lib.doCheck hspkgs.todou;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
