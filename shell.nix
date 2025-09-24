{ pkgs, hspkgs }:
hspkgs.shellFor {
  packages = p: [ p.todou ];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.hlint
    hspkgs.cabal2nix
    hspkgs.ghcprofview
    hspkgs.eventlog2html
    pkgs.ghciwatch
    pkgs.bashInteractive
    pkgs.upx
    pkgs.zstd
    pkgs.zlib
    pkgs.pkg-config
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.bzip2}/lib:$LD_LIBRARY_PATH"
  '';
}
