{ pkgs, hspkgs }:
hspkgs.shellFor {
  packages = p: [ p.todou ];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.hlint
    hspkgs.cabal2nix
    hspkgs.eventlog2html
    hspkgs.fast-tags
    hspkgs.graphmod
    pkgs.ghcid
    pkgs.bashInteractive
    pkgs.upx
    pkgs.zstd
    pkgs.zlib
    pkgs.pkg-config
    pkgs.concurrently
    pkgs.nodePackages.typescript
    pkgs.rust-analyzer
    pkgs.cargo
    pkgs.rustc
    pkgs.atk
    pkgs.glib
    pkgs.gtk3
    pkgs.webkitgtk_4_1
    pkgs.libsoup_3
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.bzip2}/lib:$LD_LIBRARY_PATH"
  '';
}
