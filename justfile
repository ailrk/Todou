default:
  @just --list

build:
    cabal2nix . > default.nix
    nix build

buildjs:
    #!/usr/bin/env bash
    pushd js/
    just build
    popd
    rm -rf data/todou
    mv -fT js/dist/ data/todou

dev:
    just buildjs
    cabal build

repl:
    cabal repl


profile:
    cabal clean
    cabal build --enable-profiling --disable-shared
    # run with +RTS -hc -p -RTS to get heap dump and prof file


vis:
    hp2ps -c -M todou.hp
    ps2pdf todou.ps


clean:
    cabal clean
    rm -f todou.hp
    rm -f todou.ps
    rm -f todou.pdf
    rm -f todou.prof
    rm -f todou.aux
