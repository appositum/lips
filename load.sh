#!/bin/sh

nix-shell --run "cabal --enable-nix v2-repl lib:lips"
