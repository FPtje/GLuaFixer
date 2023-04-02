#!/usr/bin/env bash

if [ -z ${CACHIX_AUTH_TOKEN+x} ]; then
    echo "Please set the CACHIX_AUTH_TOKEN"
    exit 1
fi;

nix build --no-link --json .#devShell.x86_64-linux | jq -r '.[].outputs | to_entries[].value' | cachix push glualint
nix build --no-link --json .#packages.x86_64-linux.glualint-static | jq -r '.[].outputs | to_entries[].value' | cachix push glualint
nix build --no-link --json .#packages.aarch64-linux.glualint-static | jq -r '.[].outputs | to_entries[].value' | cachix push glualint
