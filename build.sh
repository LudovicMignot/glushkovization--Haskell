#!/usr/bin/env bash

set -e

wasm32-wasi-cabal build

hs_wasm_path=$(find . -name "*.wasm")

"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs \
     --input "$hs_wasm_path" --output frontend/ghc_wasm_jsffi.js

if [ $# -eq 0 ]; then
    echo "Building for dev"
    dev_mode=true
else
    echo "Building for prod"
    dev_mode=false
fi

if $dev_mode; then
    cp "$hs_wasm_path" frontend/glushkovization-exe.wasm
else
    env -i GHCRTS=-H64m "$(type -P wizer)" --allow-wasi --wasm-bulk-memory true --inherit-env true --init-func _initialize -o frontend/glushkovization-exe.wasm "$hs_wasm_path"
    wasm-opt ${1+"$@"} frontend/bin.wasm -o frontend/glushkovization-exe.wasm
    wasm-tools strip -o frontend/bin.wasm frontend/glushkovization-exe.wasm
fi

