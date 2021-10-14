#!/usr/bin/env bash

tilerRoot=$(cd "$(dirname "$0")" && pwd)
cd "$tilerRoot"

echo "Building minimal-tiling-set..."
mkdir -p bin
g++ -O3 -std=c++17 -L/usr/local/opt/openssl/lib -I/usr/local/opt/openssl/include \
                   -L/usr/local/lib -I/usr/local/include \
                   -lcryptominisat5 -lboost_filesystem -lcrypto -lssl -pthread \
                   minimal-tiling-sets/minimal-tiling-sets/*.cpp -o bin/minimal-tiling-sets
echo "Build done."
