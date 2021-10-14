#!/usr/bin/env bash
set -eo pipefail

tilerRoot=$(cd "$(dirname "$0")" && pwd)
cd "$tilerRoot"

echo "Building minimal-tiling-set..."
mkdir -p bin
g++ -O3 -std=c++17 -L/usr/local/opt/openssl/lib -I/usr/local/opt/openssl/include \
                   minimal-tiling-sets/minimal-tiling-sets/*.cpp \
                   -lcryptominisat5 -lboost_filesystem -pthread -lssl -lcrypto \
                   -o bin/minimal-tiling-sets
echo "Build done."
