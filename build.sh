#!/usr/bin/env bash

tilerRoot=$(cd "$(dirname "$0")" && pwd)
cd "$tilerRoot"

echo "Building minimal-tiling-set..."
mkdir -p bin
g++ -O3 -std=c++17 -L /usr/lib64/openmpi/lib -lcryptominisat5 -lmpi minimal-tiling-sets/minimal-tiling-sets/*.cpp -o bin/minimal-tiling-sets
echo "Build done."
