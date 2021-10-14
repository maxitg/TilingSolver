#!/usr/bin/env bash
set -eo pipefail

export NUMCPUS=`grep -c '^processor' /proc/cpuinfo`
echo "Found "$NUMCPUS" CPUs"

cd ~/git;

echo "Installing dependencies from apt-get..."
sudo apt-get -y update
sudo apt-get -y upgrade
sudo apt-get -y install build-essential cmake zlib1g-dev libm4ri-dev libsqlite3-dev help2man libboost-dev \
                        libboost-program-options-dev gcc libssl-dev
source ~/.bashrc
echo "Done installing dependencies from apt-get."

# Install cryptominisat
echo "Installing cryptominisat..."
git clone https://github.com/msoos/cryptominisat.git
cd cryptominisat
mkdir build
cd build
cmake ..
make -j$NUMCPUS
sudo make install
cd ../..
echo "Done installing cryptominisat."

# Install nlohmann json
echo "Installing nlohmann json..."
git clone https://github.com/nlohmann/json.git
cd json
mkdir build
cd build
cmake ..
make -j$NUMCPUS
sudo make install
cd ../..
echo "Done installing nlohmann json."
