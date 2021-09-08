#!/usr/bin/env bash
set -eo pipefail

export NUMCPUS=`grep -c '^processor' /proc/cpuinfo`
echo "Found "$NUMCPUS" CPUs"

cd ~/git;

# Install dependencies from yum
echo "Installing dependencies from yum..."
sudo yum -y install cmake3 zlib-devel boost-devel sqlite-devel help2man autoconf automake libtool mpi-devel
echo 'export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":/usr/lib64/openmpi/lib' >> ~/.bash_profile
echo 'export PATH="$PATH":/usr/lib64/openmpi/bin/' >> ~/.bash_profile
source ~/.bash_profile
echo "Done installing dependencies from yum."

# Install m4ri
echo "Installing m4ri..."
git clone https://bitbucket.org/malb/m4ri.git
cd m4ri
autoreconf --install
./configure --enable-openmp
make -j$NUMCPUS
sudo make install
echo 'export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":/usr/local/lib' >> ~/.bash_profile
source ~/.bash_profile
cd ..
echo "Done installing m4ri."

# Install cryptominisat
echo "Installing cryptominisat..."
git clone https://github.com/msoos/cryptominisat.git
cd cryptominisat
mkdir build
cd build
cmake3 ..
make -j$NUMCPUS
sudo make install
echo 'export LD_LIBRARY_PATH="$LD_LIBRARY_PATH":/usr/local/lib64' >> ~/.bash_profile
source ~/.bash_profile
sudo ldconfig
cd ../..
echo "Done cryptominisat."
