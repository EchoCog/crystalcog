#!/bin/bash

# Install required prerequisites for Cogutil
sudo apt-get install -y \
    libboost-dev \
    libboost-filesystem-dev \
    libboost-program-options-dev \
    libboost-system-dev \
    libboost-thread-dev \
    cmake \
    cxxtest

# Install optional prerequisites for Cogutil (recommended)
apt-get install -y \
    binutils-dev \
    libiberty-dev \
    doxygen

echo "Cogutil prerequisites installation complete."