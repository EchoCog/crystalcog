#!/bin/bash

CURRENT_DIR=$(pwd)

cd /tmp/

rm -rf boost_* cmake-* cxxtest-* binutils-* doxygen*

# Download Boost
BOOST_URL=$(wget -qO- https://www.boost.org/users/download/ | grep -oP 'https://boostorg.jfrog.io/artifactory/main/release/\d+\.\d+\.\d+/boost_\d+_\d+\.\d+\.tar\.gz' | head -n 1)
if [ -n "$BOOST_URL" ]; then
    wget "$BOOST_URL"
    tar -xzf boost_*.tar.gz
    mv boost_* boost
    rm boost_*.tar.gz
else
    echo "Error: Could not determine latest Boost version."
fi

#Download CMake
CMAKE_URL=$(wget -qO- https://cmake.org/download/ | grep -oP 'https://github.com/Kitware/CMake/releases/download/v\d+\.\d+\.\d+/cmake-\d+\.\d+\.\d+-linux-x86_64\.tar\.gz'| head -n 1)
if [ -n "$CMAKE_URL" ]; then
    wget "$CMAKE_URL"
    tar -xzf cmake-*.tar.gz
    mv cmake-* cmake
    rm cmake-*.tar.gz
else
    echo "Error: Could not determine latest CMake version."
fi

#Download cxxtest
wget http://cxxtest.com/download/cxxtest-4.4.tar.gz
tar -xzf cxxtest-4.4.tar.gz
mv cxxtest-4.4 cxxtest
rm cxxtest-4.4.tar.gz

#Download Binutils
BINUTILS_URL=$(wget -qO- https://ftp.gnu.org/gnu/binutils/ | grep -oP 'href="binutils-\d+\.\d+(\.\d+)?\.tar\.gz"' | sed 's/href="//' | head -n 1)
if [ -n "$BINUTILS_URL" ]; then
    wget "https://ftp.gnu.org/gnu/binutils/$BINUTILS_URL"
    tar -xzf binutils-*.tar.gz
    mv binutils-* binutils
    rm binutils-*.tar.gz
else
    echo "Error: Could not determine latest binutils version."
fi

#Download libiberty
LIBERTY_URL=$(wget -qO- https://ftp.gnu.org/gnu/binutils/ | grep -oP 'href="libiberty-\d+\.\d+(\.\d+)?\.tar\.gz"' | sed 's/href="//' | head -n 1)
if [ -n "$LIBERTY_URL" ]; then
    wget "https://ftp.gnu.org/gnu/binutils/$LIBERTY_URL"
    tar -xzf libiberty-*.tar.gz
    mv libiberty-* libiberty
    rm libiberty-*.tar.gz
else
    echo "Error: Could not determine latest libiberty version."
fi
#Download Doxygen
DOXYGEN_URL=$(wget -qO- https://www.doxygen.nl/download.html | grep -oP 'href="https://www.doxygen.nl/files/doxygen-\d+\.\d+\.\d+\.src\.tar\.gz"' | head -n 1)
if [ -n "$DOXYGEN_URL" ]; then
    wget "$DOXYGEN_URL"
    tar -xzf doxygen-*.tar.gz
    mv doxygen-* doxygen
    rm doxygen-*.tar.gz
else
    echo "Error: Could not determine latest doxygen version."
fi


#Move files into prereq/sources/
mkdir -p "$CURRENT_DIR/prereq/sources"
mv /tmp/boost "$CURRENT_DIR/prereq/sources/"
mv /tmp/cmake "$CURRENT_DIR/prereq/sources/"
mv /tmp/cxxtest "$CURRENT_DIR/prereq/sources/"
mv /tmp/binutils "$CURRENT_DIR/prereq/sources/"
mv /tmp/libiberty "$CURRENT_DIR/prereq/sources/"
mv /tmp/doxygen "$CURRENT_DIR/prereq/sources/"

cd "$CURRENT_DIR"
