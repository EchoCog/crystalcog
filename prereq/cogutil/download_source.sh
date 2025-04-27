#!/bin/bash

CURRENT_DIR=$(pwd)

cd /tmp/

rm -rf master.tar.gz cogutil-master/ link-grammar-5.*/ atomspace-master/ boost_* cmake-* cxxtest-*

#Download cogutil
wget https://github.com/opencog/cogutil/archive/master.tar.gz
tar -xvf master.tar.gz
mv cogutil-master-* cogutil-master
rm master.tar.gz

#Download atomspace
wget https://github.com/opencog/atomspace/archive/master.tar.gz
tar -xvf master.tar.gz
mv atomspace-master-* atomspace-master
rm master.tar.gz

#Download link grammar
wget -r --no-parent -nH --cut-dirs=2 http://www.abisource.com/downloads/link-grammar/current/
tar -zxf current/link-grammar-5*.tar.gz
rm -r current

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

#Move files into prereq/sources/
mkdir -p "$CURRENT_DIR/prereq/sources"
mv /tmp/cogutil-master "$CURRENT_DIR/prereq/sources/"
mv /tmp/atomspace-master "$CURRENT_DIR/prereq/sources/"
mv /tmp/link-grammar-5* "$CURRENT_DIR/prereq/sources/"
mv /tmp/boost "$CURRENT_DIR/prereq/sources/"
mv /tmp/cmake "$CURRENT_DIR/prereq/sources/"
mv /tmp/cxxtest "$CURRENT_DIR/prereq/sources/"

cd "$CURRENT_DIR"