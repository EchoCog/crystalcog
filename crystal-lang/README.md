# Crystal Language Binaries and Sources

This directory contains Crystal language binaries and sources for offline installation
when the standard crystal-lang.org installation method is not available.

## Contents

- `README.md` - This file
- `install/` - Installation scripts and methods
- `binaries/` - Precompiled Crystal binaries for various platforms
- `sources/` - Crystal source code for building from source

## Usage

The Crystal installation script (`../scripts/install-crystal.sh`) will automatically
check this directory for offline installation resources when online methods fail.

## Supported Platforms

Currently targeting:
- Linux x86_64 (Ubuntu/Debian)
- Linux ARM64 (for ARM-based systems)

## Binary Sources

Binaries are downloaded from:
- Crystal official releases: https://github.com/crystal-lang/crystal/releases
- Mirror sites when available

## Building from Source

If precompiled binaries are not available, the installation script can build
Crystal from source using the files in the `sources/` directory.