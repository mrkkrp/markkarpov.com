#!/usr/bin/env bash
#
# Generate new Docker image for my site
#
# Usage: build-image.sh <VERSION>

VERSION=$1

set -eux pipefail

# Preparation: copy all the necessary files into this directory. This is
# necessary in order to copy them later into the image, because only local
# files (files in the same directory) can be copied to an image.

cp -v ../../../stack.yaml .
cp -v ../../../markkarpov-com.cabal .
cp -v ../../../Setup.hs .

# Build the image

docker build . -t mrkkrp/mk-com:$VERSION
