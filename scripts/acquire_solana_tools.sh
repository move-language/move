#!/usr/bin/env bash

# Copyright (c) The Diem Core Contributors
# Copyright (c) The Move Contributors
# SPDX-License-Identifier: Apache-2.0

export MOVE_DEV_PATH
export PLATFORM_TOOLS_PATH

# platform tools version
version=v1.37

# check os and arch
OS=$(uname -s)
ARCH=$(uname -m)

infix=
case $OS in
Linux)
  infix=linux-x86_64
  ;;
Darwin)
  case $ARCH in
  x86_64)
    infix=osx-x86_64
    ;;
  *)
    echo "macOS on unexpected arch: $ARCH"
    exit 1
    ;;
  esac
  ;;
*)
  echo "unexpected OS: $OS"
  exit 1
  ;;
esac

# install move dev
root_path=solana-tools/llvm
filename="move-dev-$infix.tar.bz2"

mkdir -p "$root_path"
curl -L "https://github.com/solana-labs/platform-tools/releases/download/$version/$filename" \
  -o "$root_path/$filename"

pushd "$root_path" || exit 1
tar xjf "$filename"

MOVE_DEV_PATH="$(pwd)/move-dev"
popd || exit 1

# install platform tools
root_path=solana-tools/platform-tools
filename="platform-tools-$infix.tar.bz2"

mkdir -p "$root_path"
curl -L "https://github.com/solana-labs/platform-tools/releases/download/$version/$filename" \
  -o "$root_path/$filename"

pushd "$root_path" || exit 1
tar xjf "$filename"

PLATFORM_TOOLS_PATH=$(pwd)
popd || exit 1
