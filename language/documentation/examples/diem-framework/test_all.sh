#!/bin/bash
# Copyright (c) The Diem Core Contributors
# SPDX-License-Identifier: Apache-2.0

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd "${SCRIPT_DIR}/move-packages/DPN" && cargo run -p df-cli -- package test &&
cd "${SCRIPT_DIR}/move-packages/core" && cargo run -p df-cli -- package test &&
cd "${SCRIPT_DIR}/move-packages/experimental" && cargo run -p df-cli -- package test
