#!/usr/bin/env bash

# Copyright (c) The Diem Core Contributors
# Copyright (c) The Move Contributors
# SPDX-License-Identifier: Apache-2.0

set -e

cargo_audit_ignores=(
   RUSTSEC-2021-0073
   RUSTSEC-2021-0072
   RUSTSEC-2020-0071
)

cmd="cargo audit"

cmd+="$(printf " --ignore %s" "${cargo_audit_ignores[@]}")"

$cmd
