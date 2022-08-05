// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(debug_assertions)]
pub const MOVEY_URL: &str = "https://movey-app-staging.herokuapp.com";
#[cfg(not(debug_assertions))]
pub const MOVEY_URL: &str = "https://www.movey.net";
