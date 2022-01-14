// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use once_cell::sync::Lazy;
use regex::Regex;

/// A macro which allows to define Yul functions together with their definitions.
/// This generates an enum `YulFunction` and functions `yule_name`, `yul_def`,
/// and `yul_deps` for values of this type.
macro_rules! functions {
    ($($name:ident: $def:literal $(dep $dep:ident),*),* $(, )?) => {
        #[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
        #[allow(dead_code)]
        pub enum YulFunction {
            $($name,)*
        }
        impl YulFunction {
            #[allow(dead_code)]
            pub fn yule_name(self) -> String {
                match self {
                $(
                    YulFunction::$name => make_yule_name(stringify!($name)),
                )*
                }
            }
            #[allow(dead_code)]
            pub fn yule_def(self) -> String {
                match self {
                $(
                    YulFunction::$name => make_yule_def(stringify!($name), $def),
                )*
                }
            }
            #[allow(dead_code)]
            pub fn yule_deps(self) -> Vec<YulFunction> {
                match self {
                $(
                    YulFunction::$name => vec![$(YulFunction::$dep,)*],
                )*
                }

            }
        }
    }
}

/// Helper to create name of Yul function.
fn make_yule_name(name: &str) -> String {
    format!("${}", name)
}

/// Helper to create definition of a Yule function.
fn make_yule_def(name: &str, body: &str) -> String {
    format!("function ${}{}", name, substitute(body))
}

/// Helper to substitute `$XXX` placeholders in the body of a Yul function.
fn substitute(s: &str) -> String {
    static REX: Lazy<Regex> = Lazy::new(|| Regex::new(r"(?m)(?P<var>\$[A-Z0-9_]+)").unwrap());
    let mut at = 0;
    let mut res = "".to_string();
    while let Some(cap) = (*REX).captures(&s[at..]) {
        let m = cap.name("var").unwrap();
        res.push_str(&s[at..at + m.start()]);
        match m.as_str() {
            "$MAX_U8" => res.push_str("0xff"),
            "$MAX_U64" => res.push_str("0xffffffffffffffff"),
            "$MAX_U128" => res.push_str("0xffffffffffffffffffffffffffffffff"),
            _ => res.push_str(m.as_str()),
        }
        at += m.end();
    }
    res.push_str(&s[at..]);
    res
}

// The Yul functions supporting the compilation scheme.
functions! {
Abort: "(code) {
    revert(0, 0) // TODO: convention to store code
}",
AbortBuiltin: "() {
    $Abort(-1)
}" dep Abort,
AddU64: "(x, y) -> r {
    if lt(sub($MAX_U64, x), y) { $AbortBuiltin() }
    r := add(x, y)
}" dep AbortBuiltin,
MulU64: "(x, y) -> r {
    if gt(y, div($MAX_U64, x)) { $AbortBuiltin() }
    r := mul(x, y)
}" dep AbortBuiltin,
AddU8: "(x, y) -> r {
    if lt(sub($MAX_U8, x), y) { $AbortBuiltin() }
    r := add(x, y)
}" dep AbortBuiltin,
MulU8: "(x, y) -> r {
    if gt(y, div($MAX_U8, x)) { $AbortBuiltin() }
    r := mul(x, y)
}" dep AbortBuiltin,
AddU128: "(x, y) -> r {
    if lt(sub($MAX_U128, x), y) { $AbortBuiltin() }
    r := add(x, y)
}" dep AbortBuiltin,
MulU128: "(x, y) -> r {
    if gt(y, div($MAX_U128, x)) { $AbortBuiltin() }
    r := mul(x, y)
}" dep AbortBuiltin,
Sub: "(x, y) -> r {
    if lt(x, y) { $AbortBuiltin() }
    r := sub(x, y)
}" dep AbortBuiltin,
Div: "(x, y) -> r {
    if eq(y, 0) { $AbortBuiltin() }
    r := div(x, y)
}" dep AbortBuiltin,
Mod: "(x, y) -> r {
    if eq(y, 0) { $AbortBuiltin() }
    r := mod(x, y)
}" dep AbortBuiltin,
Shr: "(x, y) -> r {
    r := shr(x, y)
}",
ShlU8: "(x, y) -> r {
    r := and(shl(x, y), $MAX_U8)
}",
ShlU64: "(x, y) -> r {
    r := and(shl(x, y), $MAX_U64)
}",
ShlU128: "(x, y) -> r {
    r := and(shl(x, y), $MAX_U128)
}",
Gt: "(x, y) -> r {
    r := gt(x, y)
}",
Lt: "(x, y) -> r {
    r := lt(x, y)
}",
GtEq: "(x, y) -> r {
    r := or(gt(x, y), eq(x, y))
}",
LtEq: "(x, y) -> r {
    r := or(lt(x, y), eq(x, y))
}",
Eq: "(x, y) -> r {
    r := eq(x, y)
}",
Neq: "(x, y) -> r {
    r := not(eq(x, y))
}",
LogicalAnd: "(x, y) -> r {
    r := and(x, y)
}",
LogicalOr: "(x, y) -> r {
    r := or(x, y)
}",
LogicalNot: "(x) -> r {
    r := not(x)
}",
BitAnd: "(x, y) -> r {
    r := and(x, y)
}",
BitOr: "(x, y) -> r {
    r := or(x, y)
}",
BitXor: "(x, y) -> r {
    r := xor(x, y)
}",
BitNot: "(x) -> r {
    r := not(x)
}",
CastU8: "(x) -> r {
    if gt(x, $MAX_U8) { $AbortBuiltin() }
    r := x
}" dep AbortBuiltin,
CastU64: "(x) -> r {
    if gt(x, $MAX_U64) { $AbortBuiltin() }
    r := x
}" dep AbortBuiltin,
CastU128: "(x) -> r {
    if gt(x, $MAX_U128) { $AbortBuiltin() }
    r := x
}" dep AbortBuiltin,
}
