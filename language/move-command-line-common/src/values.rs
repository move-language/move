// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    address::ParsedAddress,
    parser::{Parser, Token},
};
use anyhow::bail;
use move_core_types::{
    account_address::AccountAddress,
    identifier::{self, Identifier},
    value::{MoveStruct, MoveValue},
};
use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub enum ValueToken {
    Number,
    NumberTyped,
    True,
    False,
    ByteString,
    HexString,
    Ident,
    AtSign,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LParen,
    RParen,
    Comma,
    Colon,
    ColonColon,
    Whitespace,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum ParsedValue<Extra: ParsableValue = ()> {
    Address(ParsedAddress),
    InferredNum(u128),
    U8(u8),
    U64(u64),
    U128(u128),
    Bool(bool),
    Vector(Vec<ParsedValue<Extra>>),
    Struct(
        ParsedAddress,
        String,
        String,
        BTreeMap<String, ParsedValue<Extra>>,
    ),
    Custom(Extra),
}

pub trait ParsableValue: Sized {
    type ConcreteValue;
    fn parse_value<'a, I: Iterator<Item = (ValueToken, &'a str)>>(
        parser: &mut Parser<'a, ValueToken, I>,
    ) -> Option<anyhow::Result<Self>>;

    fn move_value_into_concrete(v: MoveValue) -> anyhow::Result<Self::ConcreteValue>;
    fn concrete_vector(elems: Vec<Self::ConcreteValue>) -> anyhow::Result<Self::ConcreteValue>;
    fn concrete_struct(
        addr: AccountAddress,
        module: String,
        name: String,
        values: BTreeMap<String, Self::ConcreteValue>,
    ) -> anyhow::Result<Self::ConcreteValue>;
    fn into_concrete_value(
        self,
        mapping: &impl Fn(&str) -> Option<AccountAddress>,
    ) -> anyhow::Result<Self::ConcreteValue>;
}

impl ParsableValue for () {
    type ConcreteValue = MoveValue;
    fn parse_value<'a, I: Iterator<Item = (ValueToken, &'a str)>>(
        _: &mut Parser<'a, ValueToken, I>,
    ) -> Option<anyhow::Result<Self>> {
        None
    }
    fn move_value_into_concrete(v: MoveValue) -> anyhow::Result<Self::ConcreteValue> {
        Ok(v)
    }

    fn concrete_vector(elems: Vec<Self::ConcreteValue>) -> anyhow::Result<Self::ConcreteValue> {
        Ok(MoveValue::Vector(elems))
    }

    fn concrete_struct(
        _address: AccountAddress,
        _module: String,
        _name: String,
        values: BTreeMap<String, Self::ConcreteValue>,
    ) -> anyhow::Result<Self::ConcreteValue> {
        Ok(MoveValue::Struct(MoveStruct::WithFields(
            values
                .into_iter()
                .map(|(f, v)| Ok((Identifier::new(f)?, v)))
                .collect::<anyhow::Result<_>>()?,
        )))
    }
    fn into_concrete_value(
        self,
        _mapping: &impl Fn(&str) -> Option<AccountAddress>,
    ) -> anyhow::Result<Self::ConcreteValue> {
        unreachable!()
    }
}

impl Display for ValueToken {
    fn fmt<'f>(&self, formatter: &mut fmt::Formatter<'f>) -> Result<(), fmt::Error> {
        let s = match self {
            ValueToken::Number => "[num]",
            ValueToken::NumberTyped => "[num typed]",
            ValueToken::True => "true",
            ValueToken::False => "false",
            ValueToken::ByteString => "[byte string]",
            ValueToken::HexString => "[hex string]",
            ValueToken::Whitespace => "[whitespace]",
            ValueToken::Ident => "[identifier]",
            ValueToken::AtSign => "@",
            ValueToken::LBrace => "{",
            ValueToken::RBrace => "}",
            ValueToken::LBracket => "[",
            ValueToken::RBracket => "]",
            ValueToken::LParen => "(",
            ValueToken::RParen => ")",
            ValueToken::Comma => ",",
            ValueToken::Colon => ":",
            ValueToken::ColonColon => "::",
        };
        fmt::Display::fmt(s, formatter)
    }
}

impl Token for ValueToken {
    fn is_whitespace(&self) -> bool {
        matches!(self, Self::Whitespace)
    }

    fn next_token(s: &str) -> anyhow::Result<Option<(Self, usize)>> {
        fn number_maybe_with_suffix(text: &str, num_text_len: usize) -> (ValueToken, usize) {
            let rest = &text[num_text_len..];
            if rest.starts_with("u8") {
                (ValueToken::NumberTyped, num_text_len + 2)
            } else if rest.starts_with("u64") {
                (ValueToken::NumberTyped, num_text_len + 3)
            } else if rest.starts_with("u128") {
                (ValueToken::NumberTyped, num_text_len + 4)
            } else {
                // No typed suffix
                (ValueToken::Number, num_text_len)
            }
        }
        if s.starts_with("true") {
            return Ok(Some((Self::True, 4)));
        }
        if s.starts_with("false") {
            return Ok(Some((Self::False, 5)));
        }

        let mut chars = s.chars().peekable();
        let c = match chars.next() {
            None => return Ok(None),
            Some(c) => c,
        };
        Ok(Some(match c {
            '@' => (Self::AtSign, 1),
            '{' => (Self::LBrace, 1),
            '}' => (Self::RBrace, 1),
            '[' => (Self::LBracket, 1),
            ']' => (Self::RBracket, 1),
            '(' => (Self::LParen, 1),
            ')' => (Self::RParen, 1),
            ',' => (Self::Comma, 1),
            ':' if matches!(chars.peek(), Some(':')) => (Self::ColonColon, 2),
            ':' => (Self::Colon, 1),
            '0' if matches!(chars.peek(), Some('x')) => {
                chars.next().unwrap();
                match chars.next() {
                    Some(c) if c.is_ascii_hexdigit() => {
                        let len = 3 + chars.take_while(char::is_ascii_hexdigit).count();
                        number_maybe_with_suffix(s, len)
                    }
                    _ => bail!("unrecognized token: {}", s),
                }
            }
            'b' if matches!(chars.peek(), Some('"')) => {
                chars.next().unwrap();
                // b"
                let mut len = 2;
                loop {
                    len += 1;
                    match chars.next() {
                        Some('"') => break,
                        Some(c) if c.is_ascii() => (),
                        Some(c) => bail!(
                            "Unexpected non-ascii character '{}' in byte string: {}",
                            c.escape_default(),
                            s
                        ),
                        None => bail!("Unexpected end of string before end quote: {}", s),
                    }
                }
                if s[..len].chars().any(|c| c == '\\') {
                    bail!(
                        "Escape characters not yet supported in byte string: {}",
                        &s[..len]
                    )
                }
                (ValueToken::ByteString, len)
            }
            'x' if matches!(chars.peek(), Some('"')) => {
                chars.next().unwrap();
                //  x"
                let mut len = 2;
                loop {
                    len += 1;
                    match chars.next() {
                        Some('"') => break,
                        Some(c) if c.is_ascii_hexdigit() => (),
                        Some(c) => bail!(
                            "Unexpected non-hexdigit '{}' in hex string: {}",
                            c.escape_default(),
                            s
                        ),
                        None => bail!("Unexpected end of string before end quote: {}", s),
                    }
                }
                assert!(len >= 3);
                let num_digits = len - 3;
                if num_digits % 2 != 0 {
                    bail!(
                        "Expected an even number of hex digits in hex string: {}",
                        &s[..len]
                    )
                }
                (ValueToken::HexString, len)
            }
            c if c.is_ascii_digit() => {
                // c + remaining
                let len = 1 + chars.take_while(char::is_ascii_digit).count();
                number_maybe_with_suffix(s, len)
            }
            c if c.is_ascii_whitespace() => {
                // c + remaining
                let len = 1 + chars.take_while(char::is_ascii_whitespace).count();
                (Self::Whitespace, len)
            }
            c if c.is_ascii_alphabetic() => {
                // c + remaining
                // TODO be more permissive
                let len = 1 + chars
                    .take_while(|c| identifier::is_valid_identifier_char(*c))
                    .count();
                (Self::Ident, len)
            }
            _ => bail!("unrecognized token: {}", s),
        }))
    }
}

impl<Extra: ParsableValue> ParsedValue<Extra> {
    pub fn into_concrete_value(
        self,
        mapping: &impl Fn(&str) -> Option<AccountAddress>,
    ) -> anyhow::Result<Extra::ConcreteValue> {
        match self {
            ParsedValue::Address(a) => Extra::move_value_into_concrete(MoveValue::Address(
                a.into_account_address(mapping)?,
            )),
            ParsedValue::U8(u) => Extra::move_value_into_concrete(MoveValue::U8(u)),
            ParsedValue::U64(u) => Extra::move_value_into_concrete(MoveValue::U64(u)),
            ParsedValue::InferredNum(u) if u <= (u64::MAX as u128) => {
                Extra::move_value_into_concrete(MoveValue::U64(u as u64))
            }
            ParsedValue::InferredNum(u) | ParsedValue::U128(u) => {
                Extra::move_value_into_concrete(MoveValue::U128(u))
            }
            ParsedValue::Bool(b) => Extra::move_value_into_concrete(MoveValue::Bool(b)),
            ParsedValue::Vector(values) => Extra::concrete_vector(
                values
                    .into_iter()
                    .map(|value| value.into_concrete_value(mapping))
                    .collect::<anyhow::Result<_>>()?,
            ),
            ParsedValue::Struct(addr, module, name, values) => Extra::concrete_struct(
                ParsedAddress::into_account_address(addr, mapping)?,
                module,
                name,
                values
                    .into_iter()
                    .map(|(field, value)| Ok((field, value.into_concrete_value(mapping)?)))
                    .collect::<anyhow::Result<_>>()?,
            ),
            ParsedValue::Custom(c) => Extra::into_concrete_value(c, mapping),
        }
    }
}
