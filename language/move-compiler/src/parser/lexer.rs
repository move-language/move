// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{diag, diagnostics::Diagnostic, parser::syntax::make_loc};
use move_command_line_common::files::FileHash;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::{fmt, mem};

use super::{comments::CommentKind, cst::ParsedToken, token_range::TokenRange};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Tok {
    BOF, // begin of the file
    EOF, // end of the file
    NumValue,
    NumTypedValue,
    ByteStringValue,
    Identifier,
    Exclaim,
    ExclaimEqual,
    Percent,
    Amp,
    AmpAmp,
    AmpMut,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Star,
    Plus,
    Comma,
    Minus,
    Period,
    PeriodPeriod,
    Slash,
    Colon,
    ColonColon,
    Semicolon,
    Less,
    LessEqual,
    LessLess,
    Equal,
    EqualEqual,
    EqualEqualGreater,
    LessEqualEqualGreater,
    Greater,
    GreaterEqual,
    GreaterGreater,
    Caret,
    Abort,
    Acquires,
    As,
    Break,
    Continue,
    Copy,
    Else,
    False,
    If,
    Invariant,
    Let,
    Loop,
    Module,
    Move,
    Native,
    Public,
    Return,
    Spec,
    Struct,
    True,
    Use,
    While,
    LBrace,
    Pipe,
    PipePipe,
    RBrace,
    Fun,
    Script,
    Const,
    Friend,
    NumSign,
    AtSign,
    Comment(CommentKind),
    Space, // " "
    Tab,   // "\t"
    LF,    // "\n"
    CR,    // "\r",
}

pub fn is_comment_spaces(token: Tok) -> bool {
    matches!(
        token,
        Tok::Comment(_) | Tok::Space | Tok::Tab | Tok::LF | Tok::CR
    )
}

impl fmt::Display for Tok {
    fn fmt<'f>(&self, formatter: &mut fmt::Formatter<'f>) -> Result<(), fmt::Error> {
        use Tok::*;
        let s = match *self {
            BOF => "[begin-of-file]",
            EOF => "[end-of-file]",
            NumValue => "[Num]",
            NumTypedValue => "[NumTyped]",
            ByteStringValue => "[ByteString]",
            Identifier => "[Identifier]",
            Exclaim => "!",
            ExclaimEqual => "!=",
            Percent => "%",
            Amp => "&",
            AmpAmp => "&&",
            AmpMut => "&mut",
            LParen => "(",
            RParen => ")",
            LBracket => "[",
            RBracket => "]",
            Star => "*",
            Plus => "+",
            Comma => ",",
            Minus => "-",
            Period => ".",
            PeriodPeriod => "..",
            Slash => "/",
            Colon => ":",
            ColonColon => "::",
            Semicolon => ";",
            Less => "<",
            LessEqual => "<=",
            LessLess => "<<",
            Equal => "=",
            EqualEqual => "==",
            EqualEqualGreater => "==>",
            LessEqualEqualGreater => "<==>",
            Greater => ">",
            GreaterEqual => ">=",
            GreaterGreater => ">>",
            Caret => "^",
            Abort => "abort",
            Acquires => "acquires",
            As => "as",
            Break => "break",
            Continue => "continue",
            Copy => "copy",
            Else => "else",
            False => "false",
            If => "if",
            Invariant => "invariant",
            Let => "let",
            Loop => "loop",
            Module => "module",
            Move => "move",
            Native => "native",
            Public => "public",
            Return => "return",
            Spec => "spec",
            Struct => "struct",
            True => "true",
            Use => "use",
            While => "while",
            LBrace => "{",
            Pipe => "|",
            PipePipe => "||",
            RBrace => "}",
            Fun => "fun",
            Script => "script",
            Const => "const",
            Friend => "friend",
            NumSign => "#",
            AtSign => "@",
            Comment(CommentKind::DocComment) => "///[Comment]",
            Comment(CommentKind::SignleLine) => "//[Comment]",
            Comment(CommentKind::BlockComment) => "/*[Comment]*/",
            Comment(CommentKind::DocBlockComment) => "/**[Comment]*/",
            CR => "\r",
            LF => "\n",
            Space => " ",
            Tab => "\t",
        };
        fmt::Display::fmt(s, formatter)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: Tok,
    pub content: Symbol,
    pub loc: Loc,
}

impl Token {
    pub fn new(kind: Tok, content: Symbol, loc: Loc) -> Self {
        Token { kind, content, loc }
    }
}

// FidelityToken contains one non-comment-or-space token with the spac tokens and comment tokens
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FidelityToken<'a> {
    // non-comment-or-space  token
    pub token: Token,
    // tokens including space and comments which wraps the non-comment-or-space token
    pub tokens: Vec<Token>,
    pub content: &'a str,
}

impl<'a> FidelityToken<'a> {
    pub fn new(token: Token, tokens: Vec<Token>, content: &'a str) -> Self {
        FidelityToken {
            token,
            tokens,
            content,
        }
    }

    pub fn content(&self) -> &str {
        self.token.content.as_str()
    }
}

pub struct Lexer<'input> {
    text: &'input str,
    file_hash: FileHash,
    prev_end: usize,
    cur_start: usize,
    cur_end: usize,
    token: FidelityToken<'input>,
    cached_tokens: Vec<Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(text: &'input str, file_hash: FileHash) -> Lexer<'input> {
        Lexer {
            text,
            file_hash,
            prev_end: 0,
            cur_start: 0,
            cur_end: 0,
            token: FidelityToken::new(
                Token::new(Tok::BOF, Symbol::from(""), Loc::new(file_hash, 0, 0)),
                Vec::new(),
                text,
            ),
            cached_tokens: vec![],
        }
    }

    pub fn peek(&self) -> Tok {
        self.token.token.kind
    }

    pub fn content(&self) -> &'input str {
        let start_loc = self.token.token.loc.start() as usize;
        let end_loc = self.token.token.loc.end() as usize;
        &self.text[start_loc..end_loc]
    }

    pub fn file_hash(&self) -> FileHash {
        self.file_hash
    }

    pub fn start_loc(&self) -> usize {
        self.current_loc().start() as usize
    }

    pub fn previous_end_loc(&self) -> usize {
        self.prev_end
    }

    pub fn current_loc(&self) -> Loc {
        self.token.token.loc
    }

    // take all the cached tokens out
    pub fn take_tokens(&mut self) -> Vec<Token> {
        mem::take(&mut self.cached_tokens)
    }

    pub fn peek_fadelity(&self) -> &FidelityToken {
        &self.token
    }

    pub fn current_token(&self) -> Token {
        self.token.token
    }
    pub fn token_range(&self, start: usize, end: usize) -> TokenRange {
        TokenRange::new(start, end)
    }

    pub fn tokens(&self) -> &[Token] {
        &self.cached_tokens
    }

    pub fn tokens_loc(&self) -> usize {
        self.cached_tokens.len()
    }

    pub fn lookahead(&self) -> Result<Tok, Diagnostic> {
        self.lookahead_fidelity().map(|t| t.token.kind)
    }

    pub fn lookahead_fidelity(&self) -> Result<FidelityToken, Diagnostic> {
        let text = &self.text[self.cur_end..];
        Ok(find_fidelity_token(self.file_hash, text, self.cur_end)?.0)
    }

    pub fn lookahead2(&self) -> Result<(Tok, Tok), Diagnostic> {
        let text = &self.text[self.cur_end..];
        let (tok1, len) = find_fidelity_token(self.file_hash, text, self.cur_end)?;
        let text2 = &self.text[self.cur_end + len..];
        let (tok2, _) = find_fidelity_token(self.file_hash, text2, self.cur_end + len)?;
        Ok((tok1.token.kind, tok2.token.kind))
    }

    pub fn advance(&mut self) -> Result<ParsedToken, Diagnostic> {
        let start_token_index = self.tokens_loc();
        let pre_token = self.current_token();
        self.cached_tokens.append(&mut self.token.tokens);
        self.prev_end = self.token.token.loc.end() as usize;
        self.cur_start = self.cur_end;
        let text = &self.text[self.cur_start..];
        let (token, len) = find_fidelity_token(self.file_hash, text, self.cur_start)?;
        self.cur_end = self.cur_start + len;
        self.token = token;
        let end_token_index = self.tokens_loc();
        let parsed_token = ParsedToken::new(
            pre_token,
            self.token_range(start_token_index, end_token_index),
        );
        Ok(parsed_token)
    }

    // While parsing a list and expecting a ">" token to mark the end, replace
    // a ">>" token with the expected ">". This handles the situation where there
    // are nested type parameters that result in two adjacent ">" tokens, e.g.,
    // "A<B<C>>".
    pub fn adjust_token(&mut self, end_token: Tok) {
        if self.peek() == Tok::GreaterGreater && end_token == Tok::Greater {
            let token = Token::new(
                Tok::Greater,
                Symbol::from(">"),
                Loc::new(
                    self.file_hash,
                    self.cur_start as u32,
                    (self.cur_start + 1) as u32,
                ),
            );
            self.token = FidelityToken::new(
                token,
                vec![token],
                &self.text[self.cur_start..self.cur_start + 1],
            );
            self.cur_end = self.cur_start + 1
        }
    }
}

// Find the next token and its length without changing the state of the lexer.
pub fn find_token(
    file_hash: FileHash,
    text: &str,
    start_offset: usize,
) -> Result<(Tok, usize), Diagnostic> {
    let c: char = match text.chars().next() {
        Some(next_char) => next_char,
        None => {
            return Ok((Tok::EOF, 0));
        }
    };
    let (tok, len) = match c {
        '0'..='9' => {
            if text.starts_with("0x") && text.len() > 2 {
                let (tok, hex_len) = get_hex_number(&text[2..]);
                if hex_len == 0 {
                    // Fall back to treating this as a "0" token.
                    (Tok::NumValue, 1)
                } else {
                    (tok, 2 + hex_len)
                }
            } else {
                get_decimal_number(text)
            }
        }
        'A'..='Z' | 'a'..='z' | '_' => {
            let is_hex = text.starts_with("x\"");
            if is_hex || text.starts_with("b\"") {
                let line = &text.lines().next().unwrap()[2..];
                match get_string_len(line) {
                    Some(last_quote) => (Tok::ByteStringValue, 2 + last_quote + 1),
                    None => {
                        let loc = make_loc(file_hash, start_offset, start_offset + line.len() + 2);
                        return Err(diag!(
                            if is_hex {
                                Syntax::InvalidHexString
                            } else {
                                Syntax::InvalidByteString
                            },
                            (loc, "Missing closing quote (\") after byte string")
                        ));
                    }
                }
            } else {
                let len = get_name_len(text);
                (get_name_token(&text[..len]), len)
            }
        }
        '&' => {
            if text.starts_with("&mut ") {
                (Tok::AmpMut, 5)
            } else if text.starts_with("&&") {
                (Tok::AmpAmp, 2)
            } else {
                (Tok::Amp, 1)
            }
        }
        '|' => {
            if text.starts_with("||") {
                (Tok::PipePipe, 2)
            } else {
                (Tok::Pipe, 1)
            }
        }
        '=' => {
            if text.starts_with("==>") {
                (Tok::EqualEqualGreater, 3)
            } else if text.starts_with("==") {
                (Tok::EqualEqual, 2)
            } else {
                (Tok::Equal, 1)
            }
        }
        '!' => {
            if text.starts_with("!=") {
                (Tok::ExclaimEqual, 2)
            } else {
                (Tok::Exclaim, 1)
            }
        }
        '<' => {
            if text.starts_with("<==>") {
                (Tok::LessEqualEqualGreater, 4)
            } else if text.starts_with("<=") {
                (Tok::LessEqual, 2)
            } else if text.starts_with("<<") {
                (Tok::LessLess, 2)
            } else {
                (Tok::Less, 1)
            }
        }
        '>' => {
            if text.starts_with(">=") {
                (Tok::GreaterEqual, 2)
            } else if text.starts_with(">>") {
                (Tok::GreaterGreater, 2)
            } else {
                (Tok::Greater, 1)
            }
        }
        ':' => {
            if text.starts_with("::") {
                (Tok::ColonColon, 2)
            } else {
                (Tok::Colon, 1)
            }
        }
        '%' => (Tok::Percent, 1),
        '(' => (Tok::LParen, 1),
        ')' => (Tok::RParen, 1),
        '[' => (Tok::LBracket, 1),
        ']' => (Tok::RBracket, 1),
        '*' => (Tok::Star, 1),
        '+' => (Tok::Plus, 1),
        ',' => (Tok::Comma, 1),
        '-' => (Tok::Minus, 1),
        '.' => {
            if text.starts_with("..") {
                (Tok::PeriodPeriod, 2)
            } else {
                (Tok::Period, 1)
            }
        }
        '/' => {
            if text.starts_with("/*") {
                return find_block_comment(text, start_offset, file_hash);
            } else if text.starts_with("//") {
                find_comment(text)
            } else {
                (Tok::Slash, 1)
            }
        }
        ';' => (Tok::Semicolon, 1),
        '^' => (Tok::Caret, 1),
        '{' => (Tok::LBrace, 1),
        '}' => (Tok::RBrace, 1),
        '#' => (Tok::NumSign, 1),
        '@' => (Tok::AtSign, 1),
        ' ' => (Tok::Space, 1),
        '\t' => (Tok::Tab, 1),
        '\n' => (Tok::LF, 1),
        '\r' => (Tok::CR, 1),
        _ => {
            let loc = make_loc(file_hash, start_offset, start_offset);
            return Err(diag!(
                Syntax::InvalidCharacter,
                (loc, format!("Invalid character: '{}'", c))
            ));
        }
    };

    Ok((tok, len))
}

// Find the fidelity token
// This function would group comments and spaces based on new line
// Example:
//
// """
// // comment1
// Token1 // comment2
// // comment3
//
// // comment4
// Token2
// """"
//
// lexing with the function find_fidelity_token. It would generate two tokens.
// The first one would be `FidelityToken{token: Token1, tokens: vec![comment1, LF, Token1, Space, comment2]}`.
// The second one would be `FidelityToken{token: Token1, tokens: vec![comment3, LF, LF,comment4, Token2]}`
//
// The leading comments would not be split if newline exists but the trailing comment would be split .
pub fn find_fidelity_token(
    file_hash: FileHash,
    text: &str,
    start_offset: usize,
) -> Result<(FidelityToken, usize), Diagnostic> {
    let mut cur_text = text;
    let mut text_len = 0;
    let mut new_tokens: Vec<Token> = Vec::new();
    let mut non_comment_token: Option<Token> = None;
    let mut cur_offset = start_offset;
    loop {
        let (tok, length) = find_token(file_hash, cur_text, cur_offset)?;
        let range = Loc::new(file_hash, cur_offset as u32, (cur_offset + length) as u32);

        match tok {
            Tok::Comment(kind) => {
                let content = match kind {
                    CommentKind::DocComment => Symbol::from(&cur_text[3..length]),
                    CommentKind::SignleLine => Symbol::from(&cur_text[2..length]),
                    CommentKind::BlockComment => Symbol::from(&cur_text[2..length - 2]),
                    CommentKind::DocBlockComment => Symbol::from(&cur_text[3..length - 2]),
                };
                let new_token = Token::new(tok, content, range);
                new_tokens.push(new_token);

                cur_offset += length;
                text_len += length;
                cur_text = &cur_text[length..];
            }
            Tok::LF => {
                let content = Symbol::from(&cur_text[..length]);
                let new_token = Token::new(tok, content, range);
                new_tokens.push(new_token);
                cur_offset += length;
                text_len += length;
                cur_text = &cur_text[length..];

                if non_comment_token.is_some() {
                    break;
                }
            }
            Tok::Space | Tok::Tab | Tok::CR => {
                let content = Symbol::from(&cur_text[..length]);
                let new_token = Token::new(tok, content, range);
                new_tokens.push(new_token);
                cur_offset += length;
                text_len += length;
                cur_text = &cur_text[length..];
            }
            Tok::EOF => {
                let content = Symbol::from(&cur_text[..length]);
                let new_token = Token::new(tok, content, range);
                if non_comment_token.is_some() {
                    break;
                } else {
                    new_tokens.push(new_token);
                    non_comment_token = Some(new_token);
                }
            }
            _ => {
                let content = Symbol::from(&cur_text[..length]);
                let new_token = Token::new(tok, content, range);
                if non_comment_token.is_none() {
                    new_tokens.push(new_token);
                    non_comment_token = Some(new_token);
                    cur_offset += length;
                    text_len += length;
                    cur_text = &cur_text[length..];
                } else {
                    break;
                }
            }
        }
    }
    non_comment_token
        .map(|_| {
            (
                FidelityToken::new(non_comment_token.unwrap(), new_tokens, &text[0..text_len]),
                text_len,
            )
        })
        .ok_or_else(|| {
            diag!(
                Bug::TokenizedFailure,
                (
                    Loc::new(file_hash, start_offset as u32, cur_offset as u32),
                    "Could not find valid token."
                )
            )
        })
}

// find comments like '//...\n' or doc comments '///...\n'
// the argument text must start with '//'
fn find_comment(text: &str) -> (Tok, usize) {
    let is_doc = text.starts_with("///") && !text.starts_with("////");
    let comment_end = text.trim_start_matches(|c: char| c != '\n');
    let length = text.len() - comment_end.len();
    // If this was a documentation comment, record it in our map.
    if is_doc {
        (Tok::Comment(CommentKind::DocComment), length)
    } else {
        (Tok::Comment(CommentKind::SignleLine), length)
    }
}

// find multi-line comments like '/* ... */' or '/** ... */'.
// These can be nested, as in '/* /* ... */ */', so record the
// start locations of each nested comment as a stack. The
// boolean indicates whether it's a documentation comment.
// the argument text_ must start with '/*'
fn find_block_comment(
    text_: &str,
    off_set: usize,
    file_hash: FileHash,
) -> Result<(Tok, usize), Diagnostic> {
    let mut text = &text_[0..];
    let len = text.len();
    let get_offset = |substring: &str| len - substring.len();
    let mut locs: Vec<(usize, bool)> = vec![];
    let is_doc = text_.starts_with("/**");
    loop {
        text = text.trim_start_matches(|c: char| c != '/' && c != '*');
        if text.is_empty() {
            // We've reached the end of string while searching for a
            // terminating '*/'.
            let loc = *locs.last().unwrap();
            // Highlight the '/**' if it's a documentation comment, or the '/*'
            // otherwise.
            let location = make_loc(
                file_hash,
                off_set + loc.0,
                off_set + loc.0 + if loc.1 { 3 } else { 2 },
            );
            return Err(diag!(
                Syntax::InvalidDocComment,
                (location, "Unclosed block comment"),
            ));
        } else if text.starts_with("/*") {
            // We've found a (perhaps nested) multi-line comment.
            let start = get_offset(text);
            text = &text[2..];

            // Check if this is a documentation comment: '/**', but not '/***'.
            // A documentation comment cannot be nested within another comment.
            let is_doc = text.starts_with('*') && !text.starts_with("**") && locs.is_empty();

            locs.push((start, is_doc));
        } else if text.starts_with("*/") {
            // We've found a multi-line comment terminator that ends
            // our innermost nested comment.
            locs.pop().unwrap();
            text = &text[2..];

            // If this terminated our last comment, exit the loop.
            if locs.is_empty() {
                break;
            }
        } else {
            // This is a solitary '/' or '*' that isn't part of any comment delimiter.
            // Skip over it.
            text = &text[1..];
        }
    }
    if is_doc {
        Ok((Tok::Comment(CommentKind::DocBlockComment), get_offset(text)))
    } else {
        Ok((Tok::Comment(CommentKind::BlockComment), get_offset(text)))
    }
}

// Return the length of the substring matching [a-zA-Z0-9_]. Note that
// this does not do any special check for whether the first character
// starts with a number, so the caller is responsible for any additional
// checks on the first character.
fn get_name_len(text: &str) -> usize {
    text.chars()
        .position(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
        .unwrap_or(text.len())
}

fn get_decimal_number(text: &str) -> (Tok, usize) {
    let num_text_len = text
        .chars()
        .position(|c| !matches!(c, '0'..='9' | '_'))
        .unwrap_or(text.len());
    get_number_maybe_with_suffix(text, num_text_len)
}

// Return the length of the substring containing characters in [0-9a-fA-F].
fn get_hex_number(text: &str) -> (Tok, usize) {
    let num_text_len = text
        .find(|c| !matches!(c, 'a'..='f' | 'A'..='F' | '0'..='9'))
        .unwrap_or(text.len());
    get_number_maybe_with_suffix(text, num_text_len)
}

// Given the text for a number literal and the length for the characters that match to the number
// portion, checks for a typed suffix.
fn get_number_maybe_with_suffix(text: &str, num_text_len: usize) -> (Tok, usize) {
    let rest = &text[num_text_len..];
    if rest.starts_with("u8") {
        (Tok::NumTypedValue, num_text_len + 2)
    } else if rest.starts_with("u64") || rest.starts_with("u16") || rest.starts_with("u32") {
        (Tok::NumTypedValue, num_text_len + 3)
    } else if rest.starts_with("u128") || rest.starts_with("u256") {
        (Tok::NumTypedValue, num_text_len + 4)
    } else {
        // No typed suffix
        (Tok::NumValue, num_text_len)
    }
}

// Return the length of the quoted string, or None if there is no closing quote.
fn get_string_len(text: &str) -> Option<usize> {
    let mut pos = 0;
    let mut iter = text.chars();
    while let Some(chr) = iter.next() {
        if chr == '\\' {
            // Skip over the escaped character (e.g., a quote or another backslash)
            if iter.next().is_some() {
                pos += 1;
            }
        } else if chr == '"' {
            return Some(pos);
        }
        pos += 1;
    }
    None
}

fn get_name_token(name: &str) -> Tok {
    match name {
        "abort" => Tok::Abort,
        "acquires" => Tok::Acquires,
        "as" => Tok::As,
        "break" => Tok::Break,
        "const" => Tok::Const,
        "continue" => Tok::Continue,
        "copy" => Tok::Copy,
        "else" => Tok::Else,
        "false" => Tok::False,
        "fun" => Tok::Fun,
        "friend" => Tok::Friend,
        "if" => Tok::If,
        "invariant" => Tok::Invariant,
        "let" => Tok::Let,
        "loop" => Tok::Loop,
        "module" => Tok::Module,
        "move" => Tok::Move,
        "native" => Tok::Native,
        "public" => Tok::Public,
        "return" => Tok::Return,
        "script" => Tok::Script,
        "spec" => Tok::Spec,
        "struct" => Tok::Struct,
        "true" => Tok::True,
        "use" => Tok::Use,
        "while" => Tok::While,
        _ => Tok::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::{diag, find_fidelity_token, find_token, FidelityToken, Lexer, Tok, Token};
    use crate::parser::{comments::CommentKind, syntax::make_loc};
    use move_command_line_common::files::FileHash;

    use move_ir_types::location::Loc;
    use move_symbol_pool::Symbol;

    #[test]
    fn test_find_fidelity_token_trailing_comment() {
        let file_hash = FileHash::empty();
        let text = "ident /*comment*/ ,";
        let (token, length) = find_fidelity_token(file_hash, text, 0).unwrap();

        let expected_tokens = vec![
            Token::new(
                Tok::Identifier,
                Symbol::from("ident"),
                Loc::new(file_hash, 0, 5),
            ),
            Token::new(Tok::Space, Symbol::from(" "), Loc::new(file_hash, 5, 6)),
            Token::new(
                Tok::Comment(CommentKind::BlockComment),
                Symbol::from("comment"),
                Loc::new(file_hash, 6, 17),
            ),
            Token::new(Tok::Space, Symbol::from(" "), Loc::new(file_hash, 17, 18)),
        ];

        let expected = FidelityToken::new(
            Token::new(
                Tok::Identifier,
                Symbol::from("ident"),
                Loc::new(file_hash, 0, 5),
            ),
            expected_tokens,
            "ident /*comment*/ ",
        );
        assert_eq!(token, expected);
        assert_eq!(length, 18)
    }

    #[test]
    fn test_find_fidelity_token_leading_comment() {
        let file_hash = FileHash::empty();
        let text = "//comment\n ident ,";
        let (token, length) = find_fidelity_token(file_hash, text, 0).unwrap();

        let expected_tokens = vec![
            Token::new(
                Tok::Comment(CommentKind::SignleLine),
                Symbol::from("comment"),
                Loc::new(file_hash, 0, 9),
            ),
            Token::new(Tok::LF, Symbol::from("\n"), Loc::new(file_hash, 9, 10)),
            Token::new(Tok::Space, Symbol::from(" "), Loc::new(file_hash, 10, 11)),
            Token::new(
                Tok::Identifier,
                Symbol::from("ident"),
                Loc::new(file_hash, 11, 16),
            ),
            Token::new(Tok::Space, Symbol::from(" "), Loc::new(file_hash, 16, 17)),
        ];

        let expected = FidelityToken::new(
            Token::new(
                Tok::Identifier,
                Symbol::from("ident"),
                Loc::new(file_hash, 11, 16),
            ),
            expected_tokens,
            "//comment\n ident ",
        );
        assert_eq!(token, expected);
        assert_eq!(length, 17)
    }

    #[test]
    fn test_find_fidelity_token_eof() {
        let file_hash = FileHash::empty();
        let text = "//comment\n ";
        let (token, length) = find_fidelity_token(file_hash, text, 0).unwrap();

        let expected_tokens = vec![
            Token::new(
                Tok::Comment(CommentKind::SignleLine),
                Symbol::from("comment"),
                Loc::new(file_hash, 0, 9),
            ),
            Token::new(Tok::LF, Symbol::from("\n"), Loc::new(file_hash, 9, 10)),
            Token::new(Tok::Space, Symbol::from(" "), Loc::new(file_hash, 10, 11)),
            Token::new(Tok::EOF, Symbol::from(""), Loc::new(file_hash, 11, 11)),
        ];

        let expected = FidelityToken::new(
            Token::new(Tok::EOF, Symbol::from(""), Loc::new(file_hash, 11, 11)),
            expected_tokens,
            "//comment\n ",
        );
        assert_eq!(token, expected);
        assert_eq!(length, 11)
    }

    #[test]

    fn test_lexer() {
        let mut lexer = Lexer::new("hello you are great", FileHash::empty());

        let _ = lexer.advance();
        assert_eq!(
            lexer.token,
            FidelityToken::new(
                Token::new(
                    Tok::Identifier,
                    Symbol::from("hello"),
                    Loc::new(FileHash::empty(), 0, 5)
                ),
                vec![
                    Token::new(
                        Tok::Identifier,
                        Symbol::from("hello"),
                        Loc::new(FileHash::empty(), 0, 5)
                    ),
                    Token::new(
                        Tok::Space,
                        Symbol::from(" "),
                        Loc::new(FileHash::empty(), 5, 6)
                    ),
                ],
                "hello "
            )
        );

        let _ = lexer.advance();
        assert_eq!(
            lexer.token,
            FidelityToken::new(
                Token::new(
                    Tok::Identifier,
                    Symbol::from("you"),
                    Loc::new(FileHash::empty(), 6, 9)
                ),
                vec![
                    Token::new(
                        Tok::Identifier,
                        Symbol::from("you"),
                        Loc::new(FileHash::empty(), 6, 9)
                    ),
                    Token::new(
                        Tok::Space,
                        Symbol::from(" "),
                        Loc::new(FileHash::empty(), 9, 10)
                    ),
                ],
                "you "
            )
        )
    }
    #[test]
    fn test_find_comment() {
        assert_eq!(
            find_token(FileHash::empty(), "/* foo */ sdasd", 0),
            Ok((Tok::Comment(CommentKind::BlockComment), 9))
        );
        assert_eq!(
            find_token(
                FileHash::empty(),
                "/* comment /* embedded comment */  */ foo",
                0
            ),
            Ok((Tok::Comment(CommentKind::BlockComment), 37))
        );
        assert_eq!(
            find_token(FileHash::empty(), "/** foo */ bar", 0),
            Ok((Tok::Comment(CommentKind::DocBlockComment), 10))
        );
        assert_eq!(
            find_token(FileHash::empty(), "// foo bar\n new line", 0),
            Ok((Tok::Comment(CommentKind::SignleLine), 10))
        );
        assert_eq!(
            find_token(FileHash::empty(), "/// foo bar\n new line", 0),
            Ok((Tok::Comment(CommentKind::DocComment), 11))
        );
        let location = make_loc(FileHash::empty(), 0, 2);
        assert_eq!(
            find_token(FileHash::empty(), "/* unclosed comments", 0),
            Err(diag!(
                Syntax::InvalidDocComment,
                (location, "Unclosed block comment"),
            ))
        );

        let location2 = make_loc(FileHash::empty(), 0, 3);
        assert_eq!(
            find_token(FileHash::empty(), "/** unclosed comments", 0),
            Err(diag!(
                Syntax::InvalidDocComment,
                (location2, "Unclosed block comment"),
            ))
        )
    }
}
