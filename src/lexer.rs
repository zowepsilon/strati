use std::{fmt::Display, iter::Peekable};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
    // structure
    ParenBlock(Vec<Token>),
    BraceBlock(Vec<Token>),

    Colon,
    Comma,
    Dot,
    ThinArrow,

    Assign,

    // literals
    Identifier(String),
    String(String),
    Integer(String),

    // keywords
    Let,
    Fun,
    Fn,
    Struct,
    Underscore,
    Meta,

    // specials
    NewLine,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Position { pub line: usize, pub column: usize }

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub data: TokenData,
    pub pos: Position,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<std::str::Chars<'a>>,
    ok: bool,
    current: Position,
}

impl<'a> Lexer<'a> {
    fn token(&self, data: TokenData) -> Token {
        Token {
            data,
            pos: self.current,
        }
    }

    fn new_line(&mut self) {
        self.current.column = 0;
        self.current.line += 1;
    }

    fn string(&mut self) -> Option<Token> {
        let mut content = String::new();
        let start_position = self.current;

        loop {
            self.current.column += 1;
            match self.chars.next()? {
                '"' => {
                    return Some(Token {
                        data: TokenData::String(content),
                        pos: start_position,
                    })
                }
                '\n' => {
                    self.new_line();
                    content.push('\n');
                }
                ch => content.push(ch),
            };
        }
    }

    fn identifier(&mut self, start: char) -> Token {
        let mut content = start.to_string();
        let start_position = self.current;

        loop { match self.chars.peek() {
            Some(ch) if ch.is_alphanumeric() || *ch == '_' => {
                self.current.column += 1;
                content.push(
                    self.chars
                        .next()
                        .expect("self.chars was peeked"),
                );
            }
            _ => {
                return Token {
                    data: match content.as_str() {
                        "let" => TokenData::Let,
                        "fun" => TokenData::Fun,
                        "fn" => TokenData::Fn,
                        "struct" => TokenData::Struct,
                        "_" => TokenData::Underscore,
                        "meta" => TokenData::Meta,
                        _ => TokenData::Identifier(content),
                    },
                    pos: start_position,
                }
            }
        }}
    }

    fn digit_sequence(&mut self, start: &mut String) {
        while let Some('0'..='9') = self.chars.peek() {
            self.current.column += 1;
            start.push(self.chars.next().expect("self.chars was peeked"));
        }
    }

    fn number(&mut self, start: char) -> Token {
        let mut content = start.to_string(); 

        let start_position = self.current;

        self.digit_sequence(&mut content);

        Token {
            data: TokenData::Integer(content),
            pos: start_position,
        }
    }

    pub fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
            current: Position {
                line: 1,
                column: 0
            },
            ok: true,
        }
    }

    pub fn lex(&mut self) -> Option<Vec<Token>> {
        let mut tokens = vec![];
        for tok in self.by_ref() {
            tokens.push(tok)
        }
        
        if self.ok {
            Some(tokens)
        } else {
            eprintln!("{:?}", self.current);
            None
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        use TokenData::*;

        macro_rules! two_char_token {
            ($second_char:expr, $short_tok:expr, $long_tok:expr) => {
                match self.chars.peek() {
                    Some($second_char) => {
                        self.current.column += 1;
                        self.chars.next();
                        let token = $long_tok;

                        token
                    }
                    _ => $short_tok,
                }
            };
        }

        macro_rules! block {
            ($block:ident, $end: pat) => {{
                let mut sub = Lexer {
                    chars: self.chars.clone(),
                    current: self.current,
                    ok: true,
                };

                match sub.lex() {
                    Some(tokens) => {
                        self.current = sub.current;
                        self.chars = sub.chars;

                        match self.chars.next() {
                            Some($end) => self.token($block(tokens)),
                            _ => {
                                self.ok = false;
                                return None;
                            },
                        }
                    },
                    None => {
                        self.ok = false;
                        return None;
                    },
                }

            }}
        }

        self.current.column += 1;

        if let Some(')' | '}') = self.chars.peek() {
            return None;
        }

        match self.chars.next() {
            Option::Some(ch) => Option::Some(match ch {
                '(' => block!(ParenBlock, ')'),
                '{' => block!(BraceBlock, '}'),
                ')' | '}' => unreachable!("end of block should have been detected"),
                ':' => self.token(Colon),
                ',' => self.token(Comma),
                '.' => self.token(Dot),
                '=' => self.token(Assign),
                '-' => two_char_token!(
                    '>', 
                    {
                        self.ok = false;
                        return None;
                    },
                    self.token(ThinArrow)
                ),
                '/' => two_char_token! {
                    '/',
                    {
                        self.ok = false;
                        return None;
                    },
                    {
                        loop {
                            if matches!(self.chars.next(), Option::Some('\n') | None) {
                                self.new_line();
                                break;
                            }
                        }
                        self.next()?
                    }
                },
                '"' => match self.string() {
                    Some(token) => token,
                    None => {
                        self.ok = false;
                        return None;
                    }
                },
                '\n' => {
                    self.new_line();
                    self.token(NewLine)
                }
                letter if letter.is_alphabetic() || letter == '_' => self.identifier(letter),
                letter if letter.is_numeric() => self.number(letter),
                ' ' | '\t' => self.next()?,
                other => {
                    dbg!(other);
                    self.ok = false;
                    return None;
                }
            }),
            None => None,
        }
    }
}

impl Display for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenData as TD;
        match self {
            TD::ParenBlock(inner) => {
                write!(f, "(")?;
                for tok in inner { write!(f, "{}", tok)?; }
                write!(f, ")")
            }
            TD::BraceBlock(inner) => {
                write!(f, "{{")?;
                for tok in inner { write!(f, "{}", tok)?; }
                write!(f, "}}")
            }
            TD::Colon => write!(f, "{}", ": "),
            TD::Comma => write!(f, "{}", ", "),
            TD::Dot => write!(f, "{}", "."),
            TD::Assign => write!(f, "{}", "= "),
            TD::Let => write!(f, "{}", "let "),
            TD::Fun => write!(f, "{}", "fun "),
            TD::Fn => write!(f, "{}", "fn "),
            TD::Struct => write!(f, "{}", "struct "),
            TD::Underscore => write!(f, "{}", "_"),
            TD::Meta => write!(f, "{}", "meta"),
            TD::NewLine => write!(f, "{}", "\n"),
            TD::ThinArrow => write!(f, "{}", " -> "),
            TD::Identifier(name) => write!(f, "{name} "),
            TD::String(content) => write!(f, "\"{content}\" "),
            TD::Integer(i) => write!(f, "{i} "),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}
