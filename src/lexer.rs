use std::{fmt::Display, iter::Peekable};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenData {
    // structure
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    Colon,
    Comma,
    Dot,
    ThinArrow,
    FatArrow,

    Assign,

    // literals
    Identifier(String),
    String(String),
    Integer(String),

    // keywords
    Let,
    Fun,
    Struct,

    // specials
    NewLine,
    Unknown(char),
}

#[derive(Debug, Clone, Copy)]
pub struct Position { pub line: usize, pub column: usize }

#[derive(Debug, Clone)]
pub struct Token {
    pub data: TokenData,
    pub pos: Position,
}

#[derive(Debug, Clone)]
pub enum LexingErrorKind {
    MisplacedCharacter(char),
    UnTerminatedString(Position),
}

#[derive(Debug, Clone)]
pub struct LexingError {
    pub kind: LexingErrorKind,
    pub position: Position,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    chars: Peekable<std::str::Chars<'a>>,
    errors: Vec<LexingError>,

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

    fn error(&mut self, kind: LexingErrorKind) {
        self.errors.push(LexingError {
            kind,
            position: self.current,
        });
    }

    fn string(&mut self) -> Result<Token, LexingErrorKind> {
        let mut content = String::new();
        let start_position = self.current;

        loop {
            self.current.column += 1;
            match self.chars.next() {
                None => return Err(LexingErrorKind::UnTerminatedString(start_position)),
                Some('"') => {
                    return Ok(Token {
                        data: TokenData::String(content),
                        pos: start_position,
                    })
                }
                Some('\n') => {
                    self.new_line();
                    content.push('\n');
                }
                Some(ch) => content.push(ch),
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
                        "struct" => TokenData::Struct,
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
            errors: Vec::new(),
        }
    }

    pub fn lex(mut self) -> Result<Vec<Token>, Vec<LexingError>> {
        let mut tokens = vec![];
        for tok in self.by_ref() {
            tokens.push(tok)
        }
        
        if self.errors.is_empty() {
            Ok(tokens)
        } else {
            Err(self.errors)
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

        self.current.column += 1;

        match self.chars.next() {
            Option::Some(ch) => Option::Some(match ch {
                '(' => self.token(LeftParen),
                ')' => self.token(RightParen),
                '{' => self.token(LeftBrace),
                '}' => self.token(RightBrace),
                ':' => self.token(Colon),
                ',' => self.token(Comma),
                '.' => self.token(Dot),
                '-' => two_char_token!(
                    '>', 
                    {
                        self.error(LexingErrorKind::MisplacedCharacter('-'));
                        self.token(TokenData::Unknown('-')) 
                    },
                    self.token(ThinArrow)
                ),
                '/' => two_char_token! {
                    '/',
                    {
                        self.error(LexingErrorKind::MisplacedCharacter('/'));
                        self.token(TokenData::Unknown('/'))
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
                '=' => two_char_token!(
                    '>',
                    self.token(Assign),
                    self.token(FatArrow)
                ),
                '"' => match self.string() {
                    Ok(token) => token,
                    Err(err) => {
                        self.error(err);
                        self.next()?
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
                    self.error(LexingErrorKind::MisplacedCharacter(other));
                    self.token(Unknown(other))
                }
            }),
            None => None,
        }
    }
}

impl Display for TokenData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenData::*;
        let mut done = true;

        write!(
            f,
            "{}",
            match self {
                LeftParen => "(",
                RightParen => ")",
                LeftBrace => "{ ",
                RightBrace => "}",
                Colon => ": ",
                Comma => ", ",
                Dot => ".",
                Assign => "= ",
                Let => "let ",
                Fun => "fun ",
                Struct => "struct ",
                NewLine => "\n",
                ThinArrow => " -> ",
                FatArrow => " => ",
                Identifier(_) | String(_) | Integer(_) | Unknown(_) => {
                    done = false;
                    ""
                }
            }
        )?;
        
        if !done {
            match self {
                Identifier(name) => write!(f, "{name} ")?,
                String(content) => write!(f, "\"{content}\" ")?,
                Integer(i) => write!(f, "{i} ")?,
                Unknown(c) => write!(f, " `{c}` ")?,
                _ => ()
            }
        }
        
        Ok(())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}
