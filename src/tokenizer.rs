#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    BraceLeft,
    BraceRight,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Comma,
    SemiColon,
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Character(char),
    // Keywords
    Var,
    Func,
    Return,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    True,
    False,
    Null,
}

pub fn tokenize(src: &str) -> Vec<Token> {
    let mut chars = src.chars().peekable();
    let mut tokens = Vec::<Token>::new();
    let mut line: usize = 1;
    let mut column: usize = 1;

    while let Some(c) = chars.next() {
        let start_column = column;
        let kind;
        match c {
            '(' => kind = TokenKind::ParenLeft,
            ')' => kind = TokenKind::ParenRight,
            '[' => kind = TokenKind::BracketLeft,
            ']' => kind = TokenKind::BracketRight,
            '{' => kind = TokenKind::BraceLeft,
            '}' => kind = TokenKind::BraceRight,
            '=' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    column += 1;
                    kind = TokenKind::EqualEqual;
                }
                _ => kind = TokenKind::Equal,
            },
            '!' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    column += 1;
                    kind = TokenKind::BangEqual;
                }
                _ => kind = TokenKind::Bang,
            },
            '<' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    column += 1;
                    kind = TokenKind::LessEqual;
                }
                _ => kind = TokenKind::Less,
            },
            '>' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    column += 1;
                    kind = TokenKind::GreaterEqual;
                }
                _ => kind = TokenKind::Greater,
            },
            '+' => kind = TokenKind::Plus,
            '-' => kind = TokenKind::Minus,
            '*' => kind = TokenKind::Star,
            '/' => kind = TokenKind::Slash,
            ',' => kind = TokenKind::Comma,
            ';' => kind = TokenKind::SemiColon,
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            ident.push(chars.next().unwrap());
                            column += 1;
                        }
                        _ => break,
                    }
                }
                match ident.as_str() {
                    "var" => kind = TokenKind::Var,
                    "func" => kind = TokenKind::Func,
                    "return" => kind = TokenKind::Return,
                    "if" => kind = TokenKind::If,
                    "else" => kind = TokenKind::Else,
                    "while" => kind = TokenKind::While,
                    "for" => kind = TokenKind::For,
                    "break" => kind = TokenKind::Break,
                    "continue" => kind = TokenKind::Continue,
                    "true" => kind = TokenKind::True,
                    "false" => kind = TokenKind::False,
                    "null" => kind = TokenKind::Null,
                    _ => kind = TokenKind::Identifier(ident),
                }
            }
            '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            num.push(chars.next().unwrap());
                            column += 1;
                        }
                        '.' => {
                            num.push(chars.next().unwrap());
                            column += 1;
                            while let Some(&c) = chars.peek() {
                                match c {
                                    '0'..='9' => {
                                        num.push(chars.next().unwrap());
                                        column += 1;
                                    }
                                    _ => break,
                                }
                            }
                            break;
                        }
                        _ => break,
                    }
                }
                if num.contains('.') {
                    kind = TokenKind::Float(num.parse().unwrap());
                } else {
                    kind = TokenKind::Integer(num.parse().unwrap());
                }
            }
            '"' => {
                let mut string = String::new();
                while let Some(&c) = chars.peek() {
                    match c {
                        '"' => {
                            chars.next();
                            column += 1;
                            break;
                        }
                        '\\' => {
                            chars.next();
                            column += 1;
                            match chars.next().unwrap() {
                                'n' => string.push('\n'),
                                'r' => string.push('\r'),
                                't' => string.push('\t'),
                                '\\' => string.push('\\'),
                                '"' => string.push('"'),
                                _ => panic!("Unknown escape character: {}", c),
                            }
                            column += 1;
                        }
                        _ => {
                            string.push(chars.next().unwrap());
                            column += 1;
                        }
                    }
                }
                kind = TokenKind::String(string);
            }
            '\'' => {
                let c = chars.next().unwrap();
                column += 1;
                let _ = chars.next().unwrap();
                column += 1;
                kind = TokenKind::Character(c);
            }
            '\n' => {
                line += 1;
                column = 1;
                continue;
            }
            ' ' | '\r' | '\t' => {
                column += 1;
                continue;
            }
            '#' => {
                while let Some(&c) = chars.peek() {
                    if c == '\n' {
                        break;
                    }
                    chars.next();
                    column += 1;
                }
                continue;
            }
            _ => {
                panic!("Untokenizable character: {}", c);
            }
        }

        tokens.push(Token {
            kind,
            line,
            column: start_column,
        });

        column += 1;
    }

    tokens
}
