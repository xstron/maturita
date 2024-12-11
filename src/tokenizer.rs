#[derive(Debug, PartialEq)]
pub enum Token {
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
}

pub fn tokenize(src: &str) -> Vec<Token> {
    let mut chars = src.chars().peekable();
    let mut tokens = Vec::<Token>::new();

    while chars.peek().is_some() {
        let c = chars.next().unwrap();
        match c {
            '(' => tokens.push(Token::ParenLeft),
            ')' => tokens.push(Token::ParenRight),
            '[' => tokens.push(Token::BracketLeft),
            ']' => tokens.push(Token::BracketRight),
            '{' => tokens.push(Token::BraceLeft),
            '}' => tokens.push(Token::BraceRight),
            '=' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    tokens.push(Token::EqualEqual);
                }
                _ => tokens.push(Token::Equal),
            },
            '!' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    tokens.push(Token::BangEqual);
                }
                _ => tokens.push(Token::Bang),
            },
            '<' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    tokens.push(Token::LessEqual);
                }
                _ => tokens.push(Token::Less),
            },
            '>' => match chars.peek() {
                Some(&'=') => {
                    let _ = chars.next();
                    tokens.push(Token::GreaterEqual);
                }
                _ => tokens.push(Token::Greater),
            },
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            ',' => tokens.push(Token::Comma),
            ';' => tokens.push(Token::SemiColon),
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            ident.push(chars.next().unwrap());
                        }
                        _ => break,
                    }
                }
                tokens.push(Token::Identifier(ident));
            }
            '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                while let Some(&c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            num.push(chars.next().unwrap());
                        }
                        '.' => {
                            num.push(chars.next().unwrap());
                            while let Some(&c) = chars.peek() {
                                match c {
                                    '0'..='9' => {
                                        num.push(chars.next().unwrap());
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
                    tokens.push(Token::Float(num.parse().unwrap()));
                } else {
                    tokens.push(Token::Integer(num.parse().unwrap()));
                }
            }
            '"' => {
                let mut string = String::new();
                while let Some(&c) = chars.peek() {
                    match c {
                        '"' => {
                            chars.next();
                            break;
                        }
                        '\\' => {
                            chars.next();
                            match chars.next().unwrap() {
                                'n' => string.push('\n'),
                                'r' => string.push('\r'),
                                't' => string.push('\t'),
                                '\\' => string.push('\\'),
                                '"' => string.push('"'),
                                _ => panic!("Unknown escape character: {}", c),
                            }
                        }
                        _ => {
                            string.push(chars.next().unwrap());
                        }
                    }
                }
                tokens.push(Token::String(string));
            }
            '\'' => {
                let c = chars.next().unwrap();
                let _ = chars.next().unwrap();
                tokens.push(Token::Character(c));
            }
            ' ' | '\n' | '\r' | '\t' => {}
            _ => {
                panic!("Untokenizable character: {}", c);
            }
        }
    }

    tokens
}
