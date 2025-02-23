use std::process::exit;

use crate::tokenizer::{Position, Token, TokenKind};

#[derive(Debug)]
pub struct Ast {
    pub expressions: Vec<Expression>,
}

#[derive(Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub position: Position,
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expression").field("kind", &self.kind).finish()
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Float(f64),
    Integer(i64),
    String(String),
    Character(char),
    Boolean(bool),
    Null,
    Variable(String),
    List(Vec<Expression>),
    Unary {
        operator: Operator,
        right: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: Operator,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    VariableDefinition {
        name: String,
        initializer: Box<Option<Expression>>,
    },
    Assignment {
        name: String,
        value: Box<Expression>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<String>,
        body: Box<Expression>,
    },
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Block {
        expressions: Vec<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        elseif_branches: Vec<(Expression, Expression)>,
        else_branch: Box<Option<Expression>>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    For {
        initializer: Box<Expression>,
        condition: Box<Expression>,
        increment: Box<Expression>,
        body: Box<Expression>,
    },
    Return {
        value: Box<Option<Expression>>,
    },
    Break {
        value: Box<Option<Expression>>,
    },
    Continue {
        value: Box<Option<Expression>>,
    },
    ListIndex {
        list: Box<Expression>,
        index: Box<Expression>,
    },
    Import(String),
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    MinusUnary,
    Index,
    Call,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
enum OperatorPrecedence {
    Equality,
    Comparative,
    Additive,
    Multiplicative,
    Prefix,
    Postfix,
}

#[derive(Debug, PartialEq)]
enum OperatorAssociativity {
    Left,
    Right,
}

fn get_operator(token: &Token, prefix: bool) -> Option<Operator> {
    match token.kind {
        TokenKind::BangEqual => Some(Operator::BangEqual),
        TokenKind::EqualEqual => Some(Operator::EqualEqual),
        TokenKind::Less => Some(Operator::Less),
        TokenKind::LessEqual => Some(Operator::LessEqual),
        TokenKind::Greater => Some(Operator::Greater),
        TokenKind::GreaterEqual => Some(Operator::GreaterEqual),
        TokenKind::Plus => Some(Operator::Plus),
        TokenKind::Minus => {
            if prefix {
                Some(Operator::MinusUnary)
            } else {
                Some(Operator::Minus)
            }
        }
        TokenKind::Star => Some(Operator::Star),
        TokenKind::Slash => Some(Operator::Slash),
        TokenKind::Bang => Some(Operator::Bang),
        TokenKind::BracketLeft => {
            if prefix {
                None
            } else {
                Some(Operator::Index)
            }
        }
        TokenKind::ParenLeft => {
            if prefix {
                None
            } else {
                Some(Operator::Call)
            }
        }
        _ => None,
    }
}

fn is_prefix_operator(operator: &Operator) -> bool {
    match operator {
        Operator::Bang | Operator::MinusUnary => true,
        _ => false,
    }
}

fn is_infix_operator(operator: &Operator) -> bool {
    match operator {
        Operator::BangEqual
        | Operator::EqualEqual
        | Operator::Less
        | Operator::LessEqual
        | Operator::Greater
        | Operator::GreaterEqual
        | Operator::Plus
        | Operator::Minus
        | Operator::Star
        | Operator::Slash => true,
        _ => false,
    }
}

fn is_postfix_operator(operator: &Operator) -> bool {
    match operator {
        Operator::Index | Operator::Call => true,
        _ => false,
    }
}

fn get_precedence(operator: &Operator) -> OperatorPrecedence {
    match operator {
        Operator::BangEqual | Operator::EqualEqual => OperatorPrecedence::Equality,
        Operator::Less | Operator::LessEqual | Operator::Greater | Operator::GreaterEqual => {
            OperatorPrecedence::Comparative
        }
        Operator::Plus | Operator::Minus => OperatorPrecedence::Additive,
        Operator::Star | Operator::Slash => OperatorPrecedence::Multiplicative,
        Operator::Bang | Operator::MinusUnary => OperatorPrecedence::Prefix,
        Operator::Index | Operator::Call => OperatorPrecedence::Postfix,
    }
}

fn next_precedence(precedence: OperatorPrecedence) -> OperatorPrecedence {
    match precedence {
        OperatorPrecedence::Equality => OperatorPrecedence::Comparative,
        OperatorPrecedence::Comparative => OperatorPrecedence::Additive,
        OperatorPrecedence::Additive => OperatorPrecedence::Multiplicative,
        OperatorPrecedence::Multiplicative => OperatorPrecedence::Prefix,
        OperatorPrecedence::Prefix => OperatorPrecedence::Postfix,
        OperatorPrecedence::Postfix => panic!("No next precedence for postfix operators"),
    }
}

fn get_associativity(operator: &Operator) -> OperatorAssociativity {
    match operator {
        Operator::BangEqual
        | Operator::EqualEqual
        | Operator::Less
        | Operator::LessEqual
        | Operator::Greater
        | Operator::GreaterEqual
        | Operator::Plus
        | Operator::Minus
        | Operator::Star
        | Operator::Slash => OperatorAssociativity::Left,
        Operator::Bang | Operator::MinusUnary | Operator::Index | Operator::Call => {
            OperatorAssociativity::Right
        }
    }
}

type Tokens = std::iter::Peekable<std::vec::IntoIter<Token>>;

fn panic_unexpected_token(token: &Token, expected: &str) -> ! {
    eprintln!(
        "Unexpected token: {:?} at {}, expected {}",
        token.kind, token.position, expected
    );
    exit(1);
}

// Operator precedence parsing
fn parse_expression(tokens: &mut Tokens) -> Expression {
    parse_expression_1(tokens, OperatorPrecedence::Equality)
}

fn parse_expression_1(tokens: &mut Tokens, min_precedence: OperatorPrecedence) -> Expression {
    let mut lhs = match tokens.peek() {
        Some(token) => {
            if let Some(operator) = get_operator(token, true) {
                if is_prefix_operator(&operator) {
                    let token = tokens.next().unwrap();
                    let right = Box::new(parse_expression_1(tokens, get_precedence(&operator)));
                    Expression {
                        kind: ExpressionKind::Unary { operator, right },
                        position: token.position,
                    }
                } else {
                    panic_unexpected_token(token, "prefix operator");
                }
            } else {
                parse_primary(tokens)
            }
        }
        None => {
            eprintln!("Unexpected end of input, expected start of expression");
            exit(1);
        }
    };

    while let Some(token) = tokens.peek() {
        match get_operator(token, false) {
            Some(operator) => match operator {
                operator if is_postfix_operator(&operator) => match operator {
                    Operator::Index => {
                        let token = tokens.next().unwrap();
                        let index = parse_expression(tokens);
                        expect_token(tokens, TokenKind::BracketRight);
                        lhs = Expression {
                            kind: ExpressionKind::ListIndex {
                                list: Box::new(lhs),
                                index: Box::new(index),
                            },
                            position: token.position,
                        };
                    }
                    Operator::Call => {
                        let token = tokens.next().unwrap();
                        let arguments = parse_expression_list_until(
                            tokens,
                            TokenKind::Comma,
                            TokenKind::ParenRight,
                        );
                        expect_token(tokens, TokenKind::ParenRight);
                        lhs = Expression {
                            kind: ExpressionKind::FunctionCall {
                                callee: Box::new(lhs),
                                arguments,
                            },
                            position: token.position,
                        };
                    }
                    _ => panic_unexpected_token(token, "postfix operator"),
                },
                operator if is_infix_operator(&operator) => {
                    let precedence = get_precedence(&operator);
                    if precedence < min_precedence {
                        break;
                    }
                    let token = tokens.next().unwrap();
                    let right_precedence =
                        if get_associativity(&operator) == OperatorAssociativity::Left {
                            next_precedence(precedence.clone())
                        } else {
                            precedence.clone()
                        };
                    let mut rhs = parse_expression_1(tokens, right_precedence);
                    while let Some(token) = tokens.peek() {
                        let next_operator = get_operator(token, false);
                        if next_operator.is_none() {
                            break;
                        }
                        let next_operator = next_operator.unwrap();
                        let next_precedence = get_precedence(&next_operator);
                        if next_precedence > precedence {
                            rhs = parse_expression_1(tokens, next_precedence);
                        } else {
                            break;
                        }
                    }
                    lhs = Expression {
                        kind: ExpressionKind::Binary {
                            left: Box::new(lhs),
                            operator,
                            right: Box::new(rhs),
                        },
                        position: token.position,
                    };
                }
                _ => break,
            },
            None => break,
        }
    }

    lhs
}

fn parse_primary(tokens: &mut Tokens) -> Expression {
    let token = tokens.next().unwrap();
    match token.kind {
        TokenKind::Float(f) => Expression {
            kind: ExpressionKind::Float(f),
            position: token.position,
        },
        TokenKind::Integer(i) => Expression {
            kind: ExpressionKind::Integer(i),
            position: token.position,
        },
        TokenKind::String(s) => Expression {
            kind: ExpressionKind::String(s),
            position: token.position,
        },
        TokenKind::Character(c) => Expression {
            kind: ExpressionKind::Character(c),
            position: token.position,
        },
        TokenKind::True => Expression {
            kind: ExpressionKind::Boolean(true),
            position: token.position,
        },
        TokenKind::False => Expression {
            kind: ExpressionKind::Boolean(false),
            position: token.position,
        },
        TokenKind::Null => Expression {
            kind: ExpressionKind::Null,
            position: token.position,
        },
        TokenKind::Identifier(name) => match tokens.peek() {
            Some(&Token {
                kind: TokenKind::Equal,
                ..
            }) => {
                let token = expect_token(tokens, TokenKind::Equal);
                let value = parse_expression(tokens);
                Expression {
                    kind: ExpressionKind::Assignment {
                        name,
                        value: Box::new(value),
                    },
                    position: token.position,
                }
            }
            _ => Expression {
                kind: ExpressionKind::Variable(name),
                position: token.position,
            },
        },
        TokenKind::ParenLeft => {
            let expr = parse_expression(tokens);
            expect_token(tokens, TokenKind::ParenRight);
            Expression {
                kind: ExpressionKind::Grouping(Box::new(expr)),
                position: token.position,
            }
        }
        TokenKind::BracketLeft => {
            let elements =
                parse_expression_list_until(tokens, TokenKind::Comma, TokenKind::BracketRight);
            expect_token(tokens, TokenKind::BracketRight);
            Expression {
                kind: ExpressionKind::List(elements),
                position: token.position,
            }
        }
        TokenKind::Var => parse_variable_definition(tokens, token.position),
        TokenKind::Func => parse_function_definition(tokens, token.position),
        TokenKind::If => parse_if(tokens, token.position),
        TokenKind::While => parse_while(tokens, token.position),
        TokenKind::For => parse_for(tokens, token.position),
        TokenKind::BraceLeft => {
            let expressions =
                parse_expression_list_until(tokens, TokenKind::SemiColon, TokenKind::BraceRight);
            expect_token(tokens, TokenKind::BraceRight);
            Expression {
                kind: ExpressionKind::Block { expressions },
                position: token.position,
            }
        }
        TokenKind::Return => Expression {
            kind: ExpressionKind::Return {
                value: Box::new(parse_optional_expression(tokens)),
            },
            position: token.position,
        },
        TokenKind::Break => Expression {
            kind: ExpressionKind::Break {
                value: Box::new(parse_optional_expression(tokens)),
            },
            position: token.position,
        },
        TokenKind::Continue => Expression {
            kind: ExpressionKind::Continue {
                value: Box::new(parse_optional_expression(tokens)),
            },
            position: token.position,
        },
        TokenKind::Import => {
            let path = match expect_token(tokens, TokenKind::String(String::new())) {
                token => match token.kind {
                    TokenKind::String(name) => name,
                    _ => {
                        panic_unexpected_token(&token, "string");
                    }
                },
            };
            Expression {
                kind: ExpressionKind::Import(path),
                position: token.position,
            }
        }
        _ => {
            panic_unexpected_token(&token, "primary expression");
        }
    }
}

fn expect_token(tokens: &mut Tokens, expected: TokenKind) -> Token {
    match tokens.next() {
        Some(token) => {
            // std::mem::discriminant is used to compare enum variants without comparing their data
            if std::mem::discriminant(&token.kind) != std::mem::discriminant(&expected) {
                panic_unexpected_token(&token, format!("{:?}", expected).as_str());
            }
            token
        }
        None => {
            eprintln!("Unexpected end of input, expected {:?}", expected);
            exit(1);
        },
    }
}

fn parse_optional_expression(tokens: &mut Tokens) -> Option<Expression> {
    match tokens.peek() {
        Some(&Token {
            kind: TokenKind::SemiColon,
            ..
        }) => None,
        _ => Some(parse_expression(tokens)),
    }
}

fn parse_expression_list_until(
    tokens: &mut Tokens,
    separator: TokenKind,
    end: TokenKind,
) -> Vec<Expression> {
    let mut expressions = Vec::<Expression>::new();
    let mut closed = false;

    while let Some(token) = tokens.peek() {
        if token.kind == end {
            closed = true;
            break;
        } else if token.kind == separator {
            tokens.next();
        } else {
            expressions.push(parse_expression(tokens));
        }
    }
    if closed == false && tokens.peek().is_none() {
        eprintln!("Unexpected end of input, expected {:?}", end);
        exit(1);
    }

    expressions
}

fn parse_variable_definition(tokens: &mut Tokens, position: Position) -> Expression {
    let name = match expect_token(tokens, TokenKind::Identifier(String::new())) {
        token => match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                panic_unexpected_token(&token, "identifier");
            }
        },
    };

    let initializer = if let Some(&Token {
        kind: TokenKind::Equal,
        ..
    }) = tokens.peek()
    {
        tokens.next();
        Some(parse_expression(tokens))
    } else {
        None
    };

    Expression {
        kind: ExpressionKind::VariableDefinition {
            name,
            initializer: Box::new(initializer),
        },
        position,
    }
}

fn parse_function_definition(tokens: &mut Tokens, position: Position) -> Expression {
    let name = match expect_token(tokens, TokenKind::Identifier(String::new())) {
        token => match token.kind {
            TokenKind::Identifier(name) => name,
            _ => {
                panic_unexpected_token(&token, "identifier");
            }
        },
    };

    expect_token(tokens, TokenKind::ParenLeft);
    let mut parameters = Vec::<String>::new();
    while let Some(token) = tokens.peek() {
        match &token.kind {
            TokenKind::ParenRight => {
                tokens.next();
                break;
            }
            TokenKind::Comma => {
                tokens.next();
            }
            TokenKind::Identifier(name) => {
                parameters.push(name.to_owned());
                tokens.next();
            }
            _ => {
                panic_unexpected_token(token, "identifier");
            }
        }
    }

    let body = Box::new(parse_expression(tokens));
    Expression {
        kind: ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
        },
        position,
    }
}

fn parse_if(tokens: &mut Tokens, position: Position) -> Expression {
    let condition = Box::new(parse_expression(tokens));
    let then_branch = Box::new(parse_expression(tokens));
    let mut else_branch = Box::new(None);

    let mut elseif_branches = Vec::new();
    while let Some(&Token {
        kind: TokenKind::Else,
        ..
    }) = tokens.peek()
    {
        expect_token(tokens, TokenKind::Else);
        match tokens.peek() {
            Some(&Token {
                kind: TokenKind::If,
                ..
            }) => {
                expect_token(tokens, TokenKind::If);
                let condition = parse_expression(tokens);
                let body = parse_expression(tokens);
                elseif_branches.push((condition, body));
            }
            Some(_) => {
                else_branch = Box::new(Some(parse_expression(tokens)));
            }
            None => {
                eprintln!("Unexpected end of input, expected if or expression");
                exit(1);
            },
        }
    }

    Expression {
        kind: ExpressionKind::If {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        },
        position,
    }
}

fn parse_while(tokens: &mut Tokens, position: Position) -> Expression {
    let condition = Box::new(parse_expression(tokens));
    let body = Box::new(parse_expression(tokens));
    Expression {
        kind: ExpressionKind::While { condition, body },
        position,
    }
}

fn parse_for(tokens: &mut Tokens, position: Position) -> Expression {
    expect_token(tokens, TokenKind::ParenLeft);
    let initializer = Box::new(parse_expression(tokens));
    expect_token(tokens, TokenKind::SemiColon);
    let condition = Box::new(parse_expression(tokens));
    expect_token(tokens, TokenKind::SemiColon);
    let increment = Box::new(parse_expression(tokens));
    expect_token(tokens, TokenKind::ParenRight);

    let body = Box::new(parse_expression(tokens));
    Expression {
        kind: ExpressionKind::For {
            initializer,
            condition,
            increment,
            body,
        },
        position,
    }
}

pub fn parse(tokens: Vec<Token>) -> Ast {
    let mut tokens: Tokens = tokens.into_iter().peekable();
    let mut expressions = Vec::<Expression>::new();

    while let Some(_) = tokens.peek() {
        let expression = parse_expression(&mut tokens);
        expressions.push(expression);

        match tokens.peek() {
            Some(&Token {
                kind: TokenKind::SemiColon,
                ..
            }) => {
                tokens.next();
            }
            Some(_) => {
                // panic!("Unexpected token: {:?}, expected ;", tokens.next().unwrap());
            }
            None => {
                break;
            }
        }
    }

    Ast { expressions }
}
