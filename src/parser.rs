use crate::tokenizer::{Token, TokenKind};

#[derive(Debug)]
pub struct Ast {
    pub expressions: Vec<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Float(f64),
    Integer(i64),
    String(String),
    Character(char),
    Boolean(bool),
    Variable(String),
    Unary {
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Grouping(Box<Expression>),
    VariableDeclaration {
        name: String,
        initializer: Box<Option<Expression>>,
    },
    Assignment {
        name: String,
        value: Box<Expression>,
    },
    FunctionCall {
        name: String,
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
}

#[derive(Debug)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BangEqual,
    EqualEqual,
}

fn get_operator(token: &Token) -> Option<BinaryOperator> {
    match token.kind {
        TokenKind::Plus => Some(BinaryOperator::Plus),
        TokenKind::Minus => Some(BinaryOperator::Minus),
        TokenKind::Star => Some(BinaryOperator::Star),
        TokenKind::Slash => Some(BinaryOperator::Slash),
        TokenKind::Greater => Some(BinaryOperator::Greater),
        TokenKind::GreaterEqual => Some(BinaryOperator::GreaterEqual),
        TokenKind::Less => Some(BinaryOperator::Less),
        TokenKind::LessEqual => Some(BinaryOperator::LessEqual),
        TokenKind::BangEqual => Some(BinaryOperator::BangEqual),
        TokenKind::EqualEqual => Some(BinaryOperator::EqualEqual),
        _ => None,
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum OperatorPrecedence {
    Equality,
    Comparative,
    Additive,
    Multiplicative,
}

fn get_precedence(operator: &BinaryOperator) -> OperatorPrecedence {
    match operator {
        BinaryOperator::Plus | BinaryOperator::Minus => OperatorPrecedence::Additive,
        BinaryOperator::Star | BinaryOperator::Slash => OperatorPrecedence::Multiplicative,
        BinaryOperator::Greater
        | BinaryOperator::GreaterEqual
        | BinaryOperator::Less
        | BinaryOperator::LessEqual => OperatorPrecedence::Comparative,
        BinaryOperator::BangEqual | BinaryOperator::EqualEqual => OperatorPrecedence::Equality,
    }
}

type Tokens = std::iter::Peekable<std::vec::IntoIter<Token>>;

// Operator precedence parsing
fn parse_expression(tokens: &mut Tokens) -> Expression {
    let lhs = parse_primary(tokens);
    parse_expression_1(tokens, lhs, OperatorPrecedence::Equality)
}

fn parse_expression_1(
    tokens: &mut Tokens,
    lhs: Expression,
    min_precedence: OperatorPrecedence,
) -> Expression {
    let mut lhs = lhs;

    while let Some(token) = tokens.peek() {
        let operator = get_operator(token);
        if operator.is_none() {
            break;
        }
        let operator = operator.unwrap();
        let precedence = get_precedence(&operator);
        if precedence < min_precedence {
            break;
        }
        tokens.next();

        let mut rhs = parse_primary(tokens);

        while let Some(token) = tokens.peek() {
            let next_operator = get_operator(&token);
            if next_operator.is_none() {
                break;
            }
            let next_operator = next_operator.unwrap();
            let next_precedence = get_precedence(&next_operator);
            if next_precedence > precedence {
                rhs = parse_expression_1(tokens, rhs, next_precedence);
            } else {
                break;
            }
        }

        lhs = Expression::Binary {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        };
    }

    lhs
}

fn parse_primary(tokens: &mut Tokens) -> Expression {
    let token = tokens.next().unwrap();
    match token.kind {
        TokenKind::Float(f) => Expression::Float(f),
        TokenKind::Integer(i) => Expression::Integer(i),
        TokenKind::String(s) => Expression::String(s),
        TokenKind::Character(c) => Expression::Character(c),
        TokenKind::True => Expression::Boolean(true),
        TokenKind::False => Expression::Boolean(false),
        TokenKind::Identifier(name) => match tokens.peek() {
            Some(&Token {
                kind: TokenKind::ParenLeft,
                ..
            }) => {
                tokens.next();
                let arguments = parse_argument_list(tokens);
                Expression::FunctionCall { name, arguments }
            }
            Some(&Token {
                kind: TokenKind::Equal,
                ..
            }) => {
                tokens.next();
                let value = parse_expression(tokens);
                Expression::Assignment {
                    name,
                    value: Box::new(value),
                }
            }
            _ => Expression::Variable(name),
        },
        TokenKind::ParenLeft => {
            let expr = parse_expression(tokens);
            if let Some(&Token {
                kind: TokenKind::ParenRight,
                ..
            }) = tokens.peek()
            {
                tokens.next();
            } else {
                panic!("Unexpected token {:?} Expected )", tokens.next().unwrap());
            }
            Expression::Grouping(Box::new(expr))
        }
        TokenKind::Var => parse_variable_declaration(tokens),
        TokenKind::Bang | TokenKind::Minus => {
            let operator = get_operator(&token).unwrap();
            let right = Box::new(parse_primary(tokens));
            Expression::Unary { operator, right }
        }
        TokenKind::If => parse_if(tokens),
        TokenKind::While => parse_while(tokens),
        TokenKind::For => parse_for(tokens),
        TokenKind::BraceLeft => {
            let mut expressions = Vec::<Expression>::new();
            while let Some(token) = tokens.peek() {
                match token.kind {
                    TokenKind::BraceRight => {
                        tokens.next();
                        break;
                    }
                    _ => {
                        expressions.push(parse_expression(tokens));
                        match tokens.peek() {
                            Some(&Token {
                                kind: TokenKind::SemiColon,
                                ..
                            }) => {
                                tokens.next();
                            }
                            _ => {}
                        }
                    }
                }
            }
            Expression::Block { expressions }
        }
        TokenKind::Return => {
            let value = parse_optional_expression(tokens);
            Expression::Return {
                value: Box::new(value),
            }
        }
        TokenKind::Break => {
            let value = parse_optional_expression(tokens);
            Expression::Break {
                value: Box::new(value),
            }
        }
        TokenKind::Continue => {
            let value = parse_optional_expression(tokens);
            Expression::Continue {
                value: Box::new(value),
            }
        }
        _ => panic!("Unexpected token: {:?}", token),
    }
}

fn parse_argument_list(tokens: &mut Tokens) -> Vec<Expression> {
    let mut arguments = Vec::<Expression>::new();

    while let Some(token) = tokens.peek() {
        match token.kind {
            TokenKind::ParenRight => {
                tokens.next();
                break;
            }
            TokenKind::Comma => {
                tokens.next();
            }
            _ => {
                arguments.push(parse_expression(tokens));
            }
        }
    }

    arguments
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

fn parse_variable_declaration(tokens: &mut Tokens) -> Expression {
    let name = match tokens.next().unwrap().kind {
        TokenKind::Identifier(s) => s,
        _ => panic!("Expected identifier"),
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
    Expression::VariableDeclaration {
        name,
        initializer: Box::new(initializer),
    }
}

fn parse_if(tokens: &mut Tokens) -> Expression {
    let condition = Box::new(parse_expression(tokens));
    let then_branch = Box::new(parse_expression(tokens));
    let mut else_branch = Box::new(None);

    let mut elseif_branches = Vec::new();
    while let Some(&Token {
        kind: TokenKind::Else,
        ..
    }) = tokens.peek()
    {
        tokens.next();
        match tokens.peek() {
            Some(&Token {
                kind: TokenKind::If,
                ..
            }) => {
                tokens.next();
                let condition = parse_expression(tokens);
                let body = parse_expression(tokens);
                elseif_branches.push((condition, body));
            }
            Some(_) => {
                else_branch = Box::new(Some(parse_expression(tokens)));
            },
            _ => {panic!("Expected if or expression")}
        }
    }

    Expression::If {
        condition,
        then_branch,
        elseif_branches,
        else_branch,
    }
}

fn parse_while(tokens: &mut Tokens) -> Expression {
    let condition = Box::new(parse_expression(tokens));
    let body = Box::new(parse_expression(tokens));
    Expression::While { condition, body }
}

fn parse_for(tokens: &mut Tokens) -> Expression {
    let initializer = Box::new(parse_expression(tokens));
    match tokens.peek() {
        Some(&Token {
            kind: TokenKind::SemiColon,
            ..
        }) => {
            tokens.next();
        }
        _ => panic!("Expected ;"),
    }
    let condition = Box::new(parse_expression(tokens));
    match tokens.peek() {
        Some(&Token {
            kind: TokenKind::SemiColon,
            ..
        }) => {
            tokens.next();
        }
        _ => panic!("Expected ;"),
    }
    let increment = Box::new(parse_expression(tokens));
    match tokens.peek() {
        Some(&Token {
            kind: TokenKind::SemiColon,
            ..
        }) => {
            tokens.next();
        }
        _ => panic!("Expected ;"),
    }
    let body = Box::new(parse_expression(tokens));
    Expression::For {
        initializer,
        condition,
        increment,
        body,
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
                panic!("Unexpected token: {:?}, expected ;", tokens.next().unwrap());
            }
            None => {
                break;
            }
        }
    }

    Ast { expressions }
}
