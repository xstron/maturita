use std::fmt::Display;
use std::ops;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};

use crate::parser::{Ast, Expression, ExpressionKind, Operator};

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Character(char),
    Boolean(bool),
    List(Vec<Value>),
    Function(Vec<String>, Expression),
    NativeFunction(fn(Vec<Value>) -> Value),
    Return(Box<Value>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Integer(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Character(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::List(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Function(_, _) => write!(f, "<function>"),
            Value::NativeFunction(_) => write!(f, "<native function>"),
            Value::Return(value) => write!(f, "{}", value),
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = Value;
    fn add(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left + right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 + right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left + right as f64),
            (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
            (Value::String(left), Value::String(right)) => Value::String(left + &right),
            (Value::String(left), Value::Character(right)) => {
                Value::String(left + &right.to_string())
            }
            (Value::Character(left), Value::String(right)) => {
                Value::String(left.to_string() + &right)
            }
            (Value::Character(left), Value::Character(right)) => {
                Value::String(left.to_string() + &right.to_string())
            }
            (Value::List(left), Value::List(right)) => {
                let mut list = left.clone();
                list.extend(right);
                Value::List(list)
            }
            (Value::List(left), right) => {
                let mut list = left.clone();
                list.push(right);
                Value::List(list)
            }
            _ => panic!("Invalid operands for operator +"),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;
    fn sub(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left - right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
            _ => panic!("Invalid operands for operator -"),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;
    fn mul(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left * right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left * right as f64),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 * right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
            (Value::String(left), Value::Integer(right)) => {
                Value::String(left.repeat(right as usize))
            }
            (Value::Integer(left), Value::String(right)) => {
                Value::String(right.repeat(left as usize))
            }
            _ => panic!("Invalid operands for operator *"),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;
    fn div(self, rhs: Value) -> Value {
        match (self, rhs) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left / right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
            _ => panic!("Invalid operands for operator /"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Integer(left), Value::Integer(right)) => left == right,
            (Value::Float(left), Value::Float(right)) => left == right,
            (Value::String(left), Value::String(right)) => left == right,
            (Value::Character(left), Value::Character(right)) => left == right,
            (Value::Boolean(left), Value::Boolean(right)) => left == right,
            (Value::List(left), Value::List(right)) => left == right,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => left.partial_cmp(right),
            (Value::Float(left), Value::Integer(right)) => left.partial_cmp(&(*right as f64)),
            (Value::Integer(left), Value::Float(right)) => (*left as f64).partial_cmp(right),
            (Value::Float(left), Value::Float(right)) => left.partial_cmp(right),
            (Value::String(left), Value::String(right)) => left.partial_cmp(right),
            (Value::Character(left), Value::Character(right)) => left.partial_cmp(right),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Context {
    variables: Rc<RefCell<HashMap<String, Value>>>,
    parent: Option<Rc<Context>>,
}

impl Context {
    fn new(parent: Option<Rc<Context>>) -> Self {
        Context {
            variables: Rc::new(RefCell::new(HashMap::new())),
            parent,
        }
    }

    fn get_variable(&self, name: &str) -> Option<Value> {
        match self.variables.borrow().get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.get_variable(name),
                None => None,
            },
        }
    }

    fn set_variable(&self, name: &str, value: Value) {
        if self.variables.borrow().contains_key(name) {
            self.variables.borrow_mut().insert(name.to_owned(), value);
        } else {
            match &self.parent {
                Some(parent) => parent.set_variable(name, value),
                None => panic!("Variable {} not found", name),
            }
        }
    }

    fn define_variable(&self, name: &str, value: Value) {
        self.variables.borrow_mut().insert(name.to_owned(), value);
    }
}

fn interpret_unary(context: Rc<Context>, operator: Operator, right: &Expression) -> Value {
    let value = interpret_expression(context.clone(), right);
    match operator {
        Operator::MinusUnary => match value {
            Value::Integer(value) => Value::Integer(-value),
            Value::Float(value) => Value::Float(-value),
            _ => panic!("non numeric value used with numeric operator"),
        },
        Operator::Bang => match value {
            Value::Boolean(value) => Value::Boolean(!value),
            _ => panic!("non boolean value used with boolean operator"),
        },
        _ => panic!("Invalid unary operator"),
    }
}

fn interpret_binary(
    context: Rc<Context>,
    lhs: &Expression,
    operator: Operator,
    rhs: &Expression,
) -> Value {
    let left = interpret_expression(context.clone(), lhs);
    let right = interpret_expression(context.clone(), rhs);
    match operator {
        Operator::BangEqual => Value::Boolean(left != right),
        Operator::EqualEqual => Value::Boolean(left == right),
        Operator::Less => Value::Boolean(left < right),
        Operator::LessEqual => Value::Boolean(left <= right),
        Operator::Greater => Value::Boolean(left > right),
        Operator::GreaterEqual => Value::Boolean(left >= right),
        Operator::Plus => left + right,
        Operator::Minus => left - right,
        Operator::Star => left * right,
        Operator::Slash => left / right,
        _ => panic!(
            "Unexpected bynary operator {:?} on {}:{}",
            operator, lhs.position.line, lhs.position.column
        ),
    }
}

fn interpret_variable_definition(
    context: Rc<Context>,
    name: &str,
    initializer: Option<Expression>,
) -> Value {
    match context.get_variable(name) {
        Some(_) => panic!("Variable {} already declared", name),
        None => {
            let value = match initializer {
                Some(initializer) => interpret_expression(context.clone(), &initializer),
                None => Value::Null,
            };

            context.define_variable(name, value.clone());
            value
        }
    }
}

fn interpret_assignment(context: Rc<Context>, name: &str, value: &Expression) -> Value {
    match context.get_variable(name) {
        Some(_) => {
            let value = interpret_expression(context.clone(), &value);
            context.set_variable(name, value.clone());
            value
        }
        None => panic!("Variable {} not found", name),
    }
}

fn interpret_function_definition(
    context: Rc<Context>,
    name: &str,
    parameters: Vec<String>,
    body: Expression,
) -> Value {
    let function = Value::Function(parameters, body);

    context.define_variable(name, function.clone());
    function
}

fn interpret_function_call(
    context: Rc<Context>,
    callee: &Expression,
    arguments: Vec<Expression>,
) -> Value {
    let callee = interpret_expression(context.clone(), callee);
    let arguments: Vec<Value> = arguments
        .iter()
        .map(|arg| interpret_expression(context.clone(), arg))
        .collect();

    match callee {
        Value::Function(params, body) => {
            let function_context = Rc::new(Context::new(Some(context.clone())));

            for (param, arg) in params.iter().zip(arguments) {
                function_context.define_variable(&param.to_string(), arg);
            }

            match interpret_expression(function_context, &body) {
                Value::Return(value) => *value,
                value => value,
            }
         
        }
        Value::NativeFunction(function) => function(arguments),
        _ => panic!("Invalid function call"),
    }
}

fn interpret_block(context: Rc<Context>, expressions: Vec<Expression>) -> Value {
    let block_context = Rc::new(Context::new(Some(context.clone())));

    let mut value = Value::Null;

    for expression in expressions {
        value = interpret_expression(block_context.clone(), &expression);
        match value {
            Value::Return(_) => return value,
            _ => (),
        }
    }

    value
}

fn interpret_if(
    context: Rc<Context>,
    condition: &Expression,
    then_branch: &Expression,
    elseif_branches: Vec<(Expression, Expression)>,
    else_branch: Option<Expression>,
) -> Value {
    if let Value::Boolean(true) = interpret_expression(context.clone(), condition) {
        return interpret_expression(context.clone(), then_branch);
    } else {
        for (condition, body) in elseif_branches {
            if let Value::Boolean(true) = interpret_expression(context.clone(), &condition) {
                return interpret_expression(context.clone(), &body);
            }
        }

        if let Some(else_branch) = else_branch {
            return interpret_expression(context.clone(), &else_branch);
        }
    }

    Value::Null
}

fn interpret_while(context: Rc<Context>, condition: &Expression, body: &Expression) -> Value {
    let mut list = vec![];
    while let Value::Boolean(true) = interpret_expression(context.clone(), condition) {
        list.push(interpret_expression(context.clone(), body));
    }

    Value::List(list)
}

fn interpret_for(
    context: Rc<Context>,
    initializer: &Expression,
    condition: &Expression,
    increment: &Expression,
    body: &Expression,
) -> Value {
    let mut list = vec![];
    interpret_expression(context.clone(), initializer);
    while let Value::Boolean(true) = interpret_expression(context.clone(), condition) {
        list.push(interpret_expression(context.clone(), body));
        interpret_expression(context.clone(), increment);
    }

    Value::List(list)
}

fn interpret_return(context: Rc<Context>, value: Option<Expression>) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value),
        None => Value::Null,
    };

    Value::Return(Box::new(value))
}

fn interpret_break(context: Rc<Context>, value: Option<Expression>) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value),
        None => Value::Null,
    };

    value
}

fn interpret_continue(context: Rc<Context>, value: Option<Expression>) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value),
        None => Value::Null,
    };

    value
}

fn interpret_list_index(context: Rc<Context>, list: &Expression, index: &Expression) -> Value {
    let list = interpret_expression(context.clone(), list);
    let index = interpret_expression(context.clone(), index);

    match (list, index) {
        (Value::List(list), Value::Integer(index)) => list[index as usize].clone(),
        _ => panic!("Invalid list index"),
    }
}

fn interpret_import(context: Rc<Context>, path: &str) -> Value {
    let contents = std::fs::read_to_string(path).expect("Unable to read file");
    let tokens = crate::tokenizer::tokenize(&contents, path);
    let ast = crate::parser::parse(tokens);

    interpret_ast(context, ast)
}

fn interpret_expression(context: Rc<Context>, expression: &Expression) -> Value {
    match &expression.kind {
        ExpressionKind::Float(f) => Value::Float(*f),
        ExpressionKind::Integer(i) => Value::Integer(*i),
        ExpressionKind::String(s) => Value::String(s.to_owned()),
        ExpressionKind::Character(c) => Value::Character(*c),
        ExpressionKind::Boolean(b) => Value::Boolean(*b),
        ExpressionKind::Variable(name) => match context.get_variable(name) {
            Some(value) => value,
            None => panic!("Variable {} not found", name),
        },
        ExpressionKind::List(expressions) => Value::List(
            expressions
                .iter()
                .map(|e| interpret_expression(context.clone(), e))
                .collect(),
        ),
        ExpressionKind::Unary { operator, right } => interpret_unary(context, *operator, &*right),
        ExpressionKind::Binary {
            left,
            operator,
            right,
        } => interpret_binary(context, &*left, *operator, &*right),
        ExpressionKind::Grouping(expression) => interpret_expression(context, expression),
        ExpressionKind::VariableDefinition { name, initializer } => {
            interpret_variable_definition(context, name, *initializer.clone())
        }
        ExpressionKind::Assignment { name, value } => interpret_assignment(context, name, &*value),
        ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
        } => interpret_function_definition(context, name, parameters.to_vec(), *body.to_owned()),
        ExpressionKind::FunctionCall { callee, arguments } => {
            interpret_function_call(context, callee, arguments.to_vec())
        }
        ExpressionKind::Block { expressions } => interpret_block(context, expressions.to_vec()),
        ExpressionKind::If {
            condition,
            then_branch,
            elseif_branches,
            else_branch,
        } => interpret_if(
            context,
            condition,
            &then_branch.to_owned(),
            elseif_branches.to_vec(),
            *else_branch.to_owned(),
        ),
        ExpressionKind::While { condition, body } => interpret_while(context, condition, body),
        ExpressionKind::For {
            initializer,
            condition,
            increment,
            body,
        } => interpret_for(context, initializer, condition, increment, body),
        ExpressionKind::Return { value } => interpret_return(context, *value.clone()),
        ExpressionKind::Break { value } => interpret_break(context.clone(), *value.clone()),
        ExpressionKind::Continue { value } => interpret_continue(context.clone(), *value.clone()),
        ExpressionKind::ListIndex { list, index } => interpret_list_index(context, list, index),
        ExpressionKind::Import(filepath) => interpret_import(context.clone(), filepath),
    }
}

fn interpret_ast(context: Rc<Context>, ast: Ast) -> Value {
    let mut value = Value::Null;
    for expression in ast.expressions {
        value = interpret_expression(context.clone(), &expression);
    }

    value
}

fn add_native_functions(context: Rc<Context>) {
    context.define_variable(
        "print",
        Value::NativeFunction(|arguments| {
            for argument in arguments {
                print!("{}", argument);
            }
            println!();
            Value::Null
        }),
    );
    context.define_variable(
        "append",
        Value::NativeFunction(|arguments| {
            let mut list = match &arguments[0] {
                Value::List(list) => list.clone(),
                _ => panic!("First argument must be a list"),
            };
            list.push(arguments[1].clone());
            Value::List(list)
        }),
    );
    context.define_variable(
        "pop",
        Value::NativeFunction(|arguments| {
            let mut list = match &arguments[0] {
                Value::List(list) => list.clone(),
                _ => panic!("First argument must be a list"),
            };
            match &arguments[1] {
                Value::Integer(index) => list.remove(*index as usize),
                _ => panic!("Second argument must be an integer"),
            }
        }),
    );
    context.define_variable(
        "insert",
        Value::NativeFunction(|arguments| {
            let mut list = match &arguments[0] {
                Value::List(list) => list.clone(),
                _ => panic!("First argument must be a list"),
            };
            match &arguments[1] {
                Value::Integer(index) => {
                    list.insert(*index as usize, arguments[2].clone());
                    Value::List(list)
                }
                _ => panic!("Second argument must be an integer"),
            }
        }),
    );
    context.define_variable(
        "len",
        Value::NativeFunction(|arguments| match &arguments[0] {
            Value::List(list) => Value::Integer(list.len() as i64),
            _ => panic!("Argument must be a list"),
        }),
    );
    context.define_variable(
        "input",
        Value::NativeFunction(|_| {
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::String(input.trim().to_owned())
        }),
    );
    context.define_variable(
        "int",
        Value::NativeFunction(|arguments| match &arguments[0] {
            Value::String(s) => Value::Integer(s.parse().unwrap()),
            _ => panic!("Argument must be a string"),
        }),
    );
    context.define_variable(
        "float",
        Value::NativeFunction(|arguments| match &arguments[0] {
            Value::String(s) => Value::Float(s.parse().unwrap()),
            _ => panic!("Argument must be a string"),
        }),
    );
    context.define_variable(
        "str",
        Value::NativeFunction(|arguments| Value::String(arguments[0].to_string())),
    );
    context.define_variable(
        "char",
        Value::NativeFunction(|arguments| match &arguments[0] {
            Value::Integer(i) => Value::Character((*i as u8) as char),
            _ => panic!("Argument must be an integer"),
        }),
    );
}

pub fn interpret(ast: Ast) {
    let context = Rc::new(Context::new(None));

    add_native_functions(context.clone());

    interpret_ast(context, ast);
}
