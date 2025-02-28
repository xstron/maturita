use std::fmt::Display;
use std::process::exit;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};

use crate::parser::{Ast, Expression, ExpressionKind, Operator};
use crate::tokenizer::Position;

#[derive(Debug, Clone)]
pub struct Value {
    kind: Rc<RefCell<ValueKind>>,
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Null,
    Integer(i64),
    Float(f64),
    String(String),
    Character(char),
    Boolean(bool),
    List(Vec<Value>),
    Function(Vec<String>, Expression),
    NativeFunction(fn(Rc<Context>, Vec<Value>, &Position) -> Value),
    Return(Box<Value>),
    Break(Box<Value>),
    Continue(Box<Value>),
}

impl Value {
    fn new(kind: ValueKind) -> Self {
        Value {
            kind: Rc::new(RefCell::new(kind)),
        }
    }

    fn get(&self) -> ValueKind {
        self.kind.borrow().to_owned()
    }

    fn unwrap_flow_control(self) -> Value {
        match self.get() {
            ValueKind::Return(value) => value.unwrap_flow_control(),
            ValueKind::Break(value) => value.unwrap_flow_control(),
            ValueKind::Continue(value) => value.unwrap_flow_control(),
            _ => self
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Null => write!(f, "null"),
            ValueKind::Integer(value) => write!(f, "{}", value),
            ValueKind::Float(value) => write!(f, "{}", value),
            ValueKind::String(value) => write!(f, "{}", value),
            ValueKind::Character(value) => write!(f, "{}", value),
            ValueKind::Boolean(value) => write!(f, "{}", value),
            ValueKind::List(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value.get())?;
                }
                write!(f, "]")
            }
            ValueKind::Function(_, _) => write!(f, "<function>"),
            ValueKind::NativeFunction(_) => write!(f, "<native function>"),
            ValueKind::Return(value) => write!(f, "{}", value.get()),
            ValueKind::Break(value) => write!(f, "{}", value.get()),
            ValueKind::Continue(value) => write!(f, "{}", value.get()),
        }
    }
}

fn add(lhs: Value, rhs: Value, position: &Position) -> Value {
    match (lhs.get(), rhs.get()) {
        (ValueKind::Integer(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Integer(left + right))
        }
        (ValueKind::Integer(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left as f64 + right))
        }
        (ValueKind::Float(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Float(left + right as f64))
        }
        (ValueKind::Float(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left + right))
        }
        (ValueKind::String(left), ValueKind::String(right)) => {
            Value::new(ValueKind::String(left + &right))
        }
        (ValueKind::String(left), ValueKind::Character(right)) => {
            Value::new(ValueKind::String(left + &right.to_string()))
        }
        (ValueKind::Character(left), ValueKind::String(right)) => {
            Value::new(ValueKind::String(left.to_string() + &right))
        }
        (ValueKind::Character(left), ValueKind::Character(right)) => {
            Value::new(ValueKind::String(left.to_string() + &right.to_string()))
        }
        (ValueKind::List(mut left), ValueKind::List(right)) => {
            left.extend(right);
            Value::new(ValueKind::List(left))
        }
        (ValueKind::List(mut left), right) => {
            left.push(Value::new(right));
            Value::new(ValueKind::List(left))
        }
        (left, right) => {
            eprintln!(
                "Invalid operands {} and {} for operator + at {}",
                left, right, position
            );
            exit(1);
        }
    }
}

fn sub(lhs: Value, rhs: Value, position: &Position) -> Value {
    match (lhs.get(), rhs.get()) {
        (ValueKind::Integer(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Integer(left - right))
        }
        (ValueKind::Float(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left - right))
        }
        (left, right) => {
            eprintln!(
                "Invalid operands {} and {} for operator - at {}",
                left, right, position
            );
            exit(1);
        }
    }
}

fn mul(lhs: Value, rhs: Value, position: &Position) -> Value {
    match (lhs.get(), rhs.get()) {
        (ValueKind::Integer(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Integer(left * right))
        }
        (ValueKind::Float(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Float(left * right as f64))
        }
        (ValueKind::Integer(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left as f64 * right))
        }
        (ValueKind::Float(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left * right))
        }
        (ValueKind::String(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::String(left.repeat(right as usize)))
        }
        (ValueKind::Integer(left), ValueKind::String(right)) => {
            Value::new(ValueKind::String(right.repeat(left as usize)))
        }
        (left, right) => {
            eprintln!(
                "Invalid operands {} and {} for operator * at {}",
                left, right, position
            );
            exit(1);
        }
    }
}

fn div(lhs: Value, rhs: Value, position: &Position) -> Value {
    match (lhs.get(), rhs.get()) {
        (ValueKind::Integer(left), ValueKind::Integer(right)) => {
            Value::new(ValueKind::Integer(left / right))
        }
        (ValueKind::Float(left), ValueKind::Float(right)) => {
            Value::new(ValueKind::Float(left / right))
        }
        (left, right) => {
            eprintln!(
                "Invalid operands {} and {} for operator / at {}",
                left, right, position
            );
            exit(1);
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (
            self.get(),
            other.get(),
        ) {
            (ValueKind::Null, ValueKind::Null) => true,
            (ValueKind::Integer(left), ValueKind::Integer(right)) => left == right,
            (ValueKind::Float(left), ValueKind::Float(right)) => left == right,
            (ValueKind::String(left), ValueKind::String(right)) => left == right,
            (ValueKind::Character(left), ValueKind::Character(right)) => left == right,
            (ValueKind::Boolean(left), ValueKind::Boolean(right)) => left == right,
            (ValueKind::List(left), ValueKind::List(right)) => left == right,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (
            self.get(),
            other.get(),
        ) {
            (ValueKind::Integer(left), ValueKind::Integer(right)) => left.partial_cmp(&right),
            (ValueKind::Float(left), ValueKind::Integer(right)) => {
                left.partial_cmp(&(right as f64))
            }
            (ValueKind::Integer(left), ValueKind::Float(right)) => {
                (left as f64).partial_cmp(&right)
            }
            (ValueKind::Float(left), ValueKind::Float(right)) => left.partial_cmp(&right),
            (ValueKind::String(left), ValueKind::String(right)) => left.partial_cmp(&right),
            (ValueKind::Character(left), ValueKind::Character(right)) => left.partial_cmp(&right),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Context {
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
            self.variables
                .borrow_mut()
                .insert(name.to_owned(), value);
        } else {
            match &self.parent {
                Some(parent) => parent.set_variable(name, value),
                None => panic!("Variable \"{}\" not found", name),
            }
        }
    }

    fn define_variable(&self, name: &str, value: Value) {
        self.variables
            .borrow_mut()
            .insert(name.to_owned(), value);
    }
}

fn panic_unexpected_value(value: Value, position: &Position, expected: &str) -> ! {
    eprintln!(
        "Unexpected value: {:?} at {}, expected {}",
        value, position, expected
    );
    exit(1);
}

fn panic_unexpected_operator(operator: Operator, position: &Position, expected: &str) -> ! {
    eprintln!(
        "Unexpected value: {:?} at {}, expected {}",
        operator, position, expected
    );
    exit(1);
}

fn interpret_unary(
    context: Rc<Context>,
    operator: Operator,
    right: &Expression,
    position: &Position,
) -> Value {
    let value = interpret_expression(context.clone(), right, position);
    match operator {
        Operator::MinusUnary => match value.get() {
            ValueKind::Integer(value) => Value::new(ValueKind::Integer(-value)),
            ValueKind::Float(value) => Value::new(ValueKind::Float(-value)),
            _ => panic_unexpected_value(value.clone(), &right.position, "numeric type"),
        },
        Operator::Bang => match value.get() {
            ValueKind::Boolean(value) => Value::new(ValueKind::Boolean(!value)),
            _ => panic_unexpected_value(value.clone(), &right.position, "boolean"),
        },
        _ => panic_unexpected_operator(operator, position, "unary operator"),
    }
}

fn interpret_binary(
    context: Rc<Context>,
    lhs: &Expression,
    operator: Operator,
    rhs: &Expression,
    position: &Position,
) -> Value {
    let left = interpret_expression(context.clone(), lhs, position);
    let right = interpret_expression(context.clone(), rhs, position);
    match operator {
        Operator::BangEqual => Value::new(ValueKind::Boolean(left != right)),
        Operator::EqualEqual => Value::new(ValueKind::Boolean(left == right)),
        Operator::Less => Value::new(ValueKind::Boolean(left < right)),
        Operator::LessEqual => Value::new(ValueKind::Boolean(left <= right)),
        Operator::Greater => Value::new(ValueKind::Boolean(left > right)),
        Operator::GreaterEqual => Value::new(ValueKind::Boolean(left >= right)),
        Operator::Plus => add(left, right, position),
        Operator::Minus => sub(left, right, position),
        Operator::Star => mul(left, right, position),
        Operator::Slash => div(left, right, position),
        _ => panic_unexpected_operator(operator, position, "binary operator"),
    }
}

fn interpret_variable_definition(
    context: Rc<Context>,
    name: &str,
    initializer: Option<Expression>,
    position: &Position,
) -> Value {
    match context.get_variable(name) {
        Some(_) => {
            eprintln!(
                "Already existing variable \"{}\" being defined again at {}",
                name, position
            );
            exit(1);
        }
        None => {
            let value = match initializer {
                Some(initializer) => interpret_expression(context.clone(), &initializer, position),
                None => Value::new(ValueKind::Null),
            };

            context.define_variable(name, value.clone());
            value
        }
    }
}

fn interpret_assignment(
    context: Rc<Context>,
    name: &str,
    value: &Expression,
    position: &Position,
) -> Value {
    match context.get_variable(name) {
        Some(_) => {
            let value = interpret_expression(context.clone(), &value, position);
            context.set_variable(name, value.clone());
            value
        }
        None => {
            eprintln!(
                "Non-existent variable \"{}\" being assigned a value at {}",
                name, position
            );
            exit(1);
        }
    }
}

fn interpret_function_definition(
    context: Rc<Context>,
    name: &str,
    parameters: Vec<String>,
    body: Expression,
    position: &Position,
) -> Value {
    match context.get_variable(name) {
        Some(_) => {
            eprintln!(
                "Already existing function \"{}\" being defined again at {}",
                name, position
            );
            exit(1);
        }
        None => {
            let function = Value::new(ValueKind::Function(parameters, body));

            context.define_variable(name, function.clone());
            function
        }
    }
}

fn interpret_function_call(
    context: Rc<Context>,
    callee: &Expression,
    arguments: Vec<Expression>,
    position: &Position,
) -> Value {
    let callee = interpret_expression(context.clone(), callee, position);
    let arguments: Vec<Value> = arguments
        .iter()
        .map(|arg| interpret_expression(context.clone(), arg, position))
        .collect();

    match callee.clone().get() {
        ValueKind::Function(params, body) => {
            let function_context = Rc::new(Context::new(Some(context.clone())));

            if arguments.len() != params.len() {
                eprintln!(
                    "Expected {} arguments, got {} at {}",
                    params.len(),
                    arguments.len(),
                    position
                );
                exit(1);
            }

            for (param, arg) in params.iter().zip(arguments) {
                function_context.define_variable(&param.to_string(), arg);
            }

            interpret_expression(function_context, &body, position).unwrap_flow_control()
        }
        ValueKind::NativeFunction(function) => function(context.clone(), arguments, position),
        _ => panic_unexpected_value(callee.clone(), position, "function or native function"),
    }
}

fn interpret_block(
    context: Rc<Context>,
    expressions: Vec<Expression>,
    position: &Position,
) -> Value {
    let block_context = Rc::new(Context::new(Some(context.clone())));

    let mut value = Value::new(ValueKind::Null);

    for expression in expressions {
        value = interpret_expression(block_context.clone(), &expression, position);
        match value.get() {
            ValueKind::Return(_) | ValueKind::Break(_) | ValueKind::Continue(_) => break,
            _ => {}
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
    position: &Position,
) -> Value {
    if let ValueKind::Boolean(true) = interpret_expression(context.clone(), condition, position)
        .kind
        .borrow()
        .to_owned()
    {
        return interpret_expression(context.clone(), then_branch, position);
    } else {
        for (condition, body) in elseif_branches {
            if let ValueKind::Boolean(true) =
                interpret_expression(context.clone(), &condition, position)
                    .kind
                    .borrow()
                    .to_owned()
            {
                return interpret_expression(context.clone(), &body, position);
            }
        }

        if let Some(else_branch) = else_branch {
            return interpret_expression(context.clone(), &else_branch, position);
        }
    }

    Value::new(ValueKind::Null)
}

fn interpret_while(
    context: Rc<Context>,
    condition: &Expression,
    body: &Expression,
    position: &Position,
) -> Value {
    let mut list = vec![];
    while let ValueKind::Boolean(true) = interpret_expression(context.clone(), condition, position)
        .kind
        .borrow()
        .to_owned()
    {
        let value = interpret_expression(context.clone(), body, position);
        list.push(value.clone().unwrap_flow_control());
        match value.get() {
            ValueKind::Return(_) => {
                return value.clone();
            }
            ValueKind::Break(_) => {
                break;
            }
            ValueKind::Continue(_) => {
                continue;
            }
            _ => {}
        };
    }

    Value::new(ValueKind::List(list))
}

fn interpret_for(
    context: Rc<Context>,
    initializer: &Expression,
    condition: &Expression,
    increment: &Expression,
    body: &Expression,
    position: &Position,
) -> Value {
    let mut list = vec![];
    interpret_expression(context.clone(), initializer, position);
    while let ValueKind::Boolean(true) = interpret_expression(context.clone(), condition, position)
        .kind
        .borrow()
        .to_owned()
    {
        let value = interpret_expression(context.clone(), body, position);
        list.push(value.clone().unwrap_flow_control());
        match value.get() {
            ValueKind::Return(_) => {
                return value.clone();
            }
            ValueKind::Break(_) => {
                break;
            }
            _ => {}
        }
        interpret_expression(context.clone(), increment, position);
    }

    Value::new(ValueKind::List(list))
}

fn interpret_return(context: Rc<Context>, value: Option<Expression>, position: &Position) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value, position),
        None => Value::new(ValueKind::Null),
    };

    Value::new(ValueKind::Return(Box::new(value)))
}

fn interpret_break(context: Rc<Context>, value: Option<Expression>, position: &Position) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value, position),
        None => Value::new(ValueKind::Null),
    };

    Value::new(ValueKind::Break(Box::new(value)))
}

fn interpret_continue(
    context: Rc<Context>,
    value: Option<Expression>,
    position: &Position,
) -> Value {
    let value = match value {
        Some(value) => interpret_expression(context, &value, position),
        None => Value::new(ValueKind::Null),
    };

    Value::new(ValueKind::Continue(Box::new(value)))
}

fn interpret_list_index(
    context: Rc<Context>,
    list: &Expression,
    index: &Expression,
    position: &Position,
) -> Value {
    let list_value = interpret_expression(context.clone(), list, position);
    let index_value = interpret_expression(context.clone(), index, position);

    match list_value.get() {
        ValueKind::List(_) => {}
        _ => panic_unexpected_value(list_value.clone(), &list.position, "list"),
    }

    match index_value.get() {
        ValueKind::Integer(_) => {}
        _ => panic_unexpected_value(index_value.clone(), &index.position, "integer list index"),
    }

    match (
        list_value.clone().get(),
        index_value.clone().get(),
    ) {
        (ValueKind::List(list), ValueKind::Integer(index)) => list[index as usize].clone(),
        _ => {
            eprintln!("Invalid list index at {}", position);
            exit(1);
        }
    }
}

fn interpret_import(context: Rc<Context>, path: &str, position: &Position) -> Value {
    let contents = match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Failed to import file \"{}\" at {}: {}", path, position, e);
            exit(1);
        }
    };
    let tokens = crate::tokenizer::tokenize(&contents, path);
    let ast = crate::parser::parse(tokens);

    interpret_ast(context, ast)
}

fn interpret_expression(
    context: Rc<Context>,
    expression: &Expression,
    position: &Position,
) -> Value {
    match &expression.kind {
        ExpressionKind::Float(f) => Value::new(ValueKind::Float(*f)),
        ExpressionKind::Integer(i) => Value::new(ValueKind::Integer(*i)),
        ExpressionKind::String(s) => Value::new(ValueKind::String(s.to_owned())),
        ExpressionKind::Character(c) => Value::new(ValueKind::Character(*c)),
        ExpressionKind::Boolean(b) => Value::new(ValueKind::Boolean(*b)),
        ExpressionKind::Null => Value::new(ValueKind::Null),
        ExpressionKind::Variable(name) => match context.get_variable(name) {
            Some(value) => value,
            None => {
                eprintln!(
                    "Non-existent variable \"{}\" being evaluated at {}",
                    name, position
                );
                exit(1)
            }
        },
        ExpressionKind::List(expressions) => Value::new(ValueKind::List(
            expressions
                .iter()
                .map(|e| interpret_expression(context.clone(), e, position))
                .collect(),
        )),
        ExpressionKind::Unary { operator, right } => {
            interpret_unary(context, *operator, &*right, &expression.position)
        }
        ExpressionKind::Binary {
            left,
            operator,
            right,
        } => interpret_binary(context, &*left, *operator, &*right, &expression.position),
        ExpressionKind::Grouping(expression) => interpret_expression(context, expression, position),
        ExpressionKind::VariableDefinition { name, initializer } => {
            interpret_variable_definition(context, name, *initializer.clone(), &expression.position)
        }
        ExpressionKind::Assignment { name, value } => {
            interpret_assignment(context, name, &*value, &expression.position)
        }
        ExpressionKind::FunctionDefinition {
            name,
            parameters,
            body,
        } => interpret_function_definition(
            context,
            name,
            parameters.to_vec(),
            *body.to_owned(),
            &expression.position,
        ),
        ExpressionKind::FunctionCall { callee, arguments } => {
            interpret_function_call(context, callee, arguments.to_vec(), &expression.position)
        }
        ExpressionKind::Block { expressions } => {
            interpret_block(context, expressions.to_vec(), &expression.position)
        }
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
            &expression.position,
        ),
        ExpressionKind::While { condition, body } => {
            interpret_while(context, condition, body, &expression.position)
        }
        ExpressionKind::For {
            initializer,
            condition,
            increment,
            body,
        } => interpret_for(
            context,
            initializer,
            condition,
            increment,
            body,
            &expression.position,
        ),
        ExpressionKind::Return { value } => {
            interpret_return(context, *value.clone(), &expression.position)
        }
        ExpressionKind::Break { value } => {
            interpret_break(context.clone(), *value.clone(), &expression.position)
        }
        ExpressionKind::Continue { value } => {
            interpret_continue(context.clone(), *value.clone(), &expression.position)
        }
        ExpressionKind::ListIndex { list, index } => {
            interpret_list_index(context, list, index, &expression.position)
        }
        ExpressionKind::Import(filepath) => {
            interpret_import(context.clone(), filepath, &expression.position)
        }
    }
}

fn interpret_ast(context: Rc<Context>, ast: Ast) -> Value {
    let mut value = Value::new(ValueKind::Null);
    for expression in ast.expressions {
        value = interpret_expression(context.clone(), &expression, &expression.position);
    }

    value
}

fn add_native_functions(context: Rc<Context>) {
    context.define_variable(
        "print",
        Value::new(ValueKind::NativeFunction(|_, arguments, _| {
            for argument in arguments {
                print!("{} ", argument.get());
            }
            println!();
            Value::new(ValueKind::Null)
        })),
    );
    context.define_variable(
        "push",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 2 {
                eprintln!(
                    "Expected 2 arguments, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            let mut kind = arguments[0].kind.borrow_mut();
            let mut list = match kind.to_owned() {
                ValueKind::List(list) => list,
                _ => {
                    eprintln!("First argument of \"append\" is not a list at {}", position);
                    exit(1);
                }
            };
            list.push(arguments[1].clone());
            *kind = ValueKind::List(list);
            arguments[1].clone()
        })),
    );
    context.define_variable(
        "pop",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            let mut kind = arguments[0].kind.borrow_mut();
            let mut list = match kind.to_owned() {
                ValueKind::List(list) => list,
                _ => {
                    eprintln!("First argument of \"pop\" is not a list at {}", position);
                    exit(1);
                }
            };
            let popped = list.pop();
            *kind = ValueKind::List(list);
            match popped {
                Some(val) => val,
                None => Value::new(ValueKind::Null),
            }
        })),
    );
    context.define_variable(
        "insert",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 3 {
                eprintln!(
                    "Expected 3 arguments, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            let mut kind = arguments[0].kind.borrow_mut();
            let mut list = match kind.to_owned() {
                ValueKind::List(list) => list,
                _ => {
                    eprintln!("First argument of \"insert\" is not a list at {}", position);
                    exit(1);
                }
            };
            match arguments[1].get() {
                ValueKind::Integer(index) => {
                    list.insert(index as usize, arguments[2].clone());
                    *kind = ValueKind::List(list);
                    arguments[2].clone()
                }
                _ => {
                    eprintln!(
                        "Second argument of \"insert\" is not an integer at {}",
                        position
                    );
                    exit(1);
                }
            }
        })),
    );
    context.define_variable(
        "remove",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 2 {
                eprintln!(
                    "Expected 2 arguments, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            let mut kind = arguments[0].kind.borrow_mut();
            let mut list = match kind.to_owned() {
                ValueKind::List(list) => list,
                _ => {
                    eprintln!("First argument of \"remove\" is not a list at {}", position);
                    exit(1);
                }
            };
            match arguments[1].get() {
                ValueKind::Integer(index) => {
                    let removed = list.remove(index as usize);
                    *kind = ValueKind::List(list);
                    removed
                }
                _ => {
                    eprintln!(
                        "Second argument of \"remove\" is not an integer at {}",
                        position
                    );
                    exit(1);
                }
            }
        })),
    );
    context.define_variable(
        "len",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            match arguments[0].get() {
                ValueKind::List(list) => Value::new(ValueKind::Integer(list.len() as i64)),
                _ => {
                    eprintln!("First argument of \"len\" is not a list at {}", position);
                    exit(1);
                }
            }
        })),
    );
    context.define_variable(
        "input",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 0 {
                eprintln!(
                    "Expected 0 arguments, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            Value::new(ValueKind::String(input.trim().to_owned()))
        })),
    );
    context.define_variable(
        "int",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            match arguments[0].get() {
                ValueKind::String(s) => Value::new(ValueKind::Integer(s.parse().unwrap())),
                ValueKind::Float(f) => Value::new(ValueKind::Integer(f as i64)),
                _ => {
                    eprintln!("Invalid argument type");
                    exit(1);
                }
            }
        })),
    );
    context.define_variable(
        "float",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            match arguments[0].get() {
                ValueKind::String(s) => Value::new(ValueKind::Float(s.parse().unwrap())),
                ValueKind::Integer(i) => Value::new(ValueKind::Float(i as f64)),
                _ => {
                    eprintln!("Invalid argument type");
                    exit(1);
                }
            }
        })),
    );
    context.define_variable(
        "str",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            Value::new(ValueKind::String(
                arguments[0].get().to_string(),
            ))
        })),
    );
    context.define_variable(
        "char",
        Value::new(ValueKind::NativeFunction(|_, arguments, position| {
            if arguments.len() != 1 {
                eprintln!(
                    "Expected 1 argument, got {} at {}",
                    arguments.len(),
                    position
                );
                exit(1);
            }
            match arguments[0].get() {
                ValueKind::Integer(i) => Value::new(ValueKind::Character((i as u8) as char)),
                _ => {
                    eprintln!("Invalid argument type");
                    exit(1);
                }
            }
        })),
    );
}

pub fn interpret(ast: Ast) -> Value {
    let context = Rc::new(Context::new(None));

    add_native_functions(context.clone());

    interpret_ast(context, ast)
}
