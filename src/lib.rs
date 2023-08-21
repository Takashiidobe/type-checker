use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Unary {
    Plus(Expr),
    Minus(Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Binary {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Eq(Expr, Expr),
    Ne(Expr, Expr),
    Gte(Expr, Expr),
    Gt(Expr, Expr),
    Lt(Expr, Expr),
    Lte(Expr, Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Op(Box<Op>),
    Value(Value),
    SetVar(String, Box<Value>),
    GetVar(String),
    Fn(String, Vec<Value>, Type),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Op {
    Unary(Unary),
    Binary(Binary),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Type {
    Invalid,
    Number,
    Bool,
    String,
    List,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    List(Vec<Value>),
}

#[derive(Debug, Default, Clone)]
pub struct Env {
    vars: HashMap<String, Value>,
}

impl Env {
    pub fn get(&self, var: &str) -> Option<Value> {
        self.vars.get(var).cloned()
    }

    pub fn set(&mut self, var: &str, val: Value) {
        self.vars.insert(var.to_string(), val);
    }
}

#[derive(Debug, Default, Clone)]
pub struct Program {
    body: Vec<Expr>,
    env: Env,
}

impl From<Expr> for Program {
    fn from(value: Expr) -> Self {
        Self {
            body: vec![value],
            env: Env::default(),
        }
    }
}

impl From<Vec<Expr>> for Program {
    fn from(value: Vec<Expr>) -> Self {
        Self {
            body: value,
            env: Env::default(),
        }
    }
}

impl Program {
    fn expr_to_value(&self, expr: Expr) -> Value {
        match expr {
            Expr::Op(_) => todo!(),
            Expr::Value(value) => value,
            Expr::SetVar(_, value) => *value,
            Expr::GetVar(name) => self.get_var(&name),
            Expr::Fn(_, _, _) => todo!(),
            Expr::Expr(expr) => self.expr_to_value(*expr),
        }
    }

    fn bin_op_to_type(&self, bin_op: Binary) -> Type {
        match bin_op {
            Binary::Add(_, _)
            | Binary::Sub(_, _)
            | Binary::Mul(_, _)
            | Binary::Div(_, _)
            | Binary::Eq(_, _)
            | Binary::Ne(_, _)
            | Binary::Gte(_, _)
            | Binary::Gt(_, _)
            | Binary::Lt(_, _)
            | Binary::Lte(_, _) => Type::Bool,
        }
    }

    fn unary_op_to_type(&self, unary_op: Unary) -> Type {
        match unary_op {
            Unary::Plus(expr) => match expr {
                Expr::Op(op) => match *op {
                    Op::Unary(unary) => match unary {
                        Unary::Plus(_) | Unary::Minus(_) => Type::Number,
                    },
                    Op::Binary(binary) => self.bin_op_to_type(binary),
                },
                Expr::Value(v) => v.into(),
                Expr::GetVar(name) => self.get_var(&name).into(),
                Expr::Fn(_, _, t) => t,
                Expr::Expr(expr) => self.expr_to_value(*expr).into(),
                Expr::SetVar(_, _) => todo!(),
            },
            Unary::Minus(expr) => match expr {
                Expr::Op(op) => match *op {
                    Op::Unary(unary) => match unary {
                        Unary::Plus(_) | Unary::Minus(_) => Type::Number,
                    },
                    Op::Binary(binary) => self.bin_op_to_type(binary),
                },
                Expr::Value(v) => v.into(),
                Expr::GetVar(name) => self.get_var(&name).into(),
                Expr::Fn(_, _, t) => t,
                Expr::Expr(expr) => self.expr_to_value(*expr).into(),
                Expr::SetVar(_, _) => todo!(),
            },
        }
    }

    fn op_to_type(&self, op: Op) -> Type {
        match op {
            Op::Unary(unary) => self.unary_op_to_type(unary),
            Op::Binary(binary) => self.bin_op_to_type(binary),
        }
    }

    fn expr_to_type(&self, expr: Expr) -> Type {
        match expr {
            Expr::Fn(_, _, t) => t,
            Expr::Op(op) => self.op_to_type(*op),
            _ => self.expr_to_value(expr).into(),
        }
    }

    fn get_var(&self, name: &str) -> Value {
        self.env
            .get(name)
            .unwrap_or_else(|| panic!("Could not find var: {}", name))
    }

    pub fn check(&mut self) -> bool {
        for expr in &self.body {
            let typ = match expr {
                Expr::Op(op) => self.op_to_type(*op.clone()),
                Expr::Value(value) => Type::from(value.clone()),
                Expr::SetVar(name, v) => {
                    self.env.set(name, *v.clone());
                    Type::from(*v.clone())
                }
                Expr::GetVar(name) => self.get_var(name).into(),
                Expr::Fn(_, _, t) => t.clone(),
                Expr::Expr(expr) => self.expr_to_type(*expr.clone()),
            };
            if typ == Type::Invalid {
                return false;
            }
        }
        true
    }
}

impl From<Value> for Type {
    fn from(value: Value) -> Type {
        match value {
            Value::Number(_) => Type::Number,
            Value::Bool(_) => Type::Bool,
            Value::String(_) => Type::String,
            Value::List(_) => Type::List,
        }
    }
}

impl From<Type> for Value {
    fn from(t: Type) -> Value {
        match t {
            Type::Invalid => panic!("Cannot convert invalid type to value"),
            Type::Number => Value::Number(0.0),
            Type::Bool => Value::Bool(false),
            Type::String => Value::String("".to_string()),
            Type::List => Value::List(vec![]),
        }
    }
}

use std::fmt;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::List(l) => write!(f, "{:?}", l),
        }
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Self::List(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<Value> for f64 {
    fn from(val: Value) -> Self {
        match val {
            Value::Number(n) => n,
            _ => panic!("Could not convert to f64"),
        }
    }
}

trait SumValue {
    fn sum(&self) -> f64;
}

impl SumValue for Vec<Value> {
    fn sum(&self) -> f64 {
        let mut res = 0.0;
        for item in self {
            match item {
                Value::Number(n) => res += n,
                _ => panic!("Into on wrong type"),
            }
        }
        res
    }
}
