#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Value {
    Number(f64),
    Bool(bool),
    String(String),
    List(Vec<Value>),
    Var(Box<Value>, Type),
    Fn(String, Vec<Value>, Type),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Unary {
    Plus(Value),
    Minus(Value),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Binary {
    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Op(Op),
    Value(Value),
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

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Number(_) => Self::Number,
            Value::Bool(_) => Self::Bool,
            Value::String(_) => Self::String,
            Value::List(_) => Self::List,
            Value::Var(_, t) => t,
            Value::Fn(_, _, t) => t,
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

impl Expr {
    pub fn eval(&self) -> Value {
        use Value::*;
        match self {
            Expr::Op(op) => match op {
                Op::Unary(unary) => match unary {
                    Unary::Plus(_) => todo!(),
                    Unary::Minus(_) => todo!(),
                },
                Op::Binary(binary) => match binary {
                    Binary::Add(_, _) => todo!(),
                    Binary::Sub(_, _) => todo!(),
                    Binary::Mul(_, _) => todo!(),
                    Binary::Div(_, _) => todo!(),
                },
            },
            Expr::Value(value) => match value {
                Number(_) | Bool(_) | String(_) | List(_) => value.clone(),
                Var(v, _) => *v.to_owned(),
                Fn(name, args, _) => match name.as_str() {
                    "sum" => Number(args.sum()),
                    _ => panic!("Unsupported method called"),
                },
            },
        }
    }

    pub fn check(&self) -> Type {
        use Value::*;
        match self {
            Expr::Op(op) => match op {
                Op::Unary(unary) => match unary {
                    Unary::Plus(value) => match value {
                        Number(_) => Type::Number,
                        List(_) | Bool(_) | String(_) | Var(_, _) | Fn(_, _, _) => Type::Invalid,
                    },
                    Unary::Minus(value) => match value {
                        Number(_) => Type::Number,
                        List(_) | Bool(_) | String(_) | Var(_, _) | Fn(_, _, _) => Type::Invalid,
                    },
                },
                Op::Binary(binary) => match binary {
                    Binary::Add(left, right) => match (left, right) {
                        (Number(_), Number(_)) => Type::Number,
                        (String(_), String(_)) => Type::String,
                        (List(_), List(_)) => Type::List,
                        _ => Type::Invalid,
                    },
                    Binary::Sub(left, right) => match (left, right) {
                        (Number(_), Number(_)) => Type::Number,
                        _ => Type::Invalid,
                    },
                    Binary::Mul(left, right) => match (left, right) {
                        (Number(_), Number(_)) => Type::Number,
                        (String(_), Number(_)) => Type::String,
                        (List(_), Number(_)) => Type::List,
                        _ => Type::Invalid,
                    },
                    Binary::Div(left, right) => match (left, right) {
                        (Number(_), Number(_)) => Type::Number,
                        _ => Type::Invalid,
                    },
                },
            },
            Expr::Value(value) => match value {
                Number(_) | Bool(_) | String(_) | List(_) => Type::from(value.clone()),
                Var(_, typ) => typ.clone(),
                Fn(name, _, typ) => match name.as_str() {
                    "sum" => typ.clone(),
                    _ => panic!("Unsupported method called"),
                },
            },
        }
    }
}
