use type_checker::*;

fn main() {
    let f = Expr::Value(Value::Fn(
        "sum".to_string(),
        vec![1.0.into(), 2.0.into()],
        Type::Number,
    ));
    let s = Expr::Op(Op::Binary(Binary::Add(Value::from(1.0), Value::from(2.0))));

    let x = Expr::Op(Op::Binary(Binary::Add(
        Value::from("Lol"),
        Value::from("Wat"),
    )));

    let z = Expr::Op(Op::Binary(Binary::Add(
        Value::from(vec![1.0.into()]),
        Value::from(vec![2.0.into()]),
    )));

    dbg!(f.check());
    dbg!(s.check());
    dbg!(x.check());
    dbg!(z.check());
}
