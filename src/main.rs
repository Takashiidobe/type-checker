use type_checker::*;

fn main() {
    let set_x = Expr::SetVar("x".to_string(), Box::new(Value::from(10.0)));

    let add_10_to_x = Expr::Op(Box::new(Op::Binary(Binary::Add(
        Expr::GetVar("x".to_string()),
        Expr::Value(Value::Number(10.0)),
    ))));

    let flip_x = Expr::Op(Box::new(Op::Unary(Unary::Minus(Expr::GetVar(
        "x".to_string(),
    )))));

    let fn_x = Expr::Fn("example_fn".to_string(), vec![], Type::Bool);

    let mut program = Program::from(vec![set_x, add_10_to_x, flip_x, fn_x]);

    dbg!(program.check());
    dbg!(program);
}
