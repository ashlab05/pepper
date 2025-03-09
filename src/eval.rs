use crate::parse::{parse_interpolation, Atom, Expr, Operator};
use std::collections::HashMap;
// Removed unused import: std::rc::Rc

type Context = HashMap<String, Expr>;

// Add a helper for numeric operations to reduce code duplication
fn numeric_op<F, G>(
    left: &Expr, 
    right: &Expr, 
    int_op: F, 
    float_op: G
) -> Expr 
where 
    F: Fn(i64, i64) -> i64,
    G: Fn(f64, f64) -> f64
{
    match (left, right) {
        (Expr::Constant(Atom::Number(l)), Expr::Constant(Atom::Number(r))) => {
            Expr::Constant(Atom::Number(int_op(*l, *r)))
        },
        (Expr::Constant(Atom::Float(l)), Expr::Constant(Atom::Float(r))) => {
            Expr::Constant(Atom::Float(float_op(*l, *r)))
        },
        (Expr::Constant(Atom::Number(l)), Expr::Constant(Atom::Float(r))) => {
            Expr::Constant(Atom::Float(float_op(*l as f64, *r)))
        },
        (Expr::Constant(Atom::Float(l)), Expr::Constant(Atom::Number(r))) => {
            Expr::Constant(Atom::Float(float_op(*l, *r as f64)))
        },
        _ => panic!("Can't perform numeric operation on {left} and {right}")
    }
}

pub fn eval(exprs: Vec<Expr>) {
    // Create new context for variables, then evaluate each expression
    let mut context = Context::new();
    for expr in exprs {
        eval_expr(expr, &mut context);
    }
}

fn eval_expr(expr: Expr, context: &mut Context) -> Expr {
    match expr {
        Expr::Void | Expr::Closure(_, _) | Expr::Array(_) => expr,
        
        Expr::Return(expr) => Expr::Return(Box::new(eval_expr(*expr, context))),
        
        Expr::Constant(Atom::String(ref string)) => {
            match parse_interpolation(string) {
                Ok((_, exprs)) if exprs.len() > 1 => {
                    let mut output = String::with_capacity(string.len() * 2);
                    for mut expr in exprs {
                        // Evaluate until fixed point
                        let mut prev_expr = expr.clone();
                        loop {
                            let new_expr = eval_expr(expr, context);
                            if prev_expr == new_expr {
                                expr = new_expr;
                                break;
                            }
                            prev_expr = new_expr.clone();
                            expr = new_expr;
                        }
                        output.push_str(&expr.to_string());
                    }
                    Expr::Constant(Atom::String(output))
                },
                _ => expr,
            }
        },
        
        Expr::Constant(ref atom) => match atom {
            Atom::Name(name) => context
                .get(name)
                .expect(&format!("{name} doesn't exist!"))
                .clone(),
            _ => expr,
        },
        
        Expr::Let(name, expr) => {
            let expr = eval_expr(*expr, context);
            context.insert(name, expr);
            Expr::Void
        },
        
        Expr::Binary(left, operator, right) => {
            let left = eval_expr(*left, context);
            let right = eval_expr(*right, context);
            
            match operator {
                Operator::Add => numeric_op(&left, &right, 
                    |a, b| a + b, 
                    |a, b| a + b),
                Operator::Subtract => numeric_op(&left, &right, 
                    |a, b| a - b, 
                    |a, b| a - b),
                Operator::Multiply => numeric_op(&left, &right, 
                    |a, b| a * b, 
                    |a, b| a * b),
                Operator::Divide => numeric_op(&left, &right, 
                    |a, b| a / b, 
                    |a, b| a / b),
                Operator::Modulo => numeric_op(&left, &right, 
                    |a, b| a % b, 
                    |a, b| a % b),
                _ => panic!("Unexpected operator in binary expression")
            }
        },
        
        Expr::Compare(left, operator, right) => {
            let left = eval_expr(*left, context);
            let right = eval_expr(*right, context);
            
            // Fixed: Added clone to operator to prevent move
            match (&left, operator.clone(), &right) {
                (Expr::Constant(Atom::Number(left)), Operator::LessThan, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left < right))
                },
                (Expr::Constant(Atom::Number(left)), Operator::LessThanEqual, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left <= right))
                },
                (Expr::Constant(Atom::Number(left)), Operator::GreaterThan, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left > right))
                },
                (Expr::Constant(Atom::Number(left)), Operator::GreaterThanEqual, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left >= right))
                },
                (Expr::Constant(Atom::Number(left)), Operator::Equal, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left == right))
                },
                (Expr::Constant(Atom::Number(left)), Operator::NotEqual, Expr::Constant(Atom::Number(right))) => {
                    Expr::Constant(Atom::Boolean(left != right))
                },
                
                // Handle float comparisons
                (Expr::Constant(Atom::Float(left)), op, Expr::Constant(Atom::Float(right))) => {
                    let result = match op {
                        Operator::LessThan => left < right,
                        Operator::LessThanEqual => left <= right,
                        Operator::GreaterThan => left > right,
                        Operator::GreaterThanEqual => left >= right,
                        Operator::Equal => (left - right).abs() < f64::EPSILON,
                        Operator::NotEqual => (left - right).abs() >= f64::EPSILON,
                        _ => panic!("Invalid operator for float comparison")
                    };
                    Expr::Constant(Atom::Boolean(result))
                },
                
                // Add mixed number/float comparisons
                (Expr::Constant(Atom::Number(left)), op, Expr::Constant(Atom::Float(right))) => {
                    let left = *left as f64;
                    let result = match op {
                        Operator::LessThan => left < *right,
                        Operator::LessThanEqual => left <= *right,
                        Operator::GreaterThan => left > *right,
                        Operator::GreaterThanEqual => left >= *right,
                        Operator::Equal => (left - *right).abs() < f64::EPSILON,
                        Operator::NotEqual => (left - *right).abs() >= f64::EPSILON,
                        _ => panic!("Invalid operator for mixed number comparison")
                    };
                    Expr::Constant(Atom::Boolean(result))
                },
                
                (Expr::Constant(Atom::Float(left)), op, Expr::Constant(Atom::Number(right))) => {
                    let right = *right as f64;
                    let result = match op {
                        Operator::LessThan => *left < right,
                        Operator::LessThanEqual => *left <= right,
                        Operator::GreaterThan => *left > right,
                        Operator::GreaterThanEqual => *left >= right,
                        Operator::Equal => (*left - right).abs() < f64::EPSILON,
                        Operator::NotEqual => (*left - right).abs() >= f64::EPSILON,
                        _ => panic!("Invalid operator for mixed number comparison")
                    };
                    Expr::Constant(Atom::Boolean(result))
                },
                
                // String equality
                (Expr::Constant(Atom::String(left)), Operator::Equal, Expr::Constant(Atom::String(right))) => {
                    Expr::Constant(Atom::Boolean(left == right))
                },
                (Expr::Constant(Atom::String(left)), Operator::NotEqual, Expr::Constant(Atom::String(right))) => {
                    Expr::Constant(Atom::Boolean(left != right))
                },
                
                // Boolean equality
                (Expr::Constant(Atom::Boolean(left)), Operator::Equal, Expr::Constant(Atom::Boolean(right))) => {
                    Expr::Constant(Atom::Boolean(left == right))
                },
                (Expr::Constant(Atom::Boolean(left)), Operator::NotEqual, Expr::Constant(Atom::Boolean(right))) => {
                    Expr::Constant(Atom::Boolean(left != right))
                },
                
                _ => panic!("Can't compare {left} and {right} with {:?}", operator),
            }
        },
        
        Expr::If(statement, then, otherwise) => {
            if let Expr::Constant(Atom::Boolean(value)) = eval_expr(*statement, context) {
                if value {
                    for expr in then {
                        eval_expr(expr, context);
                    }
                } else if let Some(body) = otherwise {
                    for expr in body {
                        eval_expr(expr, context);
                    }
                }
            }
            Expr::Void
        },
        
        Expr::Call(name, args) => {
            if name == "println" {
                for arg in args {
                    print!("{}", eval_expr(arg, context));
                }
                println!();
                Expr::Void
            } else {
                // Fixed: Get the closure and clone it before evaluating arguments
                match context.get(&name).cloned() {
                    Some(Expr::Closure(parameters, body)) => {
                        // Evaluate all arguments first to avoid borrowing context while using it
                        let evaluated_args: Vec<_> = args.iter()
                            .map(|arg| eval_expr(arg.clone(), context))
                            .collect();
                        
                        // Create new scope for function call
                        let mut scope = context.clone();
                        
                        // Bind parameters to evaluated arguments
                        for (parameter, expr) in parameters.iter().zip(evaluated_args.iter()) {
                            scope.insert(parameter.clone(), expr.clone());
                        }
                        
                        // Execute function body
                        for expr in body.iter() {
                            match eval_expr(expr.clone(), &mut scope) {
                                Expr::Return(expr) => return *expr,
                                _ => {}
                            }
                        }
                        Expr::Void
                    },
                    _ => panic!("Function `{name}` doesn't exist."),
                }
            }
        },
        
        Expr::Function(name, args, body) => {
            context.insert(name, Expr::Closure(args, body));
            Expr::Void
        },
        
        Expr::For(name, collection, body) => {
            let array = eval_expr(*collection, context);
            match array {
                Expr::Array(items) => {
                    let mut scope = context.clone();
                    for item in items {
                        scope.insert(name.clone(), item);
                        for expr in &body {
                            eval_expr(expr.clone(), &mut scope);
                        }
                    }
                    Expr::Void
                },
                _ => panic!("Can't loop over `{array}`"),
            }
        },
        
        Expr::Get(name, index) => match context.get(&name) {
            Some(Expr::Array(items)) => {
                if index < items.len() {
                    eval_expr(items[index].clone(), context)
                } else {
                    panic!("Index {index} out of bounds for array {name} with length {}", items.len())
                }
            },
            Some(invalid) => panic!("Expected array, got {invalid}"),
            None => panic!("Couldn't find {name}"),
        },
    }
}