use crate::parser::*;
use once_cell::unsync::Lazy;
use std::rc::Rc;

// Combinator definitions for the prelude of the language
// typing this out sucked bad but whatever
thread_local! {
    pub static PRELUDE_DEFS: Lazy<CoreProgram> = Lazy::new(|| {
        vec![
            CoreScDef {
                name: "I".into(),
                bindings: vec!["x".into()],
                expression: Expr::Variable("x".into()),
            },
            CoreScDef {
                name: "K".into(),
                bindings: vec!["x".into(), "y".into()],
                expression: Expr::Variable("x".into()),
            },
            CoreScDef {
                name: "K1".into(),
                bindings: vec!["x".into(), "y".into()],
                expression: Expr::Variable("y".into()),
            },
            CoreScDef {
                name: "S".into(),
                bindings: vec!["f".into(), "g".into(), "x".into()],
                expression: Expr::Application(
                    Rc::new(Expr::Application(
                        Rc::new(Expr::Variable("f".into())),
                        Rc::new(Expr::Variable("x".into())),
                    )),
                    Rc::new(Expr::Application(
                        Rc::new(Expr::Variable("g".into())),
                        Rc::new(Expr::Variable("x".into())),
                    )),
                ),
            },
            CoreScDef {
                name: "compose".into(),
                bindings: vec!["f".into(), "g".into(), "x".into()],
                expression: Expr::Application(
                    Rc::new(Expr::Variable("f".into())),
                    Rc::new(Expr::Application(
                        Rc::new(Expr::Variable("g".into())),
                        Rc::new(Expr::Variable("x".into())),
                    )),
                ),
            },
            CoreScDef {
                name: "twice".into(),
                bindings: vec!["f".into()],
                expression: Expr::Application(
                    Rc::new(Expr::Variable("f".into())),
                    Rc::new(Expr::Variable("f".into())),
                ),
            },
        ]
    });
}
