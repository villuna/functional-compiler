use std::fmt;

use crate::lexer::BIN_OPERATORS;
use crate::parser::*;

pub trait Sequence: Clone + Sized + fmt::Display {
    fn nil() -> Self;
    fn newline() -> Self;
    fn from_str(input: &str) -> Self;

    fn append(self, other: Self) -> Self;
    fn indent(self) -> Self;

    fn concat(seq: &[Self]) -> Self {
        if seq.is_empty() {
            Self::nil()
        } else {
            seq[0].clone().append(Self::concat(&seq[1..]))
        }
    }

    fn interleave(sep: Self, seq: &[Self]) -> Self {
        if seq.len() == 1 {
            seq[0].clone()
        } else {
            Self::concat(&[
                seq[0].clone(),
                sep.clone(),
                Self::interleave(sep, &seq[1..]),
            ])
        }
    }
}

// This is how it's implemented in the textbook
// Obviously what's good for a FP language like miranda (or haskell) isn't necessarily good for
// rust, so I'll do a different implementation later
#[derive(Clone)]
pub enum FpSeq {
    None,
    Str(String),
    Append(Box<FpSeq>, Box<FpSeq>),
    Indent(Box<FpSeq>),
    Newline,
}

impl fmt::Display for FpSeq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", flatten(0, &[(self.clone(), 0)]))
    }
}

fn flatten(col: usize, seqs: &[(FpSeq, usize)]) -> String {
    if seqs.is_empty() {
        return "".to_string();
    }

    match &seqs[0] {
        (FpSeq::None, _) => flatten(col, &seqs[1..]),

        (FpSeq::Str(st), _) => {
            let mut s = st.clone();
            s.push_str(&flatten(col + s.len(), &seqs[1..]));
            s
        }

        (FpSeq::Append(a, b), ind) => {
            // Very inefficient but again, this is functional style
            // I will rewrite it later maybe? but it's good enough to copy the book
            let mut v = Vec::new();
            v.push((*a.clone(), *ind));
            v.push((*b.clone(), *ind));
            v.extend_from_slice(&seqs[1..]);
            flatten(col, &v)
        }

        (FpSeq::Newline, ind) => {
            let mut s = "\n".to_string();
            for _ in 0..*ind {
                s.push(' ');
            }
            s.push_str(&flatten(*ind, &seqs[1..]));
            s
        }

        (FpSeq::Indent(seq), _) => {
            let mut v = vec![(*seq.clone(), col)];
            v.extend_from_slice(&seqs[1..]);
            flatten(col, &v)
        }
    }
}

impl Sequence for FpSeq {
    fn nil() -> Self {
        Self::None
    }

    fn newline() -> Self {
        Self::Newline
    }

    fn from_str(input: &str) -> Self {
        Self::Str(input.to_string())
    }

    fn append(self, other: Self) -> Self {
        Self::Append(Box::new(self), Box::new(other))
    }

    fn indent(self) -> Self {
        Self::Indent(Box::new(self))
    }
}

impl CoreExpr {
    pub fn pretty_print<S: Sequence>(&self) -> S {
        match &self {
            &Expr::Number(n) => S::from_str(&format!("{n}")),
            &Expr::Variable(v) => S::from_str(&format!("{v}")),
            &Expr::Application(box Expr::Application(box Expr::Variable(op), e1), e2)
                if { BIN_OPERATORS.contains(&op.as_str()) } =>
            {
                e1.pretty_print::<S>()
                    .append(S::from_str(&format!(" {} ", op)))
                    .append(e2.pretty_print())
            }
            &Expr::Application(a, b) => a
                .pretty_print::<S>()
                .append(S::from_str(" "))
                .append(b.pretty_print()),
            &Expr::Let {
                is_recursive,
                definitions,
                body,
            } => {
                let keyword = if *is_recursive {
                    S::from_str("letrec")
                } else {
                    S::from_str("let")
                };

                S::concat(&[
                    keyword,
                    S::newline(),
                    S::from_str("  "),
                    Self::ppr_definitions::<S>(definitions).indent(),
                    S::newline(),
                    S::from_str("in "),
                    body.pretty_print::<S>(),
                ])
            }
            _ => todo!(),
        }
    }

    fn ppr_definitions<S: Sequence>(defs: &[(String, CoreExpr)]) -> S {
        let sep = S::from_str(";").append(S::newline());
        S::interleave(
            sep,
            &defs.iter().map(Self::ppr_definition).collect::<Vec<_>>(),
        )
    }

    fn ppr_definition<S: Sequence>((name, expr): &(String, CoreExpr)) -> S {
        S::concat(&[
            S::from_str(name),
            S::from_str(" = "),
            expr.pretty_print::<S>().indent(),
        ])
    }
}

impl fmt::Display for CoreExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty_print::<FpSeq>())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_pretty_print() {
        let expr: CoreExpr = Expr::Application(
            Box::new(Expr::Variable("f".into())),
            Box::new(Expr::Variable("x".into())),
        );

        let formatted = format!("{expr}");
        assert_eq!(formatted, "f x");

        let expr: CoreExpr = Expr::Let {
            is_recursive: false,
            definitions: vec![
                ("x".into(), Expr::Number(123)),
                (
                    "xx".into(),
                    Expr::Application(
                        Box::new(Expr::Variable("square".into())),
                        Box::new(Expr::Variable("x".into())),
                    ),
                ),
            ],
            body: Box::new(Expr::Application(
                Box::new(Expr::Application(
                    Box::new(Expr::Variable("+".into())),
                    Box::new(Expr::Variable("xx".into())),
                )),
                Box::new(Expr::Variable("x".into())),
            )),
        };

        let formatted = format!("{expr}");
        println!("{formatted}");
        let expected = "let
  x = 123;
  xx = square x
in xx + x";
        assert_eq!(expected, formatted);
    }
}
