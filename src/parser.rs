#![allow(unused)]

use std::rc::Rc;

use nom::{
    branch::alt,
    combinator::{eof, opt},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, IResult, Parser,
};
// Refer to section 1.3
use once_cell::sync::Lazy;

use crate::lexer::{lex, Token, BIN_OPERATORS};

const KEYWORDS: &[&str] = &["let", "letrec", "case", "in", "of", "Pack"];

pub type Tag = i64;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Alter<T> {
    pub tag: Tag,
    pub variables: Vec<T>,
    pub expression: Expr<T>,
}

pub type CoreExpr = Expr<String>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr<T> {
    Variable(String),
    Number(i64),
    Constructor {
        tag: Tag,
        arity: usize,
    },
    Application(Rc<Expr<T>>, Rc<Expr<T>>),
    Let {
        is_recursive: bool,
        definitions: Vec<(T, Expr<T>)>,
        body: Rc<Expr<T>>,
    },
    Case {
        expression: Rc<Expr<T>>,
        alternatives: Vec<Alter<T>>,
    },
    Lambda {
        variables: Vec<T>,
        expression: Rc<Expr<T>>,
    },
}

impl<T> Expr<T> {
    pub fn is_atomic(&self) -> bool {
        match self {
            &Expr::Variable(_) | &Expr::Number(_) => true,
            _ => false,
        }
    }
}

pub fn binders_of<T, U>(defs: &[(T, U)]) -> impl Iterator<Item = &T> {
    defs.iter().map(|(t, _)| t)
}

pub fn rhss_of<T, U>(defs: &[(T, U)]) -> impl Iterator<Item = &U> {
    defs.iter().map(|(_, u)| u)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScDef<T> {
    pub name: String,
    pub bindings: Vec<T>,
    pub expression: Expr<T>,
}

pub type CoreScDef = ScDef<String>;

pub type Program<T> = Vec<ScDef<T>>;
pub type CoreProgram = Program<String>;

type TokenStream<'i, 'ts> = &'ts [Token<'i>];

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CoreParseErrorKind<'i> {
    LexError,
    UnexpectedToken,
    UnexpectedEOF,
    NomError(nom::error::ErrorKind),
    Append {
        kind: nom::error::ErrorKind,
        child_error: Rc<CoreParseError<'i>>,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CoreParseError<'i> {
    pub input: Vec<Token<'i>>,
    pub kind: CoreParseErrorKind<'i>,
}

impl<'i> nom::error::ParseError<TokenStream<'i, '_>> for CoreParseError<'i> {
    fn from_error_kind(input: TokenStream<'i, '_>, kind: nom::error::ErrorKind) -> Self {
        Self {
            input: input.to_owned(),
            kind: CoreParseErrorKind::NomError(kind),
        }
    }

    fn append(input: TokenStream<'i, '_>, kind: nom::error::ErrorKind, other: Self) -> Self {
        Self {
            input: input.to_owned(),
            kind: CoreParseErrorKind::Append {
                kind,
                child_error: Rc::new(other),
            },
        }
    }
}

fn token<'i: 'ts, 'ts>(
    s: &'i str,
) -> impl FnMut(TokenStream<'i, 'ts>) -> IResult<TokenStream<'i, 'ts>, &'i str, CoreParseError<'i>>
{
    move |input| {
        if input.is_empty() {
            Err(nom::Err::Error(CoreParseError {
                kind: CoreParseErrorKind::UnexpectedEOF,
                input: input.to_owned(),
            }))
        } else if input[0] == s {
            Ok((&input[1..], s))
        } else {
            Err(nom::Err::Error(CoreParseError {
                kind: CoreParseErrorKind::UnexpectedToken,
                input: input.to_owned(),
            }))
        }
    }
}

fn satisfy<'i: 'ts, 'ts>(
    mut predicate: impl FnMut(&Token) -> bool,
) -> impl FnMut(TokenStream<'i, 'ts>) -> IResult<TokenStream<'i, 'ts>, Token<'i>, CoreParseError<'i>>
{
    move |input| {
        if input.is_empty() {
            Err(nom::Err::Error(CoreParseError {
                kind: CoreParseErrorKind::UnexpectedEOF,
                input: input.to_owned(),
            }))
        } else if predicate(&input[0]) {
            Ok((&input[1..], input[0]))
        } else {
            Err(nom::Err::Error(CoreParseError {
                kind: CoreParseErrorKind::UnexpectedToken,
                input: input.to_owned(),
            }))
        }
    }
}

fn var<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, &'i str, CoreParseError<'i>> {
    satisfy(|t| !KEYWORDS.contains(t) && t.chars().next().is_some_and(|c| c.is_alphabetic()))(input)
}

fn num<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, i64, CoreParseError<'i>> {
    satisfy(|t| t.chars().all(|c| c.is_numeric()))
        .map(|s| s.parse::<i64>().unwrap())
        .parse(input)
}

fn pack_constructor<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    tuple((token("Pack"), token("{"), num, token(","), num, token("}")))
        .map(|(_, _, n1, _, n2, _)| CoreExpr::Constructor {
            tag: n1 as _,
            arity: n2 as _,
        })
        .parse(input)
}

fn atomic_expr<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    alt((
        var.map(|name| CoreExpr::Variable(name.to_string())),
        num.map(CoreExpr::Number),
        pack_constructor,
        delimited(token("("), expr, token(")")),
    ))(input)
}

fn binop<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, &'i str, CoreParseError<'i>> {
    satisfy(|t| BIN_OPERATORS.contains(t))(input)
}

fn defn<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, (String, CoreExpr), CoreParseError<'i>> {
    separated_pair(var.map(|s| s.to_string()), token("="), expr)(input)
}

fn defns<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, Vec<(String, CoreExpr)>, CoreParseError<'i>> {
    separated_list1(token(";"), defn)(input)
}

fn let_expr<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    tuple((
        alt((token("let"), token("letrec"))),
        defns,
        token("in"),
        expr,
    ))
    .map(|(keyword, definitions, _, expr)| CoreExpr::Let {
        is_recursive: keyword == "letrec",
        definitions,
        body: Rc::new(expr),
    })
    .parse(input)
}

fn core_alt<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, Alter<String>, CoreParseError<'i>> {
    tuple((
        delimited(token("<"), num, token(">")),
        many0(var),
        token("->"),
        expr,
    ))
    .map(|(tag, variables, _, expression)| Alter {
        tag,
        variables: variables.iter().map(|s| s.to_string()).collect(),
        expression,
    })
    .parse(input)
}

fn alts<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, Vec<Alter<String>>, CoreParseError<'i>> {
    separated_list1(token(";"), core_alt)(input)
}

fn case_expr<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    tuple((token("case"), expr, token("of"), alts))
        .map(|(_, expression, _, alternatives)| CoreExpr::Case {
            expression: Rc::new(expression),
            alternatives,
        })
        .parse(input)
}

fn lambda<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    tuple((token("\\"), many1(var), token("."), expr))
        .map(|(_, variables, _, expression)| CoreExpr::Lambda {
            variables: variables.iter().map(|s| s.to_string()).collect(),
            expression: Rc::new(expression),
        })
        .parse(input)
}

fn expr<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    alt((let_expr, case_expr, lambda, expr1))(input)
}

// I hate this shit and tbh i should have used a parser generator
// my bad
// maybe i'll make a parser generator for rust some time that sounds cool
// wait i should have used pest
// oh it's so joever
// wait but pest sucks
// we're so barack
fn infix<'i: 'ts, 'ts, P1, P2>(
    mut p1: P1,
    mut op_p2: P2,
) -> impl FnMut(TokenStream<'i, 'ts>) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>>
where
    P1: Parser<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>>,
    P2: Parser<TokenStream<'i, 'ts>, (&'i str, CoreExpr), CoreParseError<'i>>,
{
    move |input| {
        let p1 = |input| p1.parse(input);
        let op_p2 = |input| op_p2.parse(input);

        pair(p1, opt(op_p2))
            .map(|(a, mp)| match mp {
                None => a,
                Some((op, b)) => CoreExpr::Application(
                    Rc::new(CoreExpr::Application(
                        Rc::new(CoreExpr::Variable(op.to_string())),
                        Rc::new(a),
                    )),
                    Rc::new(b),
                ),
            })
            .parse(input)
    }
}

fn expr1<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    infix(expr2, pair(token("|"), expr1))(input)
}

fn expr2<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    infix(expr3, pair(token("&"), expr2))(input)
}

fn expr3<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    let mut relop = alt((
        token("<"),
        token("<="),
        token("=="),
        token("<>"),
        token(">="),
        token(">"),
    ));

    infix(expr4, pair(relop, expr4))(input)
}

fn expr4<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    infix(
        expr5,
        alt((pair(token("+"), expr4), pair(token("-"), expr5))),
    )(input)
}

fn expr5<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    infix(
        expr6,
        alt((pair(token("*"), expr5), pair(token("/"), expr6))),
    )(input)
}

fn expr6<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreExpr, CoreParseError<'i>> {
    many1(atomic_expr)
        .map(|exprs| {
            exprs
                .into_iter()
                .reduce(|l, r| CoreExpr::Application(Rc::new(l), Rc::new(r)))
                .unwrap()
        })
        .parse(input)
}

fn sc_def<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, CoreScDef, CoreParseError<'i>> {
    tuple((
        var,        // Name of function
        many0(var), // Parameters
        token("="),
        expr, // Body
    ))
    .map(|(name, bindings, _, body)| CoreScDef {
        name: name.to_string(),
        bindings: bindings.into_iter().map(|s| s.to_string()).collect(),
        expression: body,
    })
    .parse(input)
}

fn program<'i: 'ts, 'ts>(
    input: TokenStream<'i, 'ts>,
) -> IResult<TokenStream<'i, 'ts>, Vec<CoreScDef>, CoreParseError<'i>> {
    separated_list1(token(";"), sc_def)(input)
}

fn parse_syntax<'i: 'ts, 'ts>(input: &'ts [Token<'i>]) -> Result<CoreProgram, CoreParseError<'i>> {
    terminated(program, eof)(input)
        .finish()
        .map(move |(_, r)| r)
}

pub fn parse(input: &str) -> Result<CoreProgram, CoreParseError> {
    lex(input)
        .ok_or(CoreParseError {
            kind: CoreParseErrorKind::LexError,
            input: Vec::new(),
        })
        .and_then(|v| parse_syntax(&v))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sc_defs() {
        let def1 = lex("main = double 21").unwrap();
        let exp1 = CoreScDef {
            name: "main".into(),
            bindings: vec![],
            expression: Expr::Application(
                Rc::new(Expr::Variable("double".into())),
                Rc::new(Expr::Number(21)),
            ),
        };

        let res1 = sc_def(&def1).unwrap();
        assert_eq!(
            // hey rust why do i have to cast to the type here? wtf
            (&[] as &[&str], exp1),
            res1,
        );

        let def2 = lex("double x = x + x").unwrap();
        let exp2 = CoreScDef {
            name: "double".into(),
            bindings: vec!["x".into()],
            expression: Expr::Application(
                Rc::new(Expr::Application(
                    Rc::new(Expr::Variable("+".into())),
                    Rc::new(Expr::Variable("x".into())),
                )),
                Rc::new(Expr::Variable("x".into())),
            ),
        };

        let res2 = sc_def(&def2).unwrap();
        assert_eq!((&[] as &[&str], exp2), res2,);
    }

    #[test]
    fn small_program_example() {
        let program = "\
        main = double 21 ;\n\
        double x = x + x";

        let expected = vec![
            CoreScDef {
                name: "main".into(),
                bindings: vec![],
                expression: Expr::Application(
                    Rc::new(Expr::Variable("double".into())),
                    Rc::new(Expr::Number(21)),
                ),
            },
            CoreScDef {
                name: "double".into(),
                bindings: vec!["x".into()],
                expression: Expr::Application(
                    Rc::new(Expr::Application(
                        Rc::new(Expr::Variable("+".into())),
                        Rc::new(Expr::Variable("x".into())),
                    )),
                    Rc::new(Expr::Variable("x".into())),
                ),
            },
        ];

        let result = parse(program).unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_triple_add() {
        let test = lex("x + y + z").unwrap();

        let expected = CoreExpr::Application(
            Rc::new(CoreExpr::Application(
                Rc::new(CoreExpr::Variable("+".into())),
                Rc::new(CoreExpr::Variable("x".into())),
            )),
            Rc::new(CoreExpr::Application(
                Rc::new(CoreExpr::Application(
                    Rc::new(CoreExpr::Variable("+".into())),
                    Rc::new(CoreExpr::Variable("y".into())),
                )),
                Rc::new(CoreExpr::Variable("z".into())),
            )),
        );

        let res = expr(&test).unwrap();

        assert_eq!((&[] as &[&str], expected), res);
    }

    #[test]
    fn test_let_binding() {
        let test = "let
  x = 26;
  y = x * x;
  z = y * y + y * y
in x + y + z";
        let input = lex(test).unwrap();
        dbg!(input.clone());
        let res = let_expr(&input).unwrap();

        let expected = CoreExpr::Let {
            is_recursive: false,
            definitions: vec![
                ("x".into(), CoreExpr::Number(26)),
                (
                    "y".into(),
                    CoreExpr::Application(
                        Rc::new(CoreExpr::Application(
                            Rc::new(CoreExpr::Variable("*".into())),
                            Rc::new(CoreExpr::Variable("x".into())),
                        )),
                        Rc::new(CoreExpr::Variable("x".into())),
                    ),
                ),
                (
                    "z".into(),
                    CoreExpr::Application(
                        Rc::new(CoreExpr::Application(
                            Rc::new(CoreExpr::Variable("+".into())),
                            Rc::new(CoreExpr::Application(
                                Rc::new(CoreExpr::Application(
                                    Rc::new(CoreExpr::Variable("*".into())),
                                    Rc::new(CoreExpr::Variable("y".into())),
                                )),
                                Rc::new(CoreExpr::Variable("y".into())),
                            )),
                        )),
                        Rc::new(CoreExpr::Application(
                            Rc::new(CoreExpr::Application(
                                Rc::new(CoreExpr::Variable("*".into())),
                                Rc::new(CoreExpr::Variable("y".into())),
                            )),
                            Rc::new(CoreExpr::Variable("y".into())),
                        )),
                    ),
                ),
            ],
            body: Rc::new(CoreExpr::Application(
                Rc::new(CoreExpr::Application(
                    Rc::new(CoreExpr::Variable("+".into())),
                    Rc::new(CoreExpr::Variable("x".into())),
                )),
                Rc::new(CoreExpr::Application(
                    Rc::new(CoreExpr::Application(
                        Rc::new(CoreExpr::Variable("+".into())),
                        Rc::new(CoreExpr::Variable("y".into())),
                    )),
                    Rc::new(CoreExpr::Variable("z".into())),
                )),
            )),
        };

        assert_eq!((&[] as &[&str], expected), res);
    }

    #[test]
    fn program_with_let_binding() {
        let program = "main = let
  x = 26;
  y = x * x;
  z = y * y + y * y
in x + y + z";

        // do you think functional programming was a mistake?
        let expected = vec![CoreScDef {
            name: "main".into(),
            bindings: vec![],
            expression: CoreExpr::Let {
                is_recursive: false,
                definitions: vec![
                    ("x".into(), CoreExpr::Number(26)),
                    (
                        "y".into(),
                        CoreExpr::Application(
                            Rc::new(CoreExpr::Application(
                                Rc::new(CoreExpr::Variable("*".into())),
                                Rc::new(CoreExpr::Variable("x".into())),
                            )),
                            Rc::new(CoreExpr::Variable("x".into())),
                        ),
                    ),
                    (
                        "z".into(),
                        CoreExpr::Application(
                            Rc::new(CoreExpr::Application(
                                Rc::new(CoreExpr::Variable("+".into())),
                                Rc::new(CoreExpr::Application(
                                    Rc::new(CoreExpr::Application(
                                        Rc::new(CoreExpr::Variable("*".into())),
                                        Rc::new(CoreExpr::Variable("y".into())),
                                    )),
                                    Rc::new(CoreExpr::Variable("y".into())),
                                )),
                            )),
                            Rc::new(CoreExpr::Application(
                                Rc::new(CoreExpr::Application(
                                    Rc::new(CoreExpr::Variable("*".into())),
                                    Rc::new(CoreExpr::Variable("y".into())),
                                )),
                                Rc::new(CoreExpr::Variable("y".into())),
                            )),
                        ),
                    ),
                ],
                body: Rc::new(CoreExpr::Application(
                    Rc::new(CoreExpr::Application(
                        Rc::new(CoreExpr::Variable("+".into())),
                        Rc::new(CoreExpr::Variable("x".into())),
                    )),
                    Rc::new(CoreExpr::Application(
                        Rc::new(CoreExpr::Application(
                            Rc::new(CoreExpr::Variable("+".into())),
                            Rc::new(CoreExpr::Variable("y".into())),
                        )),
                        Rc::new(CoreExpr::Variable("z".into())),
                    )),
                )),
            },
        }];

        let result = parse(program).unwrap();
        assert_eq!(expected, result);
    }
}
