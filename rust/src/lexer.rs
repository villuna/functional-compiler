use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{digit1, one_of, satisfy},
    combinator::recognize,
    multi::{many0, many0_count},
    sequence::pair,
    Finish, IResult, Parser,
};

pub const BIN_OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "<", "<=", "==", "/=", ">=", ">", "&", "|",
];

pub type Token<'a> = &'a str;

fn tokenize<'a, O, P: Parser<&'a str, O, nom::error::Error<&'a str>>>(
    mut parser: P,
) -> impl FnMut(&'a str) -> IResult<&'a str, O> {
    move |input| {
        let (input, _) = many0_count(one_of(" \t\r\n"))(input)?;
        let (input, res) = parser.parse(input)?;
        let (input, _) = many0_count(one_of(" \t\r\n"))(input)?;
        Ok((input, res))
    }
}

fn var(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        satisfy(|c| c.is_alphabetic()),
        many0(satisfy(|c| c.is_alphanumeric() || c == '_')),
    ))(input)
}

fn operator(input: &str) -> IResult<&str, &str> {
    for op in BIN_OPERATORS {
        let res = tag(*op)(input);
        if res.is_ok() {
            return res;
        }
    }

    Err(nom::Err::Error(nom::error::Error {
        input,
        code: nom::error::ErrorKind::Tag,
    }))
}

fn lex_helper(input: &str) -> IResult<&str, Vec<&str>> {
    many0(tokenize(alt((digit1, var, operator, take(1usize)))))(input)
}

pub fn lex(input: &str) -> Option<Vec<Token>> {
    if input.is_empty() {
        return Some(vec![]);
    }

    lex_helper(input).finish().map(|(_, v)| v).ok()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_lex() {
        let input = "fun f x y = f (x + y)";
        let res = lex(input).unwrap();

        assert_eq!(
            res,
            vec!["fun", "f", "x", "y", "=", "f", "(", "x", "+", "y", ")",]
        );
    }
}
