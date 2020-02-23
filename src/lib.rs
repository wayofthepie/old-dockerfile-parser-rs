mod errors;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_while1;
use nom::character::complete::{line_ending, multispace0, space0, space1};
use nom::multi::many0;
use nom::sequence::{preceded, terminated};
use nom::IResult;

use errors::*;

pub fn instruction_name<'a>(ins: &'a str) -> impl Fn(&'a str) -> IResult<&str, &str> {
    move |input| preceded(space0, terminated(tag(ins), space1))(input)
}

pub fn instruction_argument(input: &str) -> IResult<&str, String, DockerParseError> {
    let handle_backslash = preceded(backslash, not_backslash_or_newline);
    let handle_full_arg = many0(alt((handle_backslash, not_backslash_or_newline)));
    let result = terminated(handle_full_arg, multispace0)(input).unwrap();
    if !result.0.is_empty() {
        let error = DockerParseError::new(
            input,
            format!("The following tokens were not consumed: {}", result.0),
        );
        return Err(nom::Err::Error(error));
    };
    let arg = result.1.join(" ").trim().to_owned();
    Ok((result.0, arg))
}

fn not_backslash_or_newline(input: &str) -> IResult<&str, &str> {
    let chars = |c| !"\\\n\r".contains(c);
    take_while1(chars)(input)
}

fn backslash(input: &str) -> IResult<&str, &str> {
    preceded(
        space0,
        terminated(terminated(tag("\\"), space0), line_ending),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::{backslash, instruction_argument, instruction_name};

    #[test]
    fn should_parse_backslash_consuming_whitespace_and_single_newline() {
        let string = "\\\n";
        let result = backslash(string);
        assert_eq!(result, Ok(("", "\\")));
    }

    #[test]
    fn should_parse_instruction_name_consuming_whitespace() {
        let string = " FROM ";
        let result = instruction_name("FROM")(string);
        assert_eq!(result, Ok(("", "FROM")));
    }

    #[test]
    fn should_fail_parsing_instruction_name_if_name_does_not_match() {
        let string = " RUN ";
        let result = instruction_name("FROM")(string);
        assert!(result.is_err());
    }

    #[test]
    fn should_parse_instruction_argument_consuming_proceeding_whitespace() {
        let string = "./run.sh test \n";
        let result = instruction_argument(string);
        assert_eq!(result, Ok(("", "./run.sh test".to_owned())));
    }

    #[test]
    fn should_parse_instruction_argument_with_no_newline_consuming_proceeding_whitespace() {
        let string = "./run.sh test";
        let result = instruction_argument(string);
        println!("{:#?}", result);
        assert_eq!(result, Ok(("", "./run.sh test".to_owned())));
    }

    #[test]
    fn should_parse_instruction_argument_with_multiple_newlines() {
        let string = "./run.sh test\\\ntest2\\\n  test3  ";
        let result = instruction_argument(string);
        println!("{:#?}", result);
        assert_eq!(result, Ok(("", "./run.sh test test2   test3".to_owned())));
    }

    #[test]
    fn should_fail_to_parse_instruction_argument_if_backslash_has_no_newline() {
        let string = r#"./run.sh test\"#;
        let result = instruction_argument(string);
        assert!(result.is_err());
    }
}
