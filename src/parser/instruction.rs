use super::common::{escaped_newline, Span};
use super::from::FromArgs;
use super::workdir::WorkdirArgs;
use crate::errors::*;
use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{multispace0, space1};
use nom::error::context;
use nom::sequence::preceded;
use nom::sequence::terminated;
use nom::Err as NomErr;
use nom::IResult;
use std::path::PathBuf;

pub type Dockerfile = Vec<Instruction>;

#[non_exhaustive]
#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    FROM {
        platform: Option<String>,
        image: String,
        as_name: Option<String>,
    },
    WORKDIR {
        /// TODO change this to a &str
        path: PathBuf,
    },
}

fn known_instructions(span: Span) -> IResult<Span, &str, DockerParseError> {
    let instructions = alt((tag_no_case("FROM"), tag_no_case("WORKDIR")));
    let (remaining, token) = context(
        &UNSUPPORTED_INSTRUCTION_ERROR,
        terminated(instructions, alt((space1, escaped_newline))),
    )(span)?;
    Ok((remaining, token.fragment()))
}

static UNSUPPORTED_INSTRUCTION_ERROR: &str =
    "Unsupported instruction encountered, expected one of [FROM, WORKDIR]";

/// Parse an instruction.
///
/// ```
/// use nom_locate::LocatedSpan;
/// use dockerfile_parser::{Dockerfile, instruction, Instruction};
///
/// type Span<'a> = LocatedSpan<&'a str>;
///
/// let span = Span::new("FROM alpine");
///
/// let (rest, ins) =
///     instruction(span).unwrap();
///
/// assert!(rest.fragment().is_empty());
/// assert_eq!(ins,
///     Instruction::FROM {
///         platform: None,
///         image: "alpine".to_string(),
///         as_name: None
///     }
/// );
/// ```
///
pub fn instruction<'a, 'b>(span: Span<'a>) -> IResult<Span<'a>, Instruction, DockerParseError<'a>> {
    let (remaining, instruction) = preceded(multispace0, known_instructions)(span)?;
    match instruction.to_uppercase().as_ref() {
        "FROM" => {
            let (remaining, ins) = FromArgs::parse(remaining)?;
            Ok((remaining, ins))
        }
        "WORKDIR" => {
            let (remaining, ins) = WorkdirArgs::parse(remaining)?;
            Ok((remaining, ins))
        }
        _ => Err(NomErr::Failure(DockerParseError::new(
            span.fragment(),
            span.location_line(),
            span.get_column(),
            format!(
                "Unknown instruction '{}' encountered. This is likely a parser bug.",
                instruction
            ),
        ))),
    }
}

#[cfg(test)]
mod test {
    use super::{instruction, Instruction};
    use crate::errors::DockerParseError;
    use nom::Err as NomErr;
    use nom::IResult;
    use nom_locate::LocatedSpan;

    type Span<'a> = LocatedSpan<&'a str>;

    fn expect_nom_error(
        result: IResult<Span, Instruction, DockerParseError>,
        msg: &str,
        line: u32,
        column: usize,
    ) {
        println!("Result {:#?}", result);
        assert!(result.is_err(), "Expected an error!");
        if let NomErr::Error(result) = result.err().unwrap() {
            assert_eq!(result.context.unwrap(), msg);
            assert_eq!(result.line, line, "Unexpected line number!");
            assert_eq!(result.column, column, "Unexpected column!");
        } else {
            panic!("Expected a nom error with a specific message!");
        }
    }

    #[test]
    fn should_error_if_given_as_but_no_name() {
        // arrange
        let from_str = "FROM alpine as ";

        // act
        let result = instruction(Span::new(from_str));

        // assert
        expect_nom_error(result, "Expected a name after `as`", 1, 16);
    }

    #[test]
    fn should_error_if_image_name_is_empty() {
        // arrange
        let from_str = "FROM ";

        // act
        let result = instruction(Span::new(from_str));

        // assert
        expect_nom_error(result, "A FROM instruction must have an image name", 1, 6);
    }

    #[test]
    fn should_error_if_no_platform_is_given() {
        // arrange
        let from_str = "FROM --platform=";

        // act
        let result = instruction(Span::new(from_str));

        // assert
        expect_nom_error(result, "The `--platform` argument cannot be empty", 1, 17);
    }

    #[test]
    fn should_parse_from_instruction_correctly_and_consume_space() {
        // arrange
        let span = Span::new("   FROM      alpine   ");

        // act
        let result = instruction(span);

        // assert
        assert!(result.is_ok());
        let (remaining, _) = result.unwrap();
        assert_eq!(
            remaining.fragment().to_owned(),
            "",
            "Expected no whitespace in remaining fragment, but received whitespace!"
        );
    }

    #[test]
    fn should_error_if_instruction_is_unknown() {
        // arrange
        let span = Span::new("NOT ");

        // act
        if let NomErr::Error(result) = instruction(span).err().unwrap() {
            // assert
            assert_eq!(
                result.context.unwrap(),
                "Unsupported instruction encountered, expected one of [FROM, WORKDIR]"
            );
            assert_eq!(result.line, 1, "Unexpected line!");
            assert_eq!(result.column, 1, "Unexpected column!");
        } else {
            panic!("Expected a nom error with a specific message!");
        }
    }
}
