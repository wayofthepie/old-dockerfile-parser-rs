mod errors;
use errors::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{multispace0, space1};
use nom::error::context;
use nom::sequence::{preceded, terminated};
use nom::IResult;
use nom_locate::LocatedSpan;

static NO_SPACE_AFTER_INSTRUCTION_ERROR: &str = "No whitespace found after instruction!";

type Span<'a> = LocatedSpan<&'a str>;

/// This macro allows us to generate all the possible supported instruction
/// types and a useful error message in case we encounter an unknown type when
/// parsing.
macro_rules! generate_instructions {
    ( $first:ident, $( $name:ident ),* ) => {
       #[non_exhaustive]
        pub enum Instruction {
            $first,
            $( $name, )*
        }

        impl Instruction {
            pub fn as_str(&self) -> &'static str {
                match self {
                    Instruction::$first => stringify!($first),
                    $( Instruction::$name => stringify!($name) )*,
                }
            }
        }

        fn known_instructions() -> impl Fn(Span) -> IResult<Span, &str, DockerParseError> {
            fn parser(span: Span) -> IResult<Span, &str, DockerParseError> {
                let (next_span, token) = context(
                    &UNSUPPORTED_INSTRUCTION_ERROR,
                    alt((
                        tag(Instruction::$first.as_str()),
                        $( tag(Instruction::$name.as_str()), )*
                    )),
                )(span)?;
                Ok((next_span, token.fragment()))
            }
            move |span| parser(span)
        }

        static UNSUPPORTED_INSTRUCTION_ERROR: &str =
            stringify!(Unsupported instruction encountered, expected one of [$first $(, $name )*]);
    }
}

generate_instructions!(FROM, RUN);

/// Parse an instruction name.
///
/// ```
/// use nom_locate::LocatedSpan;
/// use dockerfile_parser::instruction_name;
///
/// type Span<'a> = LocatedSpan<&'a str>;
///
/// let dockerfile = "FROM alpine";
///
/// let (rest, token): (Span, &str) = instruction_name(Span::new(dockerfile)).unwrap();
///
/// assert_eq!(token, "FROM");
/// assert_eq!(rest.fragment().to_owned(), "alpine");
/// ```
pub fn instruction_name(span: Span) -> IResult<Span, &str, DockerParseError> {
    let proceeding_space = context(NO_SPACE_AFTER_INSTRUCTION_ERROR, space1);
    terminated(
        preceded(multispace0, known_instructions()),
        proceeding_space,
    )(span)
}

#[cfg(test)]
mod instruction_name_tests {
    use super::{instruction_name, Instruction};
    use nom::Err as NomErr;
    use nom_locate::LocatedSpan;

    type Span<'a> = LocatedSpan<&'a str>;

    #[test]
    fn should_parse_known_instruction_correctly_if_preceeded_by_any_type_of_space() {
        // arrange
        let string = format!("\n\r\n\t {} ", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().1, Instruction::FROM.as_str());
    }

    #[test]
    fn should_fail_to_parse_known_instruction_if_not_proceeded_by_whitespace() {
        // arrange
        let string = format!("{}", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        if let NomErr::Error(error) = instruction_name(span).err().unwrap() {
            // assert
            assert_eq!(
                error.context.unwrap(),
                "No whitespace found after instruction!"
            );
            assert_eq!(error.line, 1);
            assert_eq!(error.column, 5);
        } else {
            panic!("Expected a specific error, did not receive it!");
        }
    }

    #[test]
    fn should_parse_known_instruction_correctly_and_consume_proceeding_space() {
        // arrange
        let string = format!("   {} ", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        let x = result.unwrap();
        assert_eq!(x.1, Instruction::FROM.as_str());
        assert_eq!(
            x.0.fragment().to_owned(),
            "",
            "Expected no whitespace in remaining fragment, but received whitespace!"
        );
    }

    #[test]
    fn should_parse_known_instruction_correctly() {
        // arrange
        let string = format!("{} ", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().1, Instruction::FROM.as_str());
    }

    #[test]
    fn should_error_if_instruction_is_unknown() {
        // arrange
        let string = Span::new("NOT ");

        // act
        if let NomErr::Error(result) = instruction_name(string).err().unwrap() {
            // assert
            assert_eq!(
                result.context.unwrap(),
                "Unsupported instruction encountered, expected one of [FROM, RUN]"
            );
            assert_eq!(result.line, 1, "Unexpected line!");
            assert_eq!(result.column, 1, "Unexpected column!");
        } else {
            panic!("Expected a nom error with a specific message!");
        }
    }
}
