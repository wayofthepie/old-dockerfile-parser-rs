mod errors;
use errors::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1};
use nom::error::context;
use nom::sequence::{preceded, terminated};
use nom::IResult;
use nom_locate::LocatedSpan;

static NO_SPACE_AFTER_INSTRUCTION_ERROR: &str = "No whitespace found after instruction!";

type Span<'a> = LocatedSpan<&'a str>;

/// This macro allows us to generate all the possible supported instruction
/// types and a useful error message in case we encounter an unknown type when
/// parsing.
macro_rules! instructions_enum {
    ( $first:ident, $( $name:ident ),* ) => {
       #[non_exhaustive]
        pub enum Instruction {
            $first,
            $( $name, )*
        }

        impl Instruction {
            pub fn from_str(string: &str) -> Option<Self> {
                match string {
                    stringify!($first) => Some(Self::$first),
                    $( stringify!($name) => Some(Self::$name) )*,
                    _ => None,
                }
            }

            pub fn as_str(&self) -> &'static str {
                match self {
                    Instruction::$first => stringify!($first),
                    $( Instruction::$name => stringify!($name) )*,
                }
            }
        }

        static UNSUPPORTED_INSTRUCTION_ERROR: &str =
            stringify!(Unsupported instruction encountered, expected one of [$first $(, $name )*]);
    }
}

instructions_enum!(FROM, RUN);

pub fn instruction_name<'a>(span: Span) -> IResult<Span, &str, DockerParseError> {
    let proceeding_space = context(NO_SPACE_AFTER_INSTRUCTION_ERROR, space1);
    terminated(preceded(space0, known_instructions()), proceeding_space)(span)
}

fn known_instructions() -> impl Fn(Span) -> IResult<Span, &str, DockerParseError> {
    fn parser(span: Span) -> IResult<Span, &str, DockerParseError> {
        let (next_span, token) = context(
            &UNSUPPORTED_INSTRUCTION_ERROR,
            alt((
                tag(Instruction::FROM.as_str()),
                tag(Instruction::RUN.as_str()),
            )),
        )(span)?;
        Ok((next_span, token.fragment()))
    }
    move |span| parser(span)
}

#[cfg(test)]
mod tests {
    use super::{instruction_name, Instruction};
    use nom::Err as NomErr;
    use nom_locate::LocatedSpan;

    type Span<'a> = LocatedSpan<&'a str>;

    #[test]
    fn should_fail_to_parse_known_instruction_if_not_proceeded_by_whitespace() {
        // arrange
        let string = format!("{}", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        if let NomErr::Error(error) = instruction_name(span).err().unwrap() {
            // assert
            assert_eq!(
                error.message.unwrap(),
                "Error occurred on line 1 column 5: No whitespace found after instruction!",
            );
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
    fn should_parse_known_instruction_correctly_if_preceeded_by_space() {
        // arrange
        let string = format!("   {} ", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().1, Instruction::FROM.as_str());
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
            let expected = format!(
                "{} {} {}",
                "Error occurred on line 1 column 1:",
                "Unsupported instruction encountered,",
                "expected one of [FROM, RUN]"
            );
            assert_eq!(result.message.unwrap(), expected);
        } else {
            panic!("Expected a nom error with a specific message!");
        }
    }
}
