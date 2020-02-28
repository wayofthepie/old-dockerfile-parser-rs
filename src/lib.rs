mod errors;
use errors::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until;
use nom::character::complete::{line_ending, multispace0, space0, space1};
use nom::error::context;
use nom::multi::fold_many0;
use nom::sequence::{preceded, terminated};
use nom::Err as NomErr;
use nom::IResult;
use nom_locate::LocatedSpan;

static NO_SPACE_AFTER_INSTRUCTION_ERROR: &str =
    "No whitespace or escaped newline found after instruction!";

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
///
pub fn instruction_name(span: Span) -> IResult<Span, &str, DockerParseError> {
    let escaped_newline = terminated(preceded(space0, tag("\\\n")), space0);
    let proceeding_space = context(
        NO_SPACE_AFTER_INSTRUCTION_ERROR,
        alt((space1, escaped_newline)),
    );
    terminated(
        preceded(multispace0, known_instructions()),
        proceeding_space,
    )(span)
}

/// Parse an instruction argument as a string.
pub fn instruction_arg(span: Span) -> IResult<Span, String, DockerParseError> {
    let arg_line = terminated(
        terminated(take_until("\\"), tag("\\")),
        space_to_newline_parser(
            "Expected only spaces until a newline after a backslach character!",
        ),
    );
    let (remaining, mut arg) = fold_many0(arg_line, String::new(), |mut acc: String, s: Span| {
        acc.push_str(s.fragment());
        acc
    })(span)?;
    let (remaining, token) = terminated(take_until("\n"), tag("\n"))(remaining)?;
    arg.push_str(token.fragment());
    Ok((remaining, arg))
}

/// Parses any space to a newline, will fail if it encounted any character
/// other than a space before the newline.
fn space_to_newline_parser<'a, 'b>(
    err_ctx: &'static str,
) -> impl Fn(Span<'a>) -> IResult<Span<'a>, &'a str, DockerParseError<'a>> {
    move |span| {
        let result: IResult<Span, Span, DockerParseError> = terminated(space0, line_ending)(span);
        match result {
            Ok((remaining, token)) => Ok((remaining, token.fragment().to_owned())),
            Err(_) => Err(NomErr::Failure(DockerParseError::new(
                span.fragment(),
                span.location_line(),
                span.get_column(),
                err_ctx,
            ))),
        }
    }
}

#[cfg(test)]
mod instruction_arg_tests {
    use super::instruction_arg;
    use nom::Err as NomErr;
    use nom_locate::LocatedSpan;

    type Span<'a> = LocatedSpan<&'a str>;

    #[test]
    fn should_leave_next_instruction_unconsumed_after_parsing_arg() {
        // arrange
        let string = format!("mkdir -p /opt/test \\\n&& cd /opt/test\nRUN another",);
        let span = Span::new(&string);

        // act
        let result = instruction_arg(span);

        // assert
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert_eq!(tuple.0.fragment().to_owned(), "RUN another");
    }

    #[test]
    fn should_error_if_any_character_except_space_is_between_a_backslash_and_newline() {
        // arrange
        let string = format!("mkdir -p /opt/test \\bad\n&& cd /opt/test\n",);
        let span = Span::new(&string);

        // act
        let result = instruction_arg(span);

        // assert
        assert!(result.is_err());
        if let NomErr::Failure(error) = instruction_arg(span).err().unwrap() {
            // assert
            assert_eq!(
                error.context.unwrap(),
                "Expected only spaces until a newline after a backslach character!"
            );
            assert_eq!(error.line, 1);
            assert_eq!(error.column, 21);
        } else {
            panic!("Expected a specific error, did not receive it!");
        }
    }

    #[test]
    fn should_parse_until_newline_with_no_escape_returning_items_after_newline_as_unconsumed() {
        let string = format!("alpine\nRUN blah",);
        let span = Span::new(&string);

        // act
        let result = instruction_arg(span);

        // assert
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert_eq!(tuple.0.fragment().to_owned(), "RUN blah");
        assert_eq!(tuple.1, "alpine");
    }

    /// The main example of this is the RUN command. If you are using RUN to run
    /// many shell commands directly in the Dockerfile you generally split it over
    /// multiple lines with "\" and "&&". E.g.
    ///
    /// RUN mkdir -p /opt/test \
    ///     && cd /opt/test
    #[test]
    fn should_parse_argument_with_escaped_newline_separators_as_a_single_argument() {
        // arrange
        let string = format!("mkdir -p /opt/test \\\n&& cd /opt/test\n",);
        let span = Span::new(&string);

        // act
        let result = instruction_arg(span);

        // assert
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert!(tuple.0.fragment().is_empty());
        assert_eq!(tuple.1, "mkdir -p /opt/test && cd /opt/test");
    }

    #[test]
    fn should_parse_until_newline_consuming_newline() {
        // arrange
        let string = format!("alpine\n",);
        let span = Span::new(&string);

        // act
        let result = instruction_arg(span);

        // assert
        println!("{:#?}", result);
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert!(tuple.0.fragment().is_empty());
        assert_eq!(tuple.1, "alpine");
    }
}

#[cfg(test)]
mod instruction_name_tests {
    use super::{instruction_name, Instruction};
    use nom::Err as NomErr;
    use nom_locate::LocatedSpan;

    type Span<'a> = LocatedSpan<&'a str>;

    #[test]
    fn should_fail_to_parse_known_instruction_if_proceeded_by_unescaped_newline() {
        // arrange
        let string = format!("{}\n", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        if let NomErr::Error(error) = instruction_name(span).err().unwrap() {
            // assert
            assert_eq!(
                error.context.unwrap(),
                "No whitespace or escaped newline found after instruction!"
            );
            assert_eq!(error.line, 1);
            assert_eq!(error.column, 5);
        } else {
            panic!("Expected a specific error, did not receive it!");
        }
    }

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
    fn should_consume_whitespace_after_escaped_newline() {
        // arrange
        let string = format!("{}\\\n  ", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert!(tuple.0.fragment().is_empty());
        assert_eq!(tuple.1, Instruction::FROM.as_str());
    }

    #[test]
    fn should_parse_known_instruction_if_proceeded_by_escaped_newline() {
        // arrange
        let string = format!("{}\\\n", Instruction::FROM.as_str());
        let span = Span::new(&string);

        // act
        let result = instruction_name(span);

        // assert
        assert!(result.is_ok());
        let tuple = result.unwrap();
        assert!(tuple.0.fragment().is_empty());
        assert_eq!(tuple.1, Instruction::FROM.as_str());
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
                "No whitespace or escaped newline found after instruction!"
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
