use super::{
    common::{arg_token, tag_maybe_with_internal_newlines, Span},
    instruction::Instruction,
};
use crate::errors::DockerParseError;
use nom::bytes::complete::tag;
use nom::character::complete::space0;
use nom::combinator::opt;
use nom::error::context;
use nom::sequence::tuple;
use nom::Err as NomErr;
use nom::IResult;

const FROM_MISSING_IMAGE_ERROR: &str = "A FROM instruction must have an image name";
const FROM_AS_MISSING_NAME_ERROR: &str = "Expected a name after `as`";
const FROM_PLATFORM_ARG_TAG: &str = "--platform=";
const FROM_AS_TAG: &str = "as";
const FROM_PLATFORM_CANNOT_BE_EMPTY_ERROR: &str = "The `--platform` argument cannot be empty";

#[derive(Debug)]
pub struct FromArgs;

impl FromArgs {
    /// Does not handle the following.
    ///
    /// **Empty platform setting**
    ///
    /// FROM --platform= alpine
    pub(crate) fn parse<'a>(
        span: Span<'a>,
    ) -> IResult<Span<'a>, Instruction, DockerParseError<'a>> {
        let (remaining, (platform, image, as_name)) = tuple((
            FromArgs::parse_platform,
            FromArgs::parse_image_name,
            FromArgs::parse_as_name,
        ))(span)?;
        Ok((
            remaining,
            Instruction::FROM {
                platform,
                image,
                as_name,
            },
        ))
    }

    fn parse_platform(span: Span) -> IResult<Span, Option<String>, DockerParseError> {
        if let (remaining, Some(_)) = opt(tag(FROM_PLATFORM_ARG_TAG))(span)? {
            let (remaining, platform) =
                context(FROM_PLATFORM_CANNOT_BE_EMPTY_ERROR, arg_token)(remaining)?;
            let (remaining, _) = space0(remaining)?;
            Ok((remaining, Some(platform)))
        } else {
            Ok((span, None))
        }
    }

    fn parse_image_name<'a>(span: Span<'a>) -> IResult<Span<'a>, String, DockerParseError<'a>> {
        let (remaining, token) = context(FROM_MISSING_IMAGE_ERROR, arg_token)(span)?;
        if token.is_empty() {
            // TODO: This should be a Failure, it is un-recoverable
            return Err(NomErr::Error(DockerParseError::new(
                span.fragment(),
                span.location_line(),
                span.get_column(),
                FROM_MISSING_IMAGE_ERROR,
            )));
        }
        Ok((remaining, token))
    }

    fn parse_as_name(span: Span) -> IResult<Span, Option<String>, DockerParseError> {
        let as_parser = tag_maybe_with_internal_newlines(FROM_AS_TAG);
        if let (remaining, Some(_)) = opt(as_parser)(span)? {
            let (remaining, as_name) = context(FROM_AS_MISSING_NAME_ERROR, arg_token)(remaining)?;
            Ok((remaining, Some(as_name)))
        } else {
            Ok((span, None))
        }
    }
}

#[cfg(test)]
mod test {
    use super::FromArgs;
    use crate::Instruction;
    use nom_locate::LocatedSpan;
    use quickcheck::{quickcheck, Arbitrary, Gen};
    use rand::Rng;

    type Span<'a> = LocatedSpan<&'a str>;

    #[derive(Clone, Debug)]
    struct GenFrom {
        to_parse: String,
        parsed: Instruction,
    }

    #[derive(Clone, Debug)]
    struct GenPlatform(&'static str, &'static str);

    #[derive(Clone, Debug)]
    struct GenImage(String);

    impl Arbitrary for GenPlatform {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let index = g.gen_range(0, 4);
            let platforms = vec![
                ("linux/amd64 ", "linux/amd64"),
                ("linux/arm64 ", "linux/arm64"),
                ("windows/amd64   ", "windows/amd64"),
                ("\\\nwindows/amd64 ", "windows/amd64"),
            ];
            let (given, expected) = platforms[index];
            Self(given, expected)
        }
    }

    impl Arbitrary for GenImage {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let mut image = String::new();
            while image.is_empty() {
                image = String::arbitrary(g)
                    .replace("\n", "\\\n")
                    .replace("\t", "")
                    .replace("\r", "")
                    .replace(" ", "");
                image.push_str("image"); // makes sure we dont end on an invalid char
            }
            Self(image)
        }
    }

    impl Arbitrary for GenFrom {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let mut platform = None;
            let mut as_name = None;
            let mut arg_string_to_parse = String::new();

            // platform
            if let Some(gen_platform) = Option::<GenPlatform>::arbitrary(g) {
                let platform_str = format!("--platform={} ", &gen_platform.0);
                arg_string_to_parse.push_str(&platform_str);
                platform = Some(gen_platform.1.to_string());
            }

            // image name
            let GenImage(image) = GenImage::arbitrary(g);
            arg_string_to_parse.push_str(&image);

            // as name
            let index = g.gen_range(0, 2);
            if index == 1 {
                arg_string_to_parse.push_str(" as something");
                as_name = Some("something".to_string());
            }
            Self {
                to_parse: arg_string_to_parse,
                parsed: Instruction::FROM {
                    platform,
                    // What we expect to be parsed needs to strip
                    // characters we will discard during parsing
                    image: image.replace("\\\n", "").replace("\\", ""),
                    as_name,
                },
            }
        }
    }

    #[test]
    fn prop_parse_from_args_correctly() {
        fn prop(from_arg: GenFrom) -> bool {
            // act
            let result = FromArgs::parse(Span::new(&from_arg.to_parse));

            // act / assert
            if let Err(_) = result {
                println!("Received an error but expected a successful parse.");
                println!("To parse: {:#?}", from_arg.to_parse);
                println!("Expected: {:#?}", from_arg.parsed);
                println!("Got: {:#?}", result.err());
                return false;
            }
            let (remaining, from) = result.unwrap();
            if !remaining.fragment().is_empty() {
                println!("Expected nothing to be leftover when parse is successful, but there was data remaining.");
                println!("To parse: {:#?}", from_arg.to_parse);
                println!("Expected: {:#?}", from_arg.parsed);
                println!("Got: {:#?}", from);
                println!("Remaining data: {:#?}", remaining);
                return false;
            };
            if !(from == from_arg.parsed) {
                println!("The expected parsed value and the actual parsed value are not the same!");
                println!("To parse: {:#?}", from_arg.to_parse);
                println!("Expected: {:#?}", from_arg.parsed);
                println!("Got: {:#?}", from);
                return false;
            }
            true
        }
        quickcheck(prop as fn(GenFrom) -> bool);
    }

    #[test]
    fn should_handle_escaped_newlines_in_image_name() {
        // arrange
        let span = Span::new("alp\\\nine");
        let expected = Instruction::FROM {
            platform: None,
            image: "alpine".to_string(),
            as_name: None,
        };

        // act
        let result = FromArgs::parse(span);

        // assert
        assert!(result.is_ok());
        let (remaining, instruction) = result.unwrap();
        assert!(remaining.fragment().is_empty());
        assert_eq!(instruction, expected);
    }

    #[test]
    fn should_consume_backslashes_that_do_not_escape_space_or_newline() {
        // arrange
        let span = Span::new("alp\\\\i\\ne");
        let expected = Instruction::FROM {
            platform: None,
            image: "alpine".to_string(),
            as_name: None,
        };

        // act
        let result = FromArgs::parse(span);

        // assert
        assert!(result.is_ok());
        let (remaining, instruction) = result.unwrap();
        assert!(remaining.fragment().is_empty());
        assert_eq!(instruction, expected);
    }

    #[test]
    fn should_allow_arguments_to_be_separated_by_escaped_newlines() {
        // arrange
        let span = Span::new("--platform=linux\\ \n\\\n alp\\\\i\\ne\\\n as \\\n test ");
        let expected = Instruction::FROM {
            platform: Some("linux".to_string()),
            image: "alpine".to_string(),
            as_name: Some("test".to_string()),
        };

        // act
        let result = FromArgs::parse(span);

        // assert
        println!("{:#?}", result);
        assert!(result.is_ok());
        let (remaining, instruction) = result.unwrap();
        assert!(remaining.fragment().is_empty());
        assert_eq!(instruction, expected);
    }
}
