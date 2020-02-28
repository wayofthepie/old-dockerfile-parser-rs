use super::{
    common::{
        arg_token, arg_token_parser, is_not_space, optional_escaped_newline,
        tag_maybe_with_newline, Span,
    },
    instruction::Instruction,
};
use crate::errors::DockerParseError;
use nom::branch::alt;
use nom::bytes::complete::escaped;
use nom::bytes::complete::tag;
use nom::character::complete::{one_of, space0, space1};
use nom::combinator::opt;
use nom::error::context;
use nom::sequence::{terminated, tuple};
use nom::Err as NomErr;
use nom::IResult;
use std::ops::Deref;

const FROM_MISSING_IMAGE_ERROR: &str = "A FROM instruction must have an image name";
const FROM_AS_MISSING_NAME_ERROR: &str = "Expected a name after `as`";
const FROM_PLATFORM_ARG_TAG: &str = "--platform=";
const FROM_AS_TAG: &str = "as";
const FROM_PLATFORM_CANNOT_BE_EMPTY_ERROR: &str = "The `--platform` argument cannot be empty";
const FROM_PLATFORM_SPACE_ERROR: &str =
    "The `--platform` argument and its value must be proceeded by a space";

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
        println!("INITIAL {:#?}", span);
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
        let as_parser = tag_maybe_with_newline("as");
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
    use super::DockerParseError;
    use super::FromArgs;
    use crate::Instruction;
    use nom::bytes::complete::{escaped, escaped_transform};
    use nom::character::complete::one_of;
    use nom::Err as NomErr;
    use nom::IResult;
    use nom_locate::LocatedSpan;
    use quickcheck::{quickcheck, Arbitrary, Gen, QuickCheck, TestResult};
    use rand::Rng;

    type Span<'a> = LocatedSpan<&'a str>;

    #[derive(Clone, Debug)]
    struct GenFrom {
        to_parse: String,
        parsed: Instruction,
    }

    #[derive(Clone, Debug)]
    struct GenPlatform(&'static str, &'static str);

    impl Arbitrary for GenPlatform {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let index = g.gen_range(0, 4);
            let platforms = vec![
                ("linux/amd64 ", "linux/amd64"),
                ("linux/arm64 ", "linux/arm64"),
                ("windows/amd64   ", "windows/amd64"),
                (
                    r#"\
windows/amd64 "#,
                    "windows/amd64",
                ),
            ];
            let (given, expected) = platforms[index];
            Self(given, expected)
        }
    }

    impl Arbitrary for GenFrom {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let image;
            let mut platform = None;
            let mut as_name = None;
            let mut from_arg = String::new();
            let maybe_platform = Option::<GenPlatform>::arbitrary(g);
            if let Some(gen_platform) = maybe_platform {
                let platform_str = format!("--platform={} ", &gen_platform.0);
                from_arg.push_str(&platform_str);
                platform = Some(gen_platform.1.to_string());
            }
            let index = g.gen_range(0, 4);
            let images = vec![
                "alpine",
                "alpine:3.11.0",
                "test.com/docker/alpine:3.11.0",
                "rust@sha256:af6b555730fa71a4faa1844fb98e7201d266732f39a0e5f317179c133ba94e16",
            ];
            from_arg.push_str(images[index]);
            image = images[index].to_string();
            let index = g.gen_range(0, 2);
            if index == 1 {
                from_arg.push_str(" as something");
                as_name = Some("something".to_string());
            }
            Self {
                to_parse: from_arg,
                parsed: Instruction::FROM {
                    platform,
                    image,
                    as_name,
                },
            }
        }
    }

    #[test]
    fn should_parse_from_args_correctly() {
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
}
