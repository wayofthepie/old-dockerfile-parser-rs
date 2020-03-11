use nom::bytes::complete::escaped_transform;
use nom::bytes::complete::is_not;
use nom::bytes::complete::tag;
use nom::character::complete::{line_ending, space0};
use nom::combinator::opt;
use nom::sequence::{preceded, terminated};
use nom::Err as NomErr;
use nom::IResult;
use nom_locate::LocatedSpan;

use crate::errors::DockerParseError;

pub type Span<'a> = LocatedSpan<&'a str>;

pub fn arg_token(span: Span) -> IResult<Span, String, DockerParseError> {
    let (remaining, token) = terminated(
        escaped_transform(is_not(" \\\t\r\n"), '\\', |span: Span| {
            let (r, _) = opt(tag("\n"))(span)?;
            Ok((r, ""))
        }),
        space0,
    )(span)?;
    if token.is_empty() {
        return Err(NomErr::Error(DockerParseError::new(
            span.fragment(),
            span.location_line(),
            span.get_column(),
            "Expected an argument token, but got nothing",
        )));
    }
    Ok((remaining, token))
}

/// Docker permits escaped newlines in quite a lot of places.
/// For example this is a valid dockerfile
///
/// ```docker
/// FROM alpine a\
/// s test
/// ```
///
/// This function will parse the "as" above as "as", removing
/// the escaped newline. This function is case insensitive.
///
pub fn tag_maybe_with_internal_newlines(
    tag_str: &'static str,
) -> impl Fn(Span) -> IResult<Span, (), DockerParseError> {
    move |span| {
        let (remaining, token) = escaped_transform(
            nom::bytes::complete::is_not(" \\\t\r\n"),
            '\\',
            |span: Span| {
                let (r, _) = opt(tag("\n"))(span)?;
                Ok((r, ""))
            },
        )(span)?;
        println!("AFTER ESC {:#?}", remaining);
        if token.to_lowercase() == tag_str.to_lowercase() {
            let (remaining, _) = space0(remaining)?;
            Ok((remaining, ()))
        } else {
            Err(NomErr::Error(DockerParseError::new(
                span.fragment(),
                span.location_line(),
                span.get_column(),
                "Expected a matching tag",
            )))
        }
    }
}

pub fn escaped_newline(span: Span) -> IResult<Span, Span, DockerParseError> {
    let backslash_and_space = terminated(tag("\\"), space0);
    let backslash_and_space = preceded(space0, backslash_and_space);
    let (r, s) = terminated(backslash_and_space, line_ending)(span)?;
    let (r, _) = space0(r)?;
    Ok((r, s))
}
