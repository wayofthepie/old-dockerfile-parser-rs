mod errors;
mod parser;

use errors::*;
use nom::IResult;
use nom_locate::LocatedSpan;

pub use crate::parser::instruction::{instruction, Dockerfile, Instruction};

type Span<'a> = LocatedSpan<&'a str>;

pub fn dockerfile(span: Span) -> IResult<Span, (), DockerParseError> {
    let mut d = Dockerfile::new();
    let (_remaining, _ins) = instruction(span)?;
    Ok((span, ()))
}

