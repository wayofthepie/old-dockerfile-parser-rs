use super::common::Span;
use super::instruction::Instruction;
use crate::errors::*;
use nom::character::complete::{multispace0, not_line_ending, space0};
use nom::sequence::{preceded, terminated};
use nom::IResult;
use std::path::PathBuf;

#[derive(Debug)]
pub struct WorkdirArgs;

impl WorkdirArgs {
    /// Parse the argument to WORKDIR. This is just parsed as a string, no
    /// path validation is done. Does not support having \n in a path name.
    pub fn parse(span: Span) -> IResult<Span, Instruction, DockerParseError> {
        let (remaining, arg) = terminated(preceded(space0, not_line_ending), multispace0)(span)?;
        Ok((
            remaining,
            Instruction::WORKDIR {
                path: PathBuf::from(arg.fragment().trim()),
            },
        ))
    }
}

#[cfg(test)]
mod test {
    use super::WorkdirArgs;
    use crate::parser::common::Span;
    use crate::parser::instruction::Instruction;
    use quickcheck::{Arbitrary, Gen};
    use quickcheck_macros::quickcheck;

    #[derive(Clone, Debug)]
    struct GenUtf8Path(String);

    impl Arbitrary for GenUtf8Path {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            Self(String::arbitrary(g).replace('\n', "").replace('\r', ""))
        }
    }

    #[quickcheck]
    fn should_parse_args_correctly(path: GenUtf8Path) -> bool {
        // act
        let expected = path.0;
        let result = WorkdirArgs::parse(Span::new(&expected));

        // assert
        let ok = result.is_ok();
        if let (remaining, Instruction::WORKDIR { path: parsed_path }) = result.unwrap() {
            let parsed = parsed_path.to_str().unwrap();
            ok && remaining.fragment().is_empty() && parsed == expected.trim()
        } else {
            false
        }
    }
}
