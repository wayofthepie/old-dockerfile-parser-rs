use nom::error;
use nom::error::{ParseError, VerboseError, VerboseErrorKind};
use nom_locate::LocatedSpan;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Debug)]
pub struct DockerParseError<'a> {
    pub nom_error: VerboseError<&'a str>,
    pub message: Option<Cow<'static, str>>,
}

impl std::error::Error for DockerParseError<'_> {}

impl Display for DockerParseError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.message {
            None => write!(f, "ParseError: {:?}", self.message),
            Some(_m) => Ok(()),
        }
    }
}

impl<'a> DockerParseError<'a> {
    pub fn new<M: Into<Cow<'static, str>>>(input: &'a str, message: M) -> Self {
        Self {
            nom_error: VerboseError::from_error_kind(input, nom::error::ErrorKind::Verify),
            message: Some(message.into()),
        }
    }

    pub fn with_message<M: Into<Cow<'static, str>>>(mut self, msg: M) -> Self {
        self.message = Some(msg.into());
        self
    }
}

impl<'a> ParseError<LocatedSpan<&'a str>> for DockerParseError<'a> {
    fn from_error_kind(input: LocatedSpan<&'a str>, kind: error::ErrorKind) -> Self {
        Self {
            nom_error: VerboseError::from_error_kind(input.fragment(), kind),
            message: None,
        }
    }

    fn append(input: LocatedSpan<&'a str>, kind: error::ErrorKind, mut other: Self) -> Self {
        other
            .nom_error
            .errors
            .push((input.fragment(), VerboseErrorKind::Nom(kind)));
        other
    }

    fn add_context(input: LocatedSpan<&'a str>, ctx: &'static str, _other: Self) -> Self {
        let context_and_location = format!(
            "Error occurred on line {} column {}: {}",
            input.location_line(),
            input.get_column(),
            ctx
        );
        Self::new(input.fragment(), context_and_location)
    }
}
