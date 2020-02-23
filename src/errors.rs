use nom::error;
use nom::error::{ParseError, VerboseError, VerboseErrorKind};
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

impl<'a> ParseError<&'a str> for DockerParseError<'a> {
    fn from_error_kind(input: &'a str, kind: error::ErrorKind) -> Self {
        Self {
            nom_error: VerboseError::from_error_kind(input, kind),
            message: None,
        }
    }

    fn append(input: &'a str, kind: error::ErrorKind, mut other: Self) -> Self {
        other
            .nom_error
            .errors
            .push((input, VerboseErrorKind::Nom(kind)));
        other
    }
}

impl<'a> Into<VerboseError<&'a str>> for DockerParseError<'a> {
    fn into(self) -> VerboseError<&'a str> {
        self.nom_error
    }
}
