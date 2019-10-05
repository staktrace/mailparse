use std::error;
use std::fmt;
use std::ops::Deref;

/// An error type that represents the different kinds of errors that may be
/// encountered during message parsing.
#[derive(Debug)]
pub enum MailParseError {
    /// Data that was specified as being in the quoted-printable transfer-encoding
    /// could not be successfully decoded as quoted-printable data.
    QuotedPrintableDecodeError(quoted_printable::QuotedPrintableError),
    /// Data that was specified as being in the base64 transfer-encoding could
    /// not be successfully decoded as base64 data.
    Base64DecodeError(base64::DecodeError),
    /// An error occurred when converting the raw byte data to Rust UTF-8 string
    /// format using the charset specified in the message.
    EncodingError(std::borrow::Cow<'static, str>),
    /// Some other error occurred while parsing the message; the description string
    /// provides additional details.
    Generic(&'static str),
}

impl fmt::Display for MailParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MailParseError::QuotedPrintableDecodeError(ref err) => {
                write!(f, "QuotedPrintable decode error: {}", err)
            }
            MailParseError::Base64DecodeError(ref err) => write!(f, "Base64 decode error: {}", err),
            MailParseError::EncodingError(ref err) => write!(f, "Encoding error: {}", err),
            MailParseError::Generic(ref description) => write!(f, "{}", description),
        }
    }
}

impl error::Error for MailParseError {
    fn description(&self) -> &str {
        match *self {
            MailParseError::QuotedPrintableDecodeError(ref err) => err.description(),
            MailParseError::Base64DecodeError(ref err) => err.description(),
            MailParseError::EncodingError(ref err) => err.deref(),
            _ => "An error occurred while attempting to parse the input",
        }
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        match *self {
            MailParseError::QuotedPrintableDecodeError(ref err) => Some(err),
            MailParseError::Base64DecodeError(ref err) => Some(err),
            _ => None,
        }
    }
}

impl From<quoted_printable::QuotedPrintableError> for MailParseError {
    fn from(err: quoted_printable::QuotedPrintableError) -> MailParseError {
        MailParseError::QuotedPrintableDecodeError(err)
    }
}

impl From<base64::DecodeError> for MailParseError {
    fn from(err: base64::DecodeError) -> MailParseError {
        MailParseError::Base64DecodeError(err)
    }
}

impl From<std::borrow::Cow<'static, str>> for MailParseError {
    fn from(err: std::borrow::Cow<'static, str>) -> MailParseError {
        MailParseError::EncodingError(err)
    }
}

