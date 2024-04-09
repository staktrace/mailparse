use charset::{decode_ascii, Charset};

use crate::{MailParseError, ParsedContentType};

/// Represents the body of an email (or mail subpart)
pub enum Body<'a> {
    /// A body with 'base64' Content-Transfer-Encoding.
    Base64(EncodedBody<'a>),
    /// A body with 'quoted-printable' Content-Transfer-Encoding.
    QuotedPrintable(EncodedBody<'a>),
    /// A body with '7bit' Content-Transfer-Encoding.
    SevenBit(TextBody<'a>),
    /// A body with '8bit' Content-Transfer-Encoding.
    EightBit(TextBody<'a>),
    /// A body with 'binary' Content-Transfer-Encoding.
    Binary(BinaryBody<'a>),
}

impl<'a> Body<'a> {
    pub fn new(
        body: &'a [u8],
        ctype: &'a ParsedContentType,
        transfer_encoding: &Option<String>,
    ) -> Body<'a> {
        transfer_encoding
            .as_ref()
            .map(|encoding| match encoding.as_ref() {
                "base64" => Body::Base64(EncodedBody {
                    decoder: decode_base64,
                    body,
                    ctype,
                }),
                "quoted-printable" => Body::QuotedPrintable(EncodedBody {
                    decoder: decode_quoted_printable,
                    body,
                    ctype,
                }),
                "7bit" => Body::SevenBit(TextBody { body, ctype }),
                "8bit" => Body::EightBit(TextBody { body, ctype }),
                "binary" => Body::Binary(BinaryBody { body, ctype }),
                _ => Body::get_default(body, ctype),
            })
            .unwrap_or_else(|| Body::get_default(body, ctype))
    }

    fn get_default(body: &'a [u8], ctype: &'a ParsedContentType) -> Body<'a> {
        Body::SevenBit(TextBody { body, ctype })
    }
}

/// Struct that holds the encoded body representation of the message (or message subpart).
pub struct EncodedBody<'a> {
    decoder: fn(&[u8]) -> Result<Vec<u8>, MailParseError>,
    ctype: &'a ParsedContentType,
    body: &'a [u8],
}

impl<'a> EncodedBody<'a> {
    /// Get the body Content-Type
    pub fn get_content_type(&self) -> &'a ParsedContentType {
        self.ctype
    }

    /// Get the raw body of the message exactly as it is written in the message (or message subpart).
    pub fn get_raw(&self) -> &'a [u8] {
        self.body
    }

    /// Get the decoded body of the message (or message subpart).
    pub fn get_decoded(&self) -> Result<Vec<u8>, MailParseError> {
        (self.decoder)(self.body)
    }

    /// Get the body of the message as a Rust string.
    /// This function tries to decode the body and then converts
    /// the result into a Rust UTF-8 string using the charset in the Content-Type
    /// (or "us-ascii" if the charset was missing or not recognized).
    /// This operation returns a valid result only if the decoded body
    /// has a text format.
    pub fn get_decoded_as_string(&self) -> Result<String, MailParseError> {
        get_body_as_string(&self.get_decoded()?, self.ctype)
    }
}

/// Struct that holds the textual body representation of the message (or message subpart).
pub struct TextBody<'a> {
    ctype: &'a ParsedContentType,
    body: &'a [u8],
}

impl<'a> TextBody<'a> {
    /// Get the body Content-Type
    pub fn get_content_type(&self) -> &'a ParsedContentType {
        self.ctype
    }

    /// Get the raw body of the message exactly as it is written in the message (or message subpart).
    pub fn get_raw(&self) -> &'a [u8] {
        self.body
    }

    /// Get the body of the message as a Rust string.
    /// This function converts the body into a Rust UTF-8 string using the charset
    /// in the Content-Type
    /// (or "us-ascii" if the charset was missing or not recognized).
    pub fn get_as_string(&self) -> Result<String, MailParseError> {
        get_body_as_string(self.body, self.ctype)
    }
}

/// Struct that holds a binary body representation of the message (or message subpart).
pub struct BinaryBody<'a> {
    ctype: &'a ParsedContentType,
    body: &'a [u8],
}

impl<'a> BinaryBody<'a> {
    /// Get the body Content-Type
    pub fn get_content_type(&self) -> &'a ParsedContentType {
        self.ctype
    }

    /// Get the raw body of the message exactly as it is written in the message (or message subpart).
    pub fn get_raw(&self) -> &'a [u8] {
        self.body
    }

    /// Get the body of the message as a Rust string. This function attempts
    /// to convert the body into a Rust UTF-8 string using the charset in the
    /// Content-Type header (or "us-ascii" as default). However, this may not
    /// always work for "binary" data. The API is provided anyway for
    /// convenient handling of real-world emails that may provide textual data
    /// with a binary transfer encoding, but use this at your own risk!
    pub fn get_as_string(&self) -> Result<String, MailParseError> {
        get_body_as_string(self.body, self.ctype)
    }
}

fn decode_base64(body: &[u8]) -> Result<Vec<u8>, MailParseError> {
    let cleaned = body
        .iter()
        .filter(|c| !c.is_ascii_whitespace())
        .cloned()
        .collect::<Vec<u8>>();
    Ok(data_encoding::BASE64_MIME_PERMISSIVE.decode(&cleaned)?)
}

fn decode_quoted_printable(body: &[u8]) -> Result<Vec<u8>, MailParseError> {
    Ok(quoted_printable::decode(
        body,
        quoted_printable::ParseMode::Robust,
    )?)
}

fn get_body_as_string(body: &[u8], ctype: &ParsedContentType) -> Result<String, MailParseError> {
    let cow = if let Some(charset) = Charset::for_label(ctype.charset.as_bytes()) {
        let (cow, _, _) = charset.decode(body);
        cow
    } else {
        decode_ascii(body)
    };
    Ok(cow.into_owned())
}
