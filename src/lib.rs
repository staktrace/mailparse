#![forbid(unsafe_code)]

extern crate base64;
extern crate charset;
extern crate quoted_printable;

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::error;
use std::fmt;
use std::ops::Deref;

use charset::decode_latin1;

mod addrparse;
pub mod body;
mod dateparse;
mod header;
mod msgidparse;

pub use crate::addrparse::{
    addrparse, addrparse_header, GroupInfo, MailAddr, MailAddrList, SingleInfo,
};
use crate::body::Body;
pub use crate::dateparse::dateparse;
use crate::header::HeaderToken;
pub use crate::msgidparse::{msgidparse, MessageIdList};

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

    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
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

/// A struct that represents a single header in the message.
/// It holds slices into the raw byte array passed to parse_mail, and so the
/// lifetime of this struct must be contained within the lifetime of the raw
/// input. There are additional accessor functions on this struct to extract
/// the data as Rust strings.
#[derive(Debug)]
pub struct MailHeader {
    key: Vec<u8>,
    value: Vec<u8>,
}

pub(crate) fn find_from(line: &str, ix_start: usize, key: &str) -> Option<usize> {
    line[ix_start..].find(key).map(|v| ix_start + v)
}

fn find_from_u8(line: &Vec<u8>, ix_start: usize, key: &[u8]) -> Option<usize> {
    assert!(!key.is_empty());
    assert!(ix_start < line.len());
    if line.len() < key.len() {
        return None;
    }
    let ix_end = line.len() - key.len();
    if ix_start <= ix_end {
        for i in ix_start..ix_end {
            if line[i] == key[0] {
                let mut success = true;
                for j in 1..key.len() {
                    if line[i + j] != key[j] {
                        success = false;
                        break;
                    }
                }
                if success {
                    return Some(i);
                }
            }
        }
    }
    None
}

#[test]
fn test_find_from_u8() {
    assert_eq!(find_from_u8(b"hello world", 0, b"hell"), Some(0));
    assert_eq!(find_from_u8(b"hello world", 0, b"o"), Some(4));
    assert_eq!(find_from_u8(b"hello world", 4, b"o"), Some(4));
    assert_eq!(find_from_u8(b"hello world", 5, b"o"), Some(7));
    assert_eq!(find_from_u8(b"hello world", 8, b"o"), None);
    assert_eq!(find_from_u8(b"hello world", 10, b"d"), None);
}

impl MailHeader {
    /// Get the name of the header. Note that header names are case-insensitive.
    pub fn get_key(&self) -> String {
        decode_latin1(&self.key).into_owned()
    }

    /// Get the name of the header, borrowing if it's ASCII-only.
    /// Note that header names are case-insensitive.
    pub fn get_key_ref(&self) -> Cow<str> {
        decode_latin1(&self.key)
    }

    /// Get the value of the header. Any sequences of newlines characters followed
    /// by whitespace are collapsed into a single space. In effect, header values
    /// wrapped across multiple lines are compacted back into one line, while
    /// discarding the extra whitespace required by the MIME format. Additionally,
    /// any quoted-printable words in the value are decoded.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::parse_header;
    ///     let (parsed, _) = parse_header(b"Subject: =?iso-8859-1?Q?=A1Hola,_se=F1or!?=").unwrap();
    ///     assert_eq!(parsed.get_key(), "Subject");
    ///     assert_eq!(parsed.get_value(), "\u{a1}Hola, se\u{f1}or!");
    /// ```
    pub fn get_value(&self) -> String {
        let mut result = String::new();

        let chars = decode_latin1(&self.value);
        for tok in header::normalized_tokens(&chars) {
            match tok {
                HeaderToken::Text(t) => {
                    result.push_str(t);
                }
                HeaderToken::Whitespace(ws) => {
                    result.push_str(ws);
                }
                HeaderToken::Newline(Some(ws)) => {
                    result.push_str(&ws);
                }
                HeaderToken::Newline(None) => {}
                HeaderToken::DecodedWord(dw) => {
                    result.push_str(&dw);
                }
            }
        }

        result
    }
}

#[derive(Debug)]
enum HeaderParseState {
    Initial,
    Key,
    PreValue,
    Value,
    ValueNewline,
}

/// Parse a single header from the raw data given.
/// This function takes raw byte data, and starts parsing it, expecting there
/// to be a MIME header key-value pair right at the beginning. It parses that
/// header and returns it, along with the index at which the next header is
/// expected to start. If you just want to parse a single header, you can ignore
/// the second component of the tuple, which is the index of the next header.
/// Error values are returned if the data could not be successfully interpreted
/// as a MIME key-value pair.
///
/// # Examples
/// ```
///     use mailparse::parse_header;
///     let (parsed, _) = parse_header(concat!(
///             "Subject: Hello, sir,\n",
///             "   I am multiline\n",
///             "Next:Header").as_bytes())
///         .unwrap();
///     assert_eq!(parsed.get_key(), "Subject");
///     assert_eq!(parsed.get_value(), "Hello, sir, I am multiline");
/// ```
pub fn parse_header(raw_data: &[u8]) -> Result<(MailHeader, usize), MailParseError> {
    let mut it = raw_data.iter();
    let mut ix = 0;
    let mut c = match it.next() {
        None => return Err(MailParseError::Generic("Empty string provided")),
        Some(v) => *v,
    };

    let mut ix_key_end = None;
    let mut ix_value_start = 0;
    let mut ix_value_end = 0;

    let mut state = HeaderParseState::Initial;
    loop {
        match state {
            HeaderParseState::Initial => {
                if c == b' ' {
                    return Err(MailParseError::Generic(
                        "Header cannot start with a space; it is \
                         likely an overhanging line from a \
                         previous header",
                    ));
                };
                state = HeaderParseState::Key;
                continue;
            }
            HeaderParseState::Key => {
                if c == b':' {
                    ix_key_end = Some(ix);
                    state = HeaderParseState::PreValue;
                } else if c == b'\n' {
                    return Err(MailParseError::Generic("Unexpected newline in header key"));
                }
            }
            HeaderParseState::PreValue => {
                if c != b' ' {
                    ix_value_start = ix;
                    ix_value_end = ix;
                    state = HeaderParseState::Value;
                    continue;
                }
            }
            HeaderParseState::Value => {
                if c == b'\n' {
                    state = HeaderParseState::ValueNewline;
                } else {
                    ix_value_end = ix + 1;
                }
            }
            HeaderParseState::ValueNewline => {
                if c == b' ' || c == b'\t' {
                    state = HeaderParseState::Value;
                    continue;
                } else {
                    break;
                }
            }
        }
        ix += 1;
        c = match it.next() {
            None => break,
            Some(v) => *v,
        };
    }
    match ix_key_end {
        Some(v) => Ok((
            MailHeader {
                key: raw_data[0..v].to_vec(),
                value: raw_data[ix_value_start..ix_value_end].to_vec(),
            },
            ix,
        )),

        None => Err(MailParseError::Generic(
            "Unable to determine end of the header key component",
        )),
    }
}

/// A trait that is implemented by the [MailHeader] slice. These functions are
/// also available on Vec<MailHeader> which is returned by the parse_headers
/// function. It provides a map-like interface to look up header values by their
/// name.
pub trait MailHeaderMap {
    /// Look through the list of headers and return the value of the first one
    /// that matches the provided key. It returns Ok(None) if the no matching
    /// header was found. Header names are matched case-insensitively.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::{parse_mail, MailHeaderMap};
    ///     let headers = parse_mail(concat!(
    ///             "Subject: Test\n",
    ///             "\n",
    ///             "This is a test message").as_bytes())
    ///         .unwrap().headers;
    ///     assert_eq!(headers.get_first_value("Subject"), Some("Test".to_string()));
    /// ```
    fn get_first_value(&self, key: &str) -> Option<String>;

    /// Similar to `get_first_value`, except it returns a reference to the
    /// MailHeader struct instead of just extracting the value.
    fn get_first_header(&self, key: &str) -> Option<&MailHeader>;

    /// Look through the list of headers and return the values of all headers
    /// matching the provided key. Returns an empty vector if no matching headers
    /// were found. The order of the returned values is the same as the order
    /// of the matching headers in the message. Header names are matched
    /// case-insensitively.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::{parse_mail, MailHeaderMap};
    ///     let headers = parse_mail(concat!(
    ///             "Key: Value1\n",
    ///             "Key: Value2").as_bytes())
    ///         .unwrap().headers;
    ///     assert_eq!(headers.get_all_values("Key"),
    ///         vec!["Value1".to_string(), "Value2".to_string()]);
    /// ```
    fn get_all_values(&self, key: &str) -> Vec<String>;

    /// Similar to `get_all_values`, except it returns references to the
    /// MailHeader structs instead of just extracting the values.
    fn get_all_headers(&self, key: &str) -> Vec<&MailHeader>;
}

impl MailHeaderMap for [MailHeader] {
    fn get_first_value(&self, key: &str) -> Option<String> {
        for x in self {
            if x.get_key_ref().eq_ignore_ascii_case(key) {
                return Some(x.get_value());
            }
        }
        None
    }

    fn get_first_header(&self, key: &str) -> Option<&MailHeader> {
        for x in self {
            if x.get_key_ref().eq_ignore_ascii_case(key) {
                return Some(x);
            }
        }
        None
    }

    fn get_all_values(&self, key: &str) -> Vec<String> {
        let mut values: Vec<String> = Vec::new();
        for x in self {
            if x.get_key_ref().eq_ignore_ascii_case(key) {
                values.push(x.get_value());
            }
        }
        values
    }

    fn get_all_headers(&self, key: &str) -> Vec<&MailHeader> {
        let mut headers: Vec<&MailHeader> = Vec::new();
        for x in self {
            if x.get_key_ref().eq_ignore_ascii_case(key) {
                headers.push(x);
            }
        }
        headers
    }
}

/// Parses all the headers from the raw data given.
/// This function takes raw byte data, and starts parsing it, expecting there
/// to be zero or more MIME header key-value pair right at the beginning,
/// followed by two consecutive newlines (i.e. a blank line). It parses those
/// headers and returns them in a vector. The normal vector functions can be
/// used to access the headers linearly, or the MailHeaderMap trait can be used
/// to access them in a map-like fashion. Along with this vector, the function
/// returns the index at which the message body is expected to start. If you
/// just care about the headers, you can ignore the second component of the
/// returned tuple.
/// Error values are returned if there was some sort of parsing error.
///
/// # Examples
/// ```
///     use mailparse::{parse_headers, MailHeaderMap};
///     let (headers, _) = parse_headers(concat!(
///             "Subject: Test\n",
///             "From: me@myself.com\n",
///             "To: you@yourself.com").as_bytes())
///         .unwrap();
///     assert_eq!(headers[1].get_key(), "From");
///     assert_eq!(headers.get_first_value("To"), Some("you@yourself.com".to_string()));
/// ```
pub fn parse_headers(raw_data: &[u8]) -> Result<(Vec<MailHeader>, usize), MailParseError> {
    let mut headers: Vec<MailHeader> = Vec::new();
    let mut ix = 0;
    loop {
        if ix >= raw_data.len() {
            break;
        } else if raw_data[ix] == b'\n' {
            ix += 1;
            break;
        } else if raw_data[ix] == b'\r' {
            if ix + 1 < raw_data.len() && raw_data[ix + 1] == b'\n' {
                ix += 2;
                break;
            } else {
                return Err(MailParseError::Generic(
                    "Headers were followed by an unexpected lone \
                     CR character!",
                ));
            }
        }
        let (header, ix_next) = parse_header(&raw_data[ix..])?;
        headers.push(header);
        ix += ix_next;
    }
    Ok((headers, ix))
}

/// A struct to hold a more structured representation of the Content-Type header.
/// This is provided mostly as a convenience since this metadata is usually
/// needed to interpret the message body properly.
#[derive(Debug)]
pub struct ParsedContentType {
    /// The type of the data, for example "text/plain" or "application/pdf".
    pub mimetype: String,
    /// The charset used to decode the raw byte data, for example "iso-8859-1"
    /// or "utf-8".
    pub charset: String,
    /// The additional params of Content-Type, e.g. filename and boundary. The
    /// keys in the map will be lowercased, and the values will have any
    /// enclosing quotes stripped.
    pub params: BTreeMap<String, String>,
}

impl Default for ParsedContentType {
    fn default() -> Self {
        ParsedContentType {
            mimetype: "text/plain".to_string(),
            charset: "us-ascii".to_string(),
            params: BTreeMap::new(),
        }
    }
}

/// Helper method to parse a header value as a Content-Type header. Note that
/// the returned object's `params` map will contain a charset key if a charset
/// was explicitly specified in the header; otherwise the `params` map will not
/// contain a charset key. Regardless, the `charset` field will contain a
/// charset - either the one explicitly specified or the default of "us-ascii".
///
/// # Examples
/// ```
///     use mailparse::{parse_header, parse_content_type};
///     let (parsed, _) = parse_header(
///             b"Content-Type: text/html; charset=foo; boundary=\"quotes_are_removed\"")
///         .unwrap();
///     let ctype = parse_content_type(&parsed.get_value());
///     assert_eq!(ctype.mimetype, "text/html");
///     assert_eq!(ctype.charset, "foo");
///     assert_eq!(ctype.params.get("boundary"), Some(&"quotes_are_removed".to_string()));
///     assert_eq!(ctype.params.get("charset"), Some(&"foo".to_string()));
/// ```
/// ```
///     use mailparse::{parse_header, parse_content_type};
///     let (parsed, _) = parse_header(b"Content-Type: bogus").unwrap();
///     let ctype = parse_content_type(&parsed.get_value());
///     assert_eq!(ctype.mimetype, "bogus");
///     assert_eq!(ctype.charset, "us-ascii");
///     assert_eq!(ctype.params.get("boundary"), None);
///     assert_eq!(ctype.params.get("charset"), None);
/// ```
/// ```
///     use mailparse::{parse_header, parse_content_type};
///     let (parsed, _) = parse_header(br#"Content-Type: application/octet-stream;name="=?utf8?B?6L+O5ai255m95a+M576O?=";charset="utf8""#).unwrap();
///     let ctype = parse_content_type(&parsed.get_value());
///     assert_eq!(ctype.mimetype, "application/octet-stream");
///     assert_eq!(ctype.charset, "utf8");
///     assert_eq!(ctype.params.get("boundary"), None);
///     assert_eq!(ctype.params.get("name"), Some(&"迎娶白富美".to_string()));
/// ```
pub fn parse_content_type(header: &str) -> ParsedContentType {
    let params = parse_param_content(header);
    let mimetype = params.value.to_lowercase();
    let charset = params
        .params
        .get("charset")
        .cloned()
        .unwrap_or_else(|| "us-ascii".to_string());

    ParsedContentType {
        mimetype,
        charset,
        params: params.params,
    }
}

/// The possible disposition types in a Content-Disposition header. A more
/// comprehensive list of IANA-recognized types can be found at
/// https://www.iana.org/assignments/cont-disp/cont-disp.xhtml. This library
/// only enumerates the types most commonly found in email messages, and
/// provides the `Extension` value for holding all other types.
#[derive(Debug, Clone, PartialEq)]
pub enum DispositionType {
    /// Default value, indicating the content is to be displayed inline as
    /// part of the enclosing document.
    Inline,
    /// A disposition indicating the content is not meant for inline display,
    /// but whose content can be accessed for use.
    Attachment,
    /// A disposition indicating the content contains a form submission.
    FormData,
    /// Extension type to hold any disposition not explicitly enumerated.
    Extension(String),
}

impl Default for DispositionType {
    fn default() -> Self {
        DispositionType::Inline
    }
}

/// Convert the string represented disposition type to enum.
fn parse_disposition_type(disposition: &str) -> DispositionType {
    match &disposition.to_lowercase()[..] {
        "inline" => DispositionType::Inline,
        "attachment" => DispositionType::Attachment,
        "form-data" => DispositionType::FormData,
        extension => DispositionType::Extension(extension.to_string()),
    }
}

/// A struct to hold a more structured representation of the Content-Disposition header.
/// This is provided mostly as a convenience since this metadata is usually
/// needed to interpret the message body properly.
#[derive(Debug, Default)]
pub struct ParsedContentDisposition {
    /// The disposition type of the Content-Disposition header. If this
    /// is an extension type, the string will be lowercased.
    pub disposition: DispositionType,
    /// The additional params of Content-Disposition, e.g. filename. The
    /// keys in the map will be lowercased, and the values will have any
    /// enclosing quotes stripped.
    pub params: BTreeMap<String, String>,
}

/// Helper method to parse a header value as a Content-Disposition header. The disposition
/// defaults to "inline" if no disposition parameter is provided in the header
/// value.
///
/// # Examples
/// ```
///     use mailparse::{parse_header, parse_content_disposition, DispositionType};
///     let (parsed, _) = parse_header(
///             b"Content-Disposition: attachment; filename=\"yummy dummy\"")
///         .unwrap();
///     let dis = parse_content_disposition(&parsed.get_value());
///     assert_eq!(dis.disposition, DispositionType::Attachment);
///     assert_eq!(dis.params.get("name"), None);
///     assert_eq!(dis.params.get("filename"), Some(&"yummy dummy".to_string()));
/// ```
pub fn parse_content_disposition(header: &str) -> ParsedContentDisposition {
    let params = parse_param_content(header);
    let disposition = parse_disposition_type(&params.value);
    ParsedContentDisposition {
        disposition,
        params: params.params,
    }
}

/// Struct that holds the structured representation of the message. Note that
/// since MIME allows for nested multipart messages, a tree-like structure is
/// necessary to represent it properly. This struct accomplishes that by holding
/// a vector of other ParsedMail structures for the subparts.
#[derive(Debug)]
pub struct ParsedMail {
    /// The actual buffer holding the complete mail
    buffer: Vec<u8>,

    /// The headers for the message (or message subpart).
    pub headers: Vec<MailHeader>,
    /// The Content-Type information for the message (or message subpart).
    pub ctype: ParsedContentType,
    /// The raw bytes that make up the body of the message (or message subpart).
    body: Vec<u8>,
    /// The subparts of this message or subpart. This vector is only non-empty
    /// if ctype.mimetype starts with "multipart/".
    pub subparts: Vec<ParsedMail>,
}

impl ParsedMail {
    /// Get the body of the message as a Rust string. This function tries to
    /// unapply the Content-Transfer-Encoding if there is one, and then converts
    /// the result into a Rust UTF-8 string using the charset in the Content-Type
    /// (or "us-ascii" if the charset was missing or not recognized).
    ///
    /// # Examples
    /// ```
    ///     use mailparse::parse_mail;
    ///     let p = parse_mail(concat!(
    ///             "Subject: test\n",
    ///             "\n",
    ///             "This is the body").as_bytes())
    ///         .unwrap();
    ///     assert_eq!(p.get_body().unwrap(), "This is the body");
    /// ```
    pub fn get_body(&self) -> Result<String, MailParseError> {
        match self.get_body_encoded() {
            Body::Base64(body) | Body::QuotedPrintable(body) => body.get_decoded_as_string(),
            Body::SevenBit(body) | Body::EightBit(body) => body.get_as_string(),
            Body::Binary(_) => Err(MailParseError::Generic(
                "Message body of type binary body cannot be parsed into a string",
            )),
        }
    }

    /// Get the body of the message as a Rust Vec<u8>. This function tries to
    /// unapply the Content-Transfer-Encoding if there is one, but won't do
    /// any charset decoding.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::parse_mail;
    ///     let p = parse_mail(concat!(
    ///             "Subject: test\n",
    ///             "\n",
    ///             "This is the body").as_bytes())
    ///         .unwrap();
    ///     assert_eq!(p.get_body_raw().unwrap(), b"This is the body");
    /// ```
    pub fn get_body_raw(&self) -> Result<Vec<u8>, MailParseError> {
        match self.get_body_encoded() {
            Body::Base64(body) | Body::QuotedPrintable(body) => body.get_decoded(),
            Body::SevenBit(body) | Body::EightBit(body) => Ok(body.get_raw().clone()),
            Body::Binary(body) => Ok(body.get_raw().clone()),
        }
    }

    /// Get the body of the message.
    /// This function returns original the body without attempting to
    /// unapply the Content-Transfer-Encoding.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::parse_mail;
    ///     use mailparse::body::Body;
    ///
    ///     let mail = parse_mail(b"Content-Transfer-Encoding: base64\r\n\r\naGVsbG 8gd\r\n29ybGQ=").unwrap();
    ///
    ///     match mail.get_body_encoded() {
    ///         Body::Base64(body) => {
    ///             assert_eq!(body.get_raw(), b"aGVsbG 8gd\r\n29ybGQ=");
    ///             assert_eq!(body.get_decoded().unwrap(), b"hello world");
    ///             assert_eq!(body.get_decoded_as_string().unwrap(), "hello world");
    ///         },
    ///         _ => assert!(false),
    ///     };
    ///
    ///
    ///     // An email whose body encoding is not known upfront
    ///     let another_mail = parse_mail(b"").unwrap();
    ///
    ///     match another_mail.get_body_encoded() {
    ///         Body::Base64(body) | Body::QuotedPrintable(body) => {
    ///             println!("mail body encoded: {:?}", body.get_raw());
    ///             println!("mail body decoded: {:?}", body.get_decoded().unwrap());
    ///             println!("mail body decoded as string: {}", body.get_decoded_as_string().unwrap());
    ///         },
    ///         Body::SevenBit(body) | Body::EightBit(body) => {
    ///             println!("mail body: {:?}", body.get_raw());
    ///             println!("mail body as string: {}", body.get_as_string().unwrap());
    ///         },
    ///         Body::Binary(body) => {
    ///             println!("mail body binary: {:?}", body.get_raw());
    ///         }
    ///     }
    /// ```
    pub fn get_body_encoded<'a>(&'a self) -> Body<'a> {
        let transfer_encoding = self
            .headers
            .get_first_value("Content-Transfer-Encoding")
            .map(|s| s.to_lowercase());

        Body::new(self.body.clone(), &self.ctype, &transfer_encoding)
    }

    /// Returns a struct containing a parsed representation of the
    /// Content-Disposition header. The first header with this name
    /// is used, if there are multiple. See the `parse_content_disposition`
    /// method documentation for more details on the semantics of the
    /// returned object.
    pub fn get_content_disposition(&self) -> ParsedContentDisposition {
        let disposition = self
            .headers
            .get_first_value("Content-Disposition")
            .map(|s| parse_content_disposition(&s))
            .unwrap_or_default();
        disposition
    }
}

/// The main mail-parsing entry point.
/// This function takes the raw data making up the message body and returns a
/// structured version of it, which allows easily accessing the header and body
/// information as needed.
///
/// # Examples
/// ```
///     use mailparse::*;
///     let parsed = parse_mail(concat!(
///             "Subject: This is a test email\n",
///             "Content-Type: multipart/alternative; boundary=foobar\n",
///             "Date: Sun, 02 Oct 2016 07:06:22 -0700 (PDT)\n",
///             "\n",
///             "--foobar\n",
///             "Content-Type: text/plain; charset=utf-8\n",
///             "Content-Transfer-Encoding: quoted-printable\n",
///             "\n",
///             "This is the plaintext version, in utf-8. Proof by Euro: =E2=82=AC\n",
///             "--foobar\n",
///             "Content-Type: text/html\n",
///             "Content-Transfer-Encoding: base64\n",
///             "\n",
///             "PGh0bWw+PGJvZHk+VGhpcyBpcyB0aGUgPGI+SFRNTDwvYj4gdmVyc2lvbiwgaW4g \n",
///             "dXMtYXNjaWkuIFByb29mIGJ5IEV1cm86ICZldXJvOzwvYm9keT48L2h0bWw+Cg== \n",
///             "--foobar--\n",
///             "After the final boundary stuff gets ignored.\n").as_bytes())
///         .unwrap();
///     assert_eq!(parsed.headers.get_first_value("Subject"),
///         Some("This is a test email".to_string()));
///     assert_eq!(parsed.subparts.len(), 2);
///     assert_eq!(parsed.subparts[0].get_body().unwrap(),
///         "This is the plaintext version, in utf-8. Proof by Euro: \u{20AC}");
///     assert_eq!(parsed.subparts[1].headers[1].get_value(), "base64");
///     assert_eq!(parsed.subparts[1].ctype.mimetype, "text/html");
///     assert!(parsed.subparts[1].get_body().unwrap().starts_with("<html>"));
///     assert_eq!(dateparse(parsed.headers.get_first_value("Date").unwrap().as_str()).unwrap(), 1475417182);
/// ```
pub fn parse_mail(raw_data: Vec<u8>) -> Result<ParsedMail, MailParseError> {
    let (headers, ix_body) = parse_headers(&raw_data)?;
    let ctype = headers
        .get_first_value("Content-Type")
        .map(|s| parse_content_type(&s))
        .unwrap_or_default();

    let raw_data_len = raw_data.len();

    let mut result = ParsedMail {
        headers,
        ctype,
        body: raw_data[ix_body..].to_vec(),
        subparts: Vec::<ParsedMail>::new(),
        buffer: raw_data,
    };
    if result.ctype.mimetype.starts_with("multipart/")
        && result.ctype.params.get("boundary").is_some()
        && raw_data_len > ix_body
    {
        let boundary = String::from("--") + &result.ctype.params["boundary"];
        if let Some(ix_body_end) = find_from_u8(&result.buffer, ix_body, boundary.as_bytes()) {
            result.body = result.buffer[ix_body..ix_body_end].to_vec();
            let mut ix_boundary_end = ix_body_end + boundary.len();
            while let Some(ix_part_start) =
                find_from_u8(&result.buffer, ix_boundary_end, b"\n").map(|v| v + 1)
            {
                // if there is no terminating boundary, assume the part end is the end of the email
                let ix_part_end = find_from_u8(&result.buffer, ix_part_start, boundary.as_bytes())
                    .unwrap_or_else(|| raw_data_len);

                result
                    .subparts
                    .push(parse_mail(result.buffer[ix_part_start..ix_part_end].to_vec())?);
                ix_boundary_end = ix_part_end + boundary.len();
                if ix_boundary_end + 2 > raw_data_len
                    || (result.buffer[ix_boundary_end] == b'-' && result.buffer[ix_boundary_end + 1] == b'-')
                {
                    break;
                }
            }
        }
    }
    Ok(result)
}

/// Used to store params for content-type and content-disposition
struct ParamContent {
    value: String,
    params: BTreeMap<String, String>,
}

/// Parse parameterized header values such as that for Content-Type
/// e.g. `multipart/alternative; boundary=foobar`
/// Note: this function is not made public as it may require
/// significant changes to be fully correct. For instance,
/// it does not handle quoted parameter values containing the
/// semicolon (';') character. It also produces a BTreeMap,
/// which implicitly does not support multiple parameters with
/// the same key. The format for parameterized header values
/// doesn't appear to be strongly specified anywhere.
fn parse_param_content(content: &str) -> ParamContent {
    let mut tokens = content.split(';');
    // There must be at least one token produced by split, even if it's empty.
    let value = tokens.next().unwrap().trim();
    let map = tokens
        .filter_map(|kv| {
            kv.find('=').map(|idx| {
                let key = kv[0..idx].trim().to_lowercase();
                let mut value = kv[idx + 1..].trim();
                if value.starts_with('"') && value.ends_with('"') && value.len() > 1 {
                    value = &value[1..value.len() - 1];
                }
                (key, value.to_string())
            })
        })
        .collect();

    ParamContent {
        value: value.into(),
        params: map,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_match {
        ( $x:expr, $p:pat ) => {
            match $x {
                $p => (),
                _ => panic!(
                    "Expression {} does not match pattern {}",
                    $x,
                    stringify!($p)
                ),
            }
        };
    }

    #[test]
    fn parse_basic_header() {
        let (parsed, _) = parse_header(b"Key: Value").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.get_key(), "Key");
        assert_eq!(parsed.get_key_ref(), "Key");
        assert_eq!(parsed.value, b"Value");
        assert_eq!(parsed.get_value(), "Value");

        let (parsed, _) = parse_header(b"Key :  Value ").unwrap();
        assert_eq!(parsed.key, b"Key ");
        assert_eq!(parsed.value, b"Value ");
        assert_eq!(parsed.get_value(), "Value ");

        let (parsed, _) = parse_header(b"Key:").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"");

        let (parsed, _) = parse_header(b":\n").unwrap();
        assert_eq!(parsed.key, b"");
        assert_eq!(parsed.value, b"");

        let (parsed, _) = parse_header(b"Key:Multi-line\n value").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"Multi-line\n value");
        assert_eq!(parsed.get_value(), "Multi-line value");

        let (parsed, _) = parse_header(b"Key:  Multi\n  line\n value\n").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"Multi\n  line\n value");
        assert_eq!(parsed.get_value(), "Multi line value");

        let (parsed, _) = parse_header(b"Key: One\nKey2: Two").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"One");

        let (parsed, _) = parse_header(b"Key: One\n\tOverhang").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"One\n\tOverhang");
        assert_eq!(parsed.get_value(), "One Overhang");

        let (parsed, _) = parse_header(b"SPAM: VIAGRA \xAE").unwrap();
        assert_eq!(parsed.key, b"SPAM");
        assert_eq!(parsed.value, b"VIAGRA \xAE");
        assert_eq!(parsed.get_value(), "VIAGRA \u{ae}");

        parse_header(b" Leading: Space").unwrap_err();
        parse_header(b"Just a string").unwrap_err();
        parse_header(b"Key\nBroken: Value").unwrap_err();
    }

    #[test]
    fn parse_encoded_headers() {
        let (parsed, _) = parse_header(b"Subject: =?iso-8859-1?Q?=A1Hola,_se=F1or!?=").unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(parsed.get_value(), "\u{a1}Hola, se\u{f1}or!");

        let (parsed, _) = parse_header(
            b"Subject: =?iso-8859-1?Q?=A1Hola,?=\n \
                                        =?iso-8859-1?Q?_se=F1or!?=",
        )
        .unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(parsed.get_value(), "\u{a1}Hola, se\u{f1}or!");

        let (parsed, _) = parse_header(b"Euro: =?utf-8?Q?=E2=82=AC?=").unwrap();
        assert_eq!(parsed.get_key(), "Euro");
        assert_eq!(parsed.get_key_ref(), "Euro");
        assert_eq!(parsed.get_value(), "\u{20ac}");

        let (parsed, _) = parse_header(b"HelloWorld: =?utf-8?B?aGVsbG8gd29ybGQ=?=").unwrap();
        assert_eq!(parsed.get_value(), "hello world");

        let (parsed, _) = parse_header(b"Empty: =?utf-8?Q??=").unwrap();
        assert_eq!(parsed.get_value(), "");

        let (parsed, _) = parse_header(b"Incomplete: =?").unwrap();
        assert_eq!(parsed.get_value(), "=?");

        let (parsed, _) = parse_header(b"BadEncoding: =?garbage?Q??=").unwrap();
        assert_eq!(parsed.get_value(), "=?garbage?Q??=");

        let (parsed, _) = parse_header(b"Invalid: =?utf-8?Q?=E2=AC?=").unwrap();
        assert_eq!(parsed.get_value(), "\u{fffd}");

        let (parsed, _) = parse_header(b"LineBreak: =?utf-8?Q?=E2=82\n =AC?=").unwrap();
        assert_eq!(parsed.get_value(), "=?utf-8?Q?=E2=82 =AC?=");

        let (parsed, _) = parse_header(b"NotSeparateWord: hello=?utf-8?Q?world?=").unwrap();
        assert_eq!(parsed.get_value(), "hello=?utf-8?Q?world?=");

        let (parsed, _) = parse_header(b"NotSeparateWord2: =?utf-8?Q?hello?=world").unwrap();
        assert_eq!(parsed.get_value(), "=?utf-8?Q?hello?=world");

        let (parsed, _) = parse_header(b"Key: \"=?utf-8?Q?value?=\"").unwrap();
        assert_eq!(parsed.get_value(), "\"value\"");

        let (parsed, _) = parse_header(b"Subject: =?utf-8?q?=5BOntario_Builder=5D_Understanding_home_shopping_=E2=80=93_a_q?=\n \
                                        =?utf-8?q?uick_survey?=")
            .unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(
            parsed.get_value(),
            "[Ontario Builder] Understanding home shopping \u{2013} a quick survey"
        );

        let (parsed, _) = parse_header(
            b"Subject: =?utf-8?q?=5BOntario_Builder=5D?= non-qp words\n \
             and the subject continues",
        )
        .unwrap();
        assert_eq!(
            parsed.get_value(),
            "[Ontario Builder] non-qp words and the subject continues"
        );

        let (parsed, _) = parse_header(
            b"Subject: =?utf-8?q?=5BOntario_Builder=5D?= \n \
             and the subject continues",
        )
        .unwrap();
        assert_eq!(
            parsed.get_value(),
            "[Ontario Builder]  and the subject continues"
        );

        let (parsed, _) = parse_header(b"Subject: =?ISO-2022-JP?B?GyRCRnwbKEI=?=\n\t=?ISO-2022-JP?B?GyRCS1wbKEI=?=\n\t=?ISO-2022-JP?B?GyRCOGwbKEI=?=")
            .unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(parsed.get_value(), "\u{65E5}\u{672C}\u{8A9E}");

        let (parsed, _) = parse_header(b"Subject: =?ISO-2022-JP?Q?=1B\x24\x42\x46\x7C=1B\x28\x42?=\n\t=?ISO-2022-JP?Q?=1B\x24\x42\x4B\x5C=1B\x28\x42?=\n\t=?ISO-2022-JP?Q?=1B\x24\x42\x38\x6C=1B\x28\x42?=")
            .unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(parsed.get_value(), "\u{65E5}\u{672C}\u{8A9E}");

        let (parsed, _) = parse_header(b"Subject: =?UTF-7?Q?+JgM-?=").unwrap();
        assert_eq!(parsed.get_key(), "Subject");
        assert_eq!(parsed.get_key_ref(), "Subject");
        assert_eq!(parsed.get_value(), "\u{2603}");

        let (parsed, _) =
            parse_header(b"Content-Type: image/jpeg; name=\"=?UTF-8?B?MDY2MTM5ODEuanBn?=\"")
                .unwrap();
        assert_eq!(parsed.get_key(), "Content-Type");
        assert_eq!(parsed.get_key_ref(), "Content-Type");
        assert_eq!(parsed.get_value(), "image/jpeg; name=\"06613981.jpg\"");

        let (parsed, _) = parse_header(
            b"From: =?UTF-8?Q?\"Motorola_Owners=E2=80=99_Forums\"_?=<forums@motorola.com>",
        )
        .unwrap();
        assert_eq!(parsed.get_key(), "From");
        assert_eq!(parsed.get_key_ref(), "From");
        assert_eq!(
            parsed.get_value(),
            "\"Motorola Owners\u{2019} Forums\" <forums@motorola.com>"
        );
    }

    #[test]
    fn encoded_words_and_spaces() {
        let (parsed, _) = parse_header(b"K: an =?utf-8?q?encoded?=\n word").unwrap();
        assert_eq!(parsed.get_value(), "an encoded word");

        let (parsed, _) = parse_header(b"K: =?utf-8?q?glue?= =?utf-8?q?these?= \n words").unwrap();
        assert_eq!(parsed.get_value(), "gluethese  words");

        let (parsed, _) = parse_header(b"K: =?utf-8?q?glue?= \n =?utf-8?q?again?=").unwrap();
        assert_eq!(parsed.get_value(), "glueagain");
    }

    #[test]
    fn parse_multiple_headers() {
        let (parsed, _) = parse_headers(b"Key: Value\nTwo: Second").unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].key, b"Key");
        assert_eq!(parsed[0].value, b"Value");
        assert_eq!(parsed[1].key, b"Two");
        assert_eq!(parsed[1].value, b"Second");

        let (parsed, _) =
            parse_headers(b"Key: Value\n Overhang\nTwo: Second\nThree: Third").unwrap();
        assert_eq!(parsed.len(), 3);
        assert_eq!(parsed[0].key, b"Key");
        assert_eq!(parsed[0].value, b"Value\n Overhang");
        assert_eq!(parsed[1].key, b"Two");
        assert_eq!(parsed[1].value, b"Second");
        assert_eq!(parsed[2].key, b"Three");
        assert_eq!(parsed[2].value, b"Third");

        let (parsed, _) = parse_headers(b"Key: Value\nTwo: Second\n\nBody").unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].key, b"Key");
        assert_eq!(parsed[0].value, b"Value");
        assert_eq!(parsed[1].key, b"Two");
        assert_eq!(parsed[1].value, b"Second");

        let (parsed, _) = parse_headers(
            concat!(
                "Return-Path: <kats@foobar.staktrace.com>\n",
                "X-Original-To: kats@baz.staktrace.com\n",
                "Delivered-To: kats@baz.staktrace.com\n",
                "Received: from foobar.staktrace.com (localhost [127.0.0.1])\n",
                "    by foobar.staktrace.com (Postfix) with ESMTP id \
                 139F711C1C34\n",
                "    for <kats@baz.staktrace.com>; Fri, 27 May 2016 02:34:26 \
                 -0400 (EDT)\n",
                "Date: Fri, 27 May 2016 02:34:25 -0400\n",
                "To: kats@baz.staktrace.com\n",
                "From: kats@foobar.staktrace.com\n",
                "Subject: test Fri, 27 May 2016 02:34:25 -0400\n",
                "X-Mailer: swaks v20130209.0 jetmore.org/john/code/swaks/\n",
                "Message-Id: \
                 <20160527063426.139F711C1C34@foobar.staktrace.com>\n",
                "\n",
                "This is a test mailing\n"
            )
            .as_bytes(),
        )
        .unwrap();
        assert_eq!(parsed.len(), 10);
        assert_eq!(parsed[0].key, b"Return-Path");
        assert_eq!(parsed[9].key, b"Message-Id");

        let (parsed, _) =
            parse_headers(b"Key: Value\nAnotherKey: AnotherValue\nKey: Value2\nKey: Value3\n")
                .unwrap();
        assert_eq!(parsed.len(), 4);
        assert_eq!(parsed.get_first_value("Key"), Some("Value".to_string()));
        assert_eq!(
            parsed.get_all_values("Key"),
            vec!["Value", "Value2", "Value3"]
        );
        assert_eq!(
            parsed.get_first_value("AnotherKey"),
            Some("AnotherValue".to_string())
        );
        assert_eq!(parsed.get_all_values("AnotherKey"), vec!["AnotherValue"]);
        assert_eq!(parsed.get_first_value("NoKey"), None);
        assert_eq!(parsed.get_all_values("NoKey"), Vec::<String>::new());

        let (parsed, _) = parse_headers(b"Key: value\r\nWith: CRLF\r\n\r\nBody").unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed.get_first_value("Key"), Some("value".to_string()));
        assert_eq!(parsed.get_first_value("With"), Some("CRLF".to_string()));

        assert_match!(
            parse_headers(b"Bad\nKey").unwrap_err(),
            MailParseError::Generic(_)
        );
        assert_match!(
            parse_headers(b"K:V\nBad\nKey").unwrap_err(),
            MailParseError::Generic(_)
        );
    }

    #[test]
    fn test_parse_content_type() {
        let ctype = parse_content_type("text/html; charset=utf-8");
        assert_eq!(ctype.mimetype, "text/html");
        assert_eq!(ctype.charset, "utf-8");
        assert_eq!(ctype.params.get("boundary"), None);

        let ctype = parse_content_type(" foo/bar; x=y; charset=\"fake\" ; x2=y2");
        assert_eq!(ctype.mimetype, "foo/bar");
        assert_eq!(ctype.charset, "fake");
        assert_eq!(ctype.params.get("boundary"), None);

        let ctype = parse_content_type(" multipart/bar; boundary=foo ");
        assert_eq!(ctype.mimetype, "multipart/bar");
        assert_eq!(ctype.charset, "us-ascii");
        assert_eq!(ctype.params.get("boundary").unwrap(), "foo");
    }

    #[test]
    fn test_parse_content_disposition() {
        let dis = parse_content_disposition("inline");
        assert_eq!(dis.disposition, DispositionType::Inline);
        assert_eq!(dis.params.get("name"), None);
        assert_eq!(dis.params.get("filename"), None);

        let dis = parse_content_disposition(
            " attachment; x=y; charset=\"fake\" ; x2=y2; name=\"King Joffrey.death\"",
        );
        assert_eq!(dis.disposition, DispositionType::Attachment);
        assert_eq!(
            dis.params.get("name"),
            Some(&"King Joffrey.death".to_string())
        );
        assert_eq!(dis.params.get("filename"), None);

        let dis = parse_content_disposition(" form-data");
        assert_eq!(dis.disposition, DispositionType::FormData);
        assert_eq!(dis.params.get("name"), None);
        assert_eq!(dis.params.get("filename"), None);
    }

    #[test]
    fn test_parse_mail() {
        let mail = parse_mail(b"Key: value\r\n\r\nSome body stuffs").unwrap();
        assert_eq!(mail.headers.len(), 1);
        assert_eq!(mail.headers[0].get_key(), "Key");
        assert_eq!(mail.headers[0].get_key_ref(), "Key");
        assert_eq!(mail.headers[0].get_value(), "value");
        assert_eq!(mail.ctype.mimetype, "text/plain");
        assert_eq!(mail.ctype.charset, "us-ascii");
        assert_eq!(mail.ctype.params.get("boundary"), None);
        assert_eq!(mail.body, b"Some body stuffs");
        assert_eq!(mail.get_body_raw().unwrap(), b"Some body stuffs");
        assert_eq!(mail.get_body().unwrap(), "Some body stuffs");
        assert_eq!(mail.subparts.len(), 0);

        let mail = parse_mail(
            concat!(
                "Content-Type: MULTIpart/alternative; bounDAry=myboundary\r\n\r\n",
                "--myboundary\r\n",
                "Content-Type: text/plain\r\n\r\n",
                "This is the plaintext version.\r\n",
                "--myboundary\r\n",
                "Content-Type: text/html;chARset=utf-8\r\n\r\n",
                "This is the <b>HTML</b> version with fake --MYBOUNDARY.\r\n",
                "--myboundary--"
            )
            .as_bytes(),
        )
        .unwrap();
        assert_eq!(mail.headers.len(), 1);
        assert_eq!(mail.headers[0].get_key(), "Content-Type");
        assert_eq!(mail.headers[0].get_key_ref(), "Content-Type");
        assert_eq!(mail.ctype.mimetype, "multipart/alternative");
        assert_eq!(mail.ctype.charset, "us-ascii");
        assert_eq!(mail.ctype.params.get("boundary").unwrap(), "myboundary");
        assert_eq!(mail.subparts.len(), 2);
        assert_eq!(mail.subparts[0].headers.len(), 1);
        assert_eq!(mail.subparts[0].ctype.mimetype, "text/plain");
        assert_eq!(mail.subparts[0].ctype.charset, "us-ascii");
        assert_eq!(mail.subparts[0].ctype.params.get("boundary"), None);
        assert_eq!(mail.subparts[1].ctype.mimetype, "text/html");
        assert_eq!(mail.subparts[1].ctype.charset, "utf-8");
        assert_eq!(mail.subparts[1].ctype.params.get("boundary"), None);

        let mail =
            parse_mail(b"Content-Transfer-Encoding: base64\r\n\r\naGVsbG 8gd\r\n29ybGQ=").unwrap();
        assert_eq!(mail.get_body_raw().unwrap(), b"hello world");
        assert_eq!(mail.get_body().unwrap(), "hello world");

        let mail =
            parse_mail(b"Content-Type: text/plain; charset=x-unknown\r\n\r\nhello world").unwrap();
        assert_eq!(mail.get_body_raw().unwrap(), b"hello world");
        assert_eq!(mail.get_body().unwrap(), "hello world");

        let mail = parse_mail(b"ConTENT-tyPE: text/html\r\n\r\nhello world").unwrap();
        assert_eq!(mail.ctype.mimetype, "text/html");
        assert_eq!(mail.get_body_raw().unwrap(), b"hello world");
        assert_eq!(mail.get_body().unwrap(), "hello world");

        let mail = parse_mail(
            b"Content-Type: text/plain; charset=UTF-7\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\n+JgM-",
        ).unwrap();
        assert_eq!(mail.get_body_raw().unwrap(), b"+JgM-");
        assert_eq!(mail.get_body().unwrap(), "\u{2603}");

        let mail = parse_mail(b"Content-Type: text/plain; charset=UTF-7\r\n\r\n+JgM-").unwrap();
        assert_eq!(mail.get_body_raw().unwrap(), b"+JgM-");
        assert_eq!(mail.get_body().unwrap(), "\u{2603}");
    }

    #[test]
    fn test_missing_terminating_boundary() {
        let mail = parse_mail(
            concat!(
                "Content-Type: multipart/alternative; boundary=myboundary\r\n\r\n",
                "--myboundary\r\n",
                "Content-Type: text/plain\r\n\r\n",
                "part0\r\n",
                "--myboundary\r\n",
                "Content-Type: text/html\r\n\r\n",
                "part1\r\n"
            )
            .as_bytes(),
        )
        .unwrap();
        assert_eq!(mail.subparts[0].get_body().unwrap(), "part0\r\n");
        assert_eq!(mail.subparts[1].get_body().unwrap(), "part1\r\n");
    }

    #[test]
    fn test_missing_body() {
        let parsed =
            parse_mail("Content-Type: multipart/related; boundary=\"----=_\"\n".as_bytes())
                .unwrap();
        assert_eq!(parsed.headers[0].get_key(), "Content-Type");
        assert_eq!(parsed.get_body_raw().unwrap(), b"");
        assert_eq!(parsed.get_body().unwrap(), "");
    }

    #[test]
    fn test_no_headers_in_subpart() {
        let mail = parse_mail(
            concat!(
                "Content-Type: multipart/report; report-type=delivery-status;\n",
                "\tboundary=\"1404630116.22555.postech.q0.x.x.x\"\n",
                "\n",
                "--1404630116.22555.postech.q0.x.x.x\n",
                "\n",
                "--1404630116.22555.postech.q0.x.x.x--\n"
            )
            .as_bytes(),
        )
        .unwrap();
        assert_eq!(mail.ctype.mimetype, "multipart/report");
        assert_eq!(mail.subparts[0].headers.len(), 0);
        assert_eq!(mail.subparts[0].ctype.mimetype, "text/plain");
        assert_eq!(mail.subparts[0].get_body_raw().unwrap(), b"");
        assert_eq!(mail.subparts[0].get_body().unwrap(), "");
    }

    #[test]
    fn test_empty() {
        let mail = parse_mail("".as_bytes()).unwrap();
        assert_eq!(mail.get_body_raw().unwrap(), b"");
        assert_eq!(mail.get_body().unwrap(), "");
    }

    #[test]
    fn test_dont_panic_for_value_with_new_lines() {
        let parsed = parse_param_content(r#"Content-Type: application/octet-stream; name=""#);
        assert_eq!(parsed.params["name"], "\"");
    }

    #[test]
    fn test_default_content_encoding() {
        let mail = parse_mail(b"Content-Type: text/plain; charset=UTF-7\r\n\r\n+JgM-").unwrap();
        let body = mail.get_body_encoded();
        match body {
            Body::SevenBit(body) => {
                assert_eq!(body.get_raw(), b"+JgM-");
                assert_eq!(body.get_as_string().unwrap(), "\u{2603}");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_7bit_content_encoding() {
        let mail = parse_mail(b"Content-Type: text/plain; charset=UTF-7\r\nContent-Transfer-Encoding: 7bit\r\n\r\n+JgM-").unwrap();
        let body = mail.get_body_encoded();
        match body {
            Body::SevenBit(body) => {
                assert_eq!(body.get_raw(), b"+JgM-");
                assert_eq!(body.get_as_string().unwrap(), "\u{2603}");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_8bit_content_encoding() {
        let mail = parse_mail(b"Content-Type: text/plain; charset=UTF-7\r\nContent-Transfer-Encoding: 8bit\r\n\r\n+JgM-").unwrap();
        let body = mail.get_body_encoded();
        match body {
            Body::EightBit(body) => {
                assert_eq!(body.get_raw(), b"+JgM-");
                assert_eq!(body.get_as_string().unwrap(), "\u{2603}");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_quoted_printable_content_encoding() {
        let mail = parse_mail(
            b"Content-Type: text/plain; charset=UTF-7\r\nContent-Transfer-Encoding: quoted-printable\r\n\r\n+JgM-",
        ).unwrap();
        match mail.get_body_encoded() {
            Body::QuotedPrintable(body) => {
                assert_eq!(body.get_raw(), b"+JgM-");
                assert_eq!(body.get_decoded().unwrap(), b"+JgM-");
                assert_eq!(body.get_decoded_as_string().unwrap(), "\u{2603}");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_base64_content_encoding() {
        let mail =
            parse_mail(b"Content-Transfer-Encoding: base64\r\n\r\naGVsbG 8gd\r\n29ybGQ=").unwrap();
        match mail.get_body_encoded() {
            Body::Base64(body) => {
                assert_eq!(body.get_raw(), b"aGVsbG 8gd\r\n29ybGQ=");
                assert_eq!(body.get_decoded().unwrap(), b"hello world");
                assert_eq!(body.get_decoded_as_string().unwrap(), "hello world");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_binary_content_encoding() {
        let mail = parse_mail(b"Content-Transfer-Encoding: binary\r\n\r\n######").unwrap();
        let body = mail.get_body_encoded();
        match body {
            Body::Binary(body) => {
                assert_eq!(body.get_raw(), b"######");
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_body_content_encoding_with_multipart() {
        let mail_filepath = "./tests/files/test_email_01.txt";
        let mail = std::fs::read(mail_filepath)
            .expect(&format!("Unable to open the file [{}]", mail_filepath));
        let mail = parse_mail(&mail).unwrap();

        let subpart_0 = mail.subparts.get(0).unwrap();
        match subpart_0.get_body_encoded() {
            Body::SevenBit(body) => {
                assert_eq!(
                    body.get_as_string().unwrap().trim(),
                    "<html>Test with attachments</html>"
                );
            }
            _ => assert!(false),
        };

        let subpart_1 = mail.subparts.get(1).unwrap();
        match subpart_1.get_body_encoded() {
            Body::Base64(body) => {
                let pdf_filepath = "./tests/files/test_email_01_sample.pdf";
                let original_pdf = std::fs::read(pdf_filepath)
                    .expect(&format!("Unable to open the file [{}]", pdf_filepath));
                assert_eq!(body.get_decoded().unwrap(), original_pdf);
            }
            _ => assert!(false),
        };

        let subpart_2 = mail.subparts.get(2).unwrap();
        match subpart_2.get_body_encoded() {
            Body::Base64(body) => {
                assert_eq!(
                    body.get_decoded_as_string().unwrap(),
                    "txt file context for email collector\n1234567890987654321\n"
                );
            }
            _ => assert!(false),
        };
    }

    #[test]
    fn test_fuzzer_testcase() {
        const INPUT: &'static str = "U3ViamVjdDplcy1UeXBlOiBtdW50ZW50LVV5cGU6IW11bAAAAAAAAAAAamVjdDplcy1UeXBlOiBtdW50ZW50LVV5cGU6IG11bAAAAAAAAAAAAAAAAABTTUFZdWJqZf86OiP/dCBTdWJqZWN0Ol8KRGF0ZTog/////////////////////wAAAAAAAAAAAHQgYnJmAHQgYnJmZXItRW5jeXBlOnY9NmU3OjA2OgAAAAAAAAAAAAAAADEAAAAAAP/8mAAAAAAAAAAA+f///wAAAAAAAP8AAAAAAAAAAAAAAAAAAAAAAAAAPT0/PzEAAAEAAA==";

        if let Ok(parsed) = parse_mail(&base64::decode(INPUT).unwrap()) {
            if let Some(date) = parsed.headers.get_first_value("Date") {
                let _ = dateparse(&date);
            }
        }
    }

    #[test]
    fn test_fuzzer_testcase_2() {
        const INPUT: &'static str = "U3ViamVjdDogVGhpcyBpcyBhIHRlc3QgZW1haWwKQ29udGVudC1UeXBlOiBtdWx0aXBhcnQvYWx0ZXJuYXRpdmU7IGJvdW5kYXJ5PczMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMzMZm9vYmFyCkRhdGU6IFN1biwgMDIgT2MKCi1TdWJqZWMtZm9vYmFydDo=";
        if let Ok(parsed) = parse_mail(&base64::decode(INPUT).unwrap()) {
            if let Some(date) = parsed.headers.get_first_value("Date") {
                let _ = dateparse(&date);
            }
        }
    }
}
