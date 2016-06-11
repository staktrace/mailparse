extern crate base64;
extern crate encoding;
extern crate quoted_printable;

use std::error;
use std::fmt;

use encoding::Encoding;

#[derive(Debug)]
pub enum MailParseError {
    QuotedPrintableDecodeError(quoted_printable::QuotedPrintableError),
    Base64DecodeError(base64::Base64Error),
    Generic(String, usize),
}

impl fmt::Display for MailParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MailParseError::QuotedPrintableDecodeError(ref err) => {
                write!(f, "QuotedPrintable decode error: {}", err)
            }
            MailParseError::Base64DecodeError(ref err) => write!(f, "Base64 decode error: {}", err),
            MailParseError::Generic(ref description, ref position) => {
                write!(f, "{} (offset {})", description, position)
            }
        }
    }
}

impl error::Error for MailParseError {
    fn description(&self) -> &str {
        match *self {
            MailParseError::QuotedPrintableDecodeError(ref err) => err.description(),
            MailParseError::Base64DecodeError(ref err) => err.description(),
            _ => "An error occurred while attempting to parse the input",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
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

impl From<base64::Base64Error> for MailParseError {
    fn from(err: base64::Base64Error) -> MailParseError {
        MailParseError::Base64DecodeError(err)
    }
}

impl From<std::borrow::Cow<'static, str>> for MailParseError {
    fn from(err: std::borrow::Cow<'static, str>) -> MailParseError {
        MailParseError::Generic(err.into_owned().to_string(), 0)
    }
}

#[derive(Debug)]
pub struct MailHeader<'a> {
    key: &'a [u8],
    value: &'a [u8],
}

fn is_boundary(line: &str, ix: Option<usize>) -> bool {
    ix.map_or_else(|| true,
                   |v| v >= line.len() || line.chars().nth(v).unwrap().is_whitespace())
}

fn find_from(line: &str, ix_start: usize, key: &str) -> Option<usize> {
    line[ix_start..].find(key).map(|v| ix_start + v)
}

fn find_from_u8(line: &[u8], ix_start: usize, key: &[u8]) -> Option<usize> {
    assert!(key.len() > 0);
    assert!(ix_start < line.len());
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

impl<'a> MailHeader<'a> {
    pub fn get_key(&self) -> Result<String, MailParseError> {
        Ok(try!(encoding::all::ISO_8859_1.decode(self.key, encoding::DecoderTrap::Strict))
            .trim()
            .to_string())
    }

    fn decode_word(&self, encoded: &str) -> Result<String, MailParseError> {
        let ix_delim1 = try!(encoded.find("?")
            .ok_or(MailParseError::Generic("Unable to find '?' inside encoded-word".to_string(),
                                           0)));
        let ix_delim2 = try!(find_from(encoded, ix_delim1 + 1, "?")
            .ok_or(MailParseError::Generic("Unable to find second '?' inside encoded-word"
                                               .to_string(),
                                           ix_delim1 + 1)));

        let charset = &encoded[0..ix_delim1];
        let transfer_coding = &encoded[ix_delim1 + 1..ix_delim2];
        let input = &encoded[ix_delim2 + 1..];

        let decoded = match transfer_coding {
            "B" => try!(base64::u8de(input.as_bytes())),
            "Q" => {
                try!(quoted_printable::decode(&input.replace("_", " "),
                                              quoted_printable::ParseMode::Robust))
            }
            _ => {
                return Err(MailParseError::Generic("Unknown transfer-coding name found in \
                                                    encoded-word"
                                                       .to_string(),
                                                   ix_delim1 + 1))
            }
        };
        let charset_conv = try!(encoding::label::encoding_from_whatwg_label(charset)
            .ok_or(MailParseError::Generic("Unknown charset found in encoded-word".to_string(),
                                           0)));
        charset_conv.decode(&decoded, encoding::DecoderTrap::Replace).map_err(|_| {
            MailParseError::Generic("Unable to convert transfer-decoded bytes from specified \
                                     charset"
                                        .to_string(),
                                    0)
        })
    }

    pub fn get_value(&self) -> Result<String, MailParseError> {
        let mut result = String::new();
        let chars =
            try!(encoding::all::ISO_8859_1.decode(self.value, encoding::DecoderTrap::Strict));
        let mut lines = chars.lines();
        let mut add_space = false;
        loop {
            let line = match lines.next() {
                Some(v) => v.trim_left(),
                None => break,
            };

            if add_space {
                result.push(' ');
            }
            add_space = true;

            let mut ix_search = 0;
            loop {
                match find_from(line, ix_search, "=?") {
                    Some(v) => {
                        let ix_begin = v + 2;
                        if !is_boundary(line, ix_begin.checked_sub(3)) {
                            result.push_str(&line[ix_search..ix_begin]);
                            ix_search = ix_begin;
                            continue;
                        }
                        result.push_str(&line[ix_search..ix_begin - 2]);
                        let mut ix_end_search = ix_begin;
                        loop {
                            match find_from(line, ix_end_search, "?=") {
                                Some(ix_end) => {
                                    if !is_boundary(line, ix_end.checked_add(2)) {
                                        ix_end_search = ix_end + 2;
                                        continue;
                                    }
                                    match self.decode_word(&line[ix_begin..ix_end]) {
                                        Ok(v) => {
                                            result.push_str(&v);
                                        }
                                        Err(_) => result.push_str(&line[ix_begin - 2..ix_end + 2]),
                                    };
                                    ix_search = ix_end;
                                }
                                None => {
                                    result.push_str(&"=?");
                                }
                            };
                            break;
                        }
                        ix_search = ix_search + 2;
                        continue;
                    }
                    None => {
                        result.push_str(&line[ix_search..]);
                        break;
                    }
                };
            }
        }
        Ok(result)
    }
}

enum HeaderParseState {
    Initial,
    Key,
    PreValue,
    Value,
    ValueNewline,
}

pub fn parse_header(raw_data: &[u8]) -> Result<(MailHeader, usize), MailParseError> {
    let mut it = raw_data.iter();
    let mut ix = 0;
    let mut c = match it.next() {
        None => return Err(MailParseError::Generic("Empty string provided".to_string(), 0)),
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
                    return Err(MailParseError::Generic("Header cannot start with a space; it is \
                                                        likely an overhanging line from a \
                                                        previous header"
                                                           .to_string(),
                                                       ix));
                };
                state = HeaderParseState::Key;
                continue;
            }
            HeaderParseState::Key => {
                if c == b':' {
                    ix_key_end = Some(ix);
                    state = HeaderParseState::PreValue;
                } else if c == b'\n' {
                    return Err(MailParseError::Generic("Unexpected newline in header key"
                                                           .to_string(),
                                                       ix));
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
        ix = ix + 1;
        c = match it.next() {
            None => break,
            Some(v) => *v,
        };
    }
    match ix_key_end {
        Some(v) => {
            Ok((MailHeader {
                key: &raw_data[0..v],
                value: &raw_data[ix_value_start..ix_value_end],
            },
                ix))
        }

        None => {
            Err(MailParseError::Generic("Unable to determine end of the header key component"
                                            .to_string(),
                                        ix))
        }
    }
}

pub trait MailHeaderMap {
    fn get_first_value(&self, key: &str) -> Result<Option<String>, MailParseError>;
    fn get_all_values(&self, key: &str) -> Result<Vec<String>, MailParseError>;
}

impl<'a> MailHeaderMap for Vec<MailHeader<'a>> {
    fn get_first_value(&self, key: &str) -> Result<Option<String>, MailParseError> {
        for x in self {
            if try!(x.get_key()) == key {
                return x.get_value().map(|v| Some(v));
            }
        }
        Ok(None)
    }

    fn get_all_values(&self, key: &str) -> Result<Vec<String>, MailParseError> {
        let mut values: Vec<String> = Vec::new();
        for x in self {
            if try!(x.get_key()) == key {
                values.push(try!(x.get_value()));
            }
        }
        Ok(values)
    }
}

pub fn parse_headers(raw_data: &[u8]) -> Result<(Vec<MailHeader>, usize), MailParseError> {
    let mut headers: Vec<MailHeader> = Vec::new();
    let mut ix = 0;
    loop {
        let (header, ix_next) = try!(parse_header(&raw_data[ix..]).map_err(|e| {
            match e {
                MailParseError::Generic(ref description, ref position) => {
                    MailParseError::Generic(description.clone(), position + ix)
                }
                err => err,
            }
        }));
        headers.push(header);
        ix = ix + ix_next;
        if ix >= raw_data.len() || raw_data[ix] == b'\n' {
            break;
        }
    }
    Ok((headers, ix))
}

#[derive(Debug)]
pub struct ParsedContentType {
    pub mimetype: String,
    pub charset: String,
    pub boundary: Option<String>,
}

pub fn parse_content_type(header: &str) -> Result<ParsedContentType, MailParseError> {
    let mut parsed_type = ParsedContentType{
        mimetype: "text/plain".to_string(),
        charset: "us-ascii".to_string(),
        boundary: None
    };
    let mut tokens = header.split(';');
    // There must be at least one token produced by split, even if it's empty.
    parsed_type.mimetype = String::from(tokens.next().unwrap().trim());
    while let Some(param) = tokens.next() {
        if let Some(ix_eq) = param.find('=') {
            let attr = param[0..ix_eq].trim();
            let mut value = param[ix_eq+1..].trim();
            if value.starts_with('"') && value.ends_with('"') {
                value = &value[1..value.len() - 1];
            }
            if attr == "charset" {
                parsed_type.charset = String::from(value);
            } else if attr == "boundary" {
                parsed_type.boundary = Some(String::from(value));
            }
        } else {
            return Err(MailParseError::Generic("Content-Type parameter did not have an '=' character".to_string(), 0));
        }
    }
    Ok(parsed_type)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_match {
        ( $x:expr, $p:pat ) => {
            match $x {
                $p => (),
                _ => panic!("Expression {} does not match given pattern", $x),
            }
        }
    }

    #[test]
    fn parse_basic_header() {
        let (parsed, _) = parse_header(b"Key: Value").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.get_key().unwrap(), "Key");
        assert_eq!(parsed.value, b"Value");
        assert_eq!(parsed.get_value().unwrap(), "Value");

        let (parsed, _) = parse_header(b"Key :  Value ").unwrap();
        assert_eq!(parsed.key, b"Key ");
        assert_eq!(parsed.value, b"Value ");
        assert_eq!(parsed.get_value().unwrap(), "Value ");

        let (parsed, _) = parse_header(b"Key:").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"");

        let (parsed, _) = parse_header(b":\n").unwrap();
        assert_eq!(parsed.key, b"");
        assert_eq!(parsed.value, b"");

        let (parsed, _) = parse_header(b"Key:Multi-line\n value").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"Multi-line\n value");
        assert_eq!(parsed.get_value().unwrap(), "Multi-line value");

        let (parsed, _) = parse_header(b"Key:  Multi\n  line\n value\n").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"Multi\n  line\n value");
        assert_eq!(parsed.get_value().unwrap(), "Multi line value");

        let (parsed, _) = parse_header(b"Key: One\nKey2: Two").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"One");

        let (parsed, _) = parse_header(b"Key: One\n\tOverhang").unwrap();
        assert_eq!(parsed.key, b"Key");
        assert_eq!(parsed.value, b"One\n\tOverhang");
        assert_eq!(parsed.get_value().unwrap(), "One Overhang");

        let (parsed, _) = parse_header(b"SPAM: VIAGRA \xAE").unwrap();
        assert_eq!(parsed.key, b"SPAM");
        assert_eq!(parsed.value, b"VIAGRA \xAE");
        assert_eq!(parsed.get_value().unwrap(), "VIAGRA \u{ae}");

        parse_header(b" Leading: Space").unwrap_err();
        parse_header(b"Just a string").unwrap_err();
        parse_header(b"Key\nBroken: Value").unwrap_err();
    }

    #[test]
    fn parse_encoded_headers() {
        let (parsed, _) = parse_header(b"Subject: =?iso-8859-1?Q?=A1Hola,_se=F1or!?=").unwrap();
        assert_eq!(parsed.get_key().unwrap(), "Subject");
        assert_eq!(parsed.get_value().unwrap(), "\u{a1}Hola, se\u{f1}or!");

        let (parsed, _) = parse_header(b"Subject: =?iso-8859-1?Q?=A1Hola,?=\n \
                                        =?iso-8859-1?Q?_se=F1or!?=")
            .unwrap();
        assert_eq!(parsed.get_key().unwrap(), "Subject");
        assert_eq!(parsed.get_value().unwrap(), "\u{a1}Hola,  se\u{f1}or!");

        let (parsed, _) = parse_header(b"Euro: =?utf-8?Q?=E2=82=AC?=").unwrap();
        assert_eq!(parsed.get_key().unwrap(), "Euro");
        assert_eq!(parsed.get_value().unwrap(), "\u{20ac}");

        let (parsed, _) = parse_header(b"HelloWorld: =?utf-8?B?aGVsbG8gd29ybGQ=?=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "hello world");

        let (parsed, _) = parse_header(b"Empty: =?utf-8?Q??=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "");

        let (parsed, _) = parse_header(b"Incomplete: =?").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "=?");

        let (parsed, _) = parse_header(b"BadEncoding: =?garbage?Q??=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "=?garbage?Q??=");

        let (parsed, _) = parse_header(b"Invalid: =?utf-8?Q?=E2=AC?=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "\u{fffd}");

        let (parsed, _) = parse_header(b"LineBreak: =?utf-8?Q?=E2=82\n =AC?=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "=?utf-8?Q?=E2=82 =AC?=");

        let (parsed, _) = parse_header(b"NotSeparateWord: hello=?utf-8?Q?world?=").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "hello=?utf-8?Q?world?=");

        let (parsed, _) = parse_header(b"NotSeparateWord2: =?utf-8?Q?hello?=world").unwrap();
        assert_eq!(parsed.get_value().unwrap(), "=?utf-8?Q?hello?=world");
    }

    #[test]
    fn parse_multiple_headers() {
        let (parsed, _) = parse_headers(b"Key: Value\nTwo: Second").unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0].key, b"Key");
        assert_eq!(parsed[0].value, b"Value");
        assert_eq!(parsed[1].key, b"Two");
        assert_eq!(parsed[1].value, b"Second");

        let (parsed, _) = parse_headers(b"Key: Value\n Overhang\nTwo: Second\nThree: Third")
            .unwrap();
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

        let (parsed, _) = parse_headers(b"Return-Path: <kats@foobar.staktrace.com>\nX-Original-To: \
                           kats@baz.staktrace.com\nDelivered-To: \
                           kats@baz.staktrace.com\nReceived: from foobar.staktrace.com \
                           (localhost [127.0.0.1])\n    by foobar.staktrace.com (Postfix) with \
                           ESMTP id 139F711C1C34\n    for <kats@baz.staktrace.com>; Fri, 27 May \
                           2016 02:34:26 -0400 (EDT)\nDate: Fri, 27 May 2016 02:34:25 -0400\nTo: \
                           kats@baz.staktrace.com\nFrom: kats@foobar.staktrace.com\nSubject: \
                           test Fri, 27 May 2016 02:34:25 -0400\nX-Mailer: swaks v20130209.0 \
                           jetmore.org/john/code/swaks/\nMessage-Id: \
                           <20160527063426.139F711C1C34@foobar.staktrace.com>\n\nThis is a test \
                           mailing\n")
            .unwrap();
        assert_eq!(parsed.len(), 10);
        assert_eq!(parsed[0].key, b"Return-Path");
        assert_eq!(parsed[9].key, b"Message-Id");

        let (parsed, _) =
            parse_headers(b"Key: Value\nAnotherKey: AnotherValue\nKey: Value2\nKey: Value3\n")
                .unwrap();
        assert_eq!(parsed.len(), 4);
        assert_eq!(parsed.get_first_value("Key").unwrap(),
                   Some("Value".to_string()));
        assert_eq!(parsed.get_all_values("Key").unwrap(),
                   vec!["Value", "Value2", "Value3"]);
        assert_eq!(parsed.get_first_value("AnotherKey").unwrap(),
                   Some("AnotherValue".to_string()));
        assert_eq!(parsed.get_all_values("AnotherKey").unwrap(),
                   vec!["AnotherValue"]);
        assert_eq!(parsed.get_first_value("NoKey").unwrap(), None);
        assert_eq!(parsed.get_all_values("NoKey").unwrap(),
                   Vec::<String>::new());

        assert_match!(parse_headers(b"Bad\nKey").unwrap_err(), MailParseError::Generic(_, 3));
        assert_match!(parse_headers(b"K:V\nBad\nKey").unwrap_err(), MailParseError::Generic(_, 7));
    }

    #[test]
    fn test_parse_content_type() {
        let ctype = parse_content_type("text/html; charset=utf-8").unwrap();
        assert_eq!(ctype.mimetype, "text/html");
        assert_eq!(ctype.charset, "utf-8");
        assert_eq!(ctype.boundary, None);

        let ctype = parse_content_type(" foo/bar; x=y; charset=\"fake\" ; x2=y2").unwrap();
        assert_eq!(ctype.mimetype, "foo/bar");
        assert_eq!(ctype.charset, "fake");
        assert_eq!(ctype.boundary, None);

        let ctype = parse_content_type(" multipart/bar; boundary=foo ").unwrap();
        assert_eq!(ctype.mimetype, "multipart/bar");
        assert_eq!(ctype.charset, "us-ascii");
        assert_eq!(ctype.boundary.unwrap(), "foo");
    }
}
