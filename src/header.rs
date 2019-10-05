use charset::decode_latin1;
use charset::Charset;

use error::*;
use util::is_boundary;
use util::find_from;

/// A struct that represents a single header in the message.
/// It holds slices into the raw byte array passed to parse_mail, and so the
/// lifetime of this struct must be contained within the lifetime of the raw
/// input. There are additional accessor functions on this struct to extract
/// the data as Rust strings.
#[derive(Debug)]
pub struct MailHeader<'a> {
    key: &'a [u8],
    value: &'a [u8],
}

impl<'a> MailHeader<'a> {
    /// Get the name of the header. Note that header names are case-insensitive.
    pub fn get_key(&self) -> Result<String, MailParseError> {
        Ok(decode_latin1(self.key).into_owned())
    }

    fn decode_word(&self, encoded: &str) -> Option<String> {
        let ix_delim1 = encoded.find('?')?;
        let ix_delim2 = find_from(encoded, ix_delim1 + 1, "?")?;

        let charset = &encoded[0..ix_delim1];
        let transfer_coding = &encoded[ix_delim1 + 1..ix_delim2];
        let input = &encoded[ix_delim2 + 1..];

        let decoded = match transfer_coding {
            "B" | "b" => base64::decode(input.as_bytes()).ok()?,
            "Q" | "q" => {
                // The quoted_printable module does a trim_end on the input, so if
                // that affects the output we should save and restore the trailing
                // whitespace
                let to_decode = input.replace("_", " ");
                let trimmed = to_decode.trim_end();
                let mut d = quoted_printable::decode(&trimmed, quoted_printable::ParseMode::Robust);
                if d.is_ok() && to_decode.len() != trimmed.len() {
                    d.as_mut()
                        .unwrap()
                        .extend_from_slice(to_decode[trimmed.len()..].as_bytes());
                }
                d.ok()?
            }
            _ => return None,
        };
        let charset = Charset::for_label_no_replacement(charset.as_bytes())?;
        let (cow, _) = charset.decode_without_bom_handling(&decoded);
        Some(cow.into_owned())
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
    ///     assert_eq!(parsed.get_key().unwrap(), "Subject");
    ///     assert_eq!(parsed.get_value().unwrap(), "\u{a1}Hola, se\u{f1}or!");
    /// ```
    pub fn get_value(&self) -> Result<String, MailParseError> {
        let mut result = String::new();
        let chars = decode_latin1(self.value);
        let mut lines = chars.lines();
        let mut add_space = false;
        while let Some(line) = lines.next().map(str::trim_start) {
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
                                        Some(v) => {
                                            result.push_str(&v);
                                            add_space = false;
                                        }
                                        None => result.push_str(&line[ix_begin - 2..ix_end + 2]),
                                    };
                                    ix_search = ix_end;
                                }
                                None => {
                                    result.push_str(&"=?");
                                    ix_search = ix_begin - 2;
                                }
                            };
                            break;
                        }
                        ix_search += 2;
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
///     assert_eq!(parsed.get_key().unwrap(), "Subject");
///     assert_eq!(parsed.get_value().unwrap(), "Hello, sir, I am multiline");
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
                key: &raw_data[0..v],
                value: &raw_data[ix_value_start..ix_value_end],
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
    ///     assert_eq!(headers.get_first_value("Subject").unwrap(), Some("Test".to_string()));
    /// ```
    fn get_first_value(&self, key: &str) -> Result<Option<String>, MailParseError>;

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
    ///     assert_eq!(headers.get_all_values("Key").unwrap(),
    ///         vec!["Value1".to_string(), "Value2".to_string()]);
    /// ```
    fn get_all_values(&self, key: &str) -> Result<Vec<String>, MailParseError>;
}

impl<'a> MailHeaderMap for [MailHeader<'a>] {
    fn get_first_value(&self, key: &str) -> Result<Option<String>, MailParseError> {
        for x in self {
            if x.get_key()?.eq_ignore_ascii_case(key) {
                return x.get_value().map(Some);
            }
        }
        Ok(None)
    }

    fn get_all_values(&self, key: &str) -> Result<Vec<String>, MailParseError> {
        let mut values: Vec<String> = Vec::new();
        for x in self {
            if x.get_key()?.eq_ignore_ascii_case(key) {
                values.push(x.get_value()?);
            }
        }
        Ok(values)
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
///     assert_eq!(headers[1].get_key().unwrap(), "From");
///     assert_eq!(headers.get_first_value("To").unwrap(), Some("you@yourself.com".to_string()));
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

