use crate::{MailHeader, MailHeaderMap};
use std::fmt;
use std::slice;

/// A struct that wrapps the header portion of a message and provides
/// utility functions to look up specific headers.
pub struct Headers<'a> {
    raw_bytes: &'a [u8],
    headers: &'a [MailHeader<'a>],
}

impl<'a> Headers<'a> {
    pub(crate) fn new(raw_bytes: &'a [u8], headers: &'a [MailHeader<'a>]) -> Headers<'a> {
        Headers { raw_bytes, headers }
    }

    /// Returns the raw, unparsed bytes that make up the header block of
    /// the message. This includes everything up to and including the empty
    /// line at the end of the header block.
    ///
    /// # Examples
    /// ```
    ///     use mailparse::{parse_mail, headers::Headers};
    ///     let mail = parse_mail(concat!(
    ///             "SubJECT : foo\n",
    ///             "\n",
    ///             "Body starts here").as_bytes())
    ///         .unwrap();
    ///     assert_eq!(mail.get_headers().get_raw_bytes(), b"SubJECT : foo\n\n");
    pub fn get_raw_bytes(&self) -> &'a [u8] {
        self.raw_bytes
    }
}

/// Allows iterating over the individual `MailHeader` items in this block of
/// headers.
///
/// # Examples
/// ```
///     use mailparse::{parse_mail, headers::Headers};
///     let mail = parse_mail(concat!(
///             "Subject: foo\n",
///             "Another header: bar\n",
///             "\n",
///             "Body starts here").as_bytes())
///         .unwrap();
///     let mut iter = mail.get_headers().into_iter();
///     assert_eq!(iter.next().unwrap().get_key(), "Subject");
///     assert_eq!(iter.next().unwrap().get_key(), "Another header");
/// ```
impl<'a> IntoIterator for Headers<'a> {
    type Item = &'a MailHeader<'a>;
    type IntoIter = slice::Iter<'a, MailHeader<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.headers.into_iter()
    }
}

/// Allows formatting and printing the `Headers` struct items.
///
/// # Examples
/// ```
///     use mailparse::parse_mail;
///     let mail = parse_mail(concat!(
///             "Subject: foo\n",
///             "Another header: bar\n",
///             "\n",
///             "Body starts here").as_bytes())
///         .unwrap();
///     let mut headers = mail.get_headers();
///     assert_eq!(format!("{:?}", headers), "Headers { \
///                headers: [MailHeader { key: \"Subject\", value: \"foo\" }, \
///                MailHeader { key: \"Another header\", value: \"bar\" }] }");
/// ```
impl<'a> fmt::Debug for Headers<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Headers")
            .field("headers", &self.headers)
            .finish()
    }
}

impl<'a> MailHeaderMap for Headers<'a> {
    /// # Examples
    /// ```
    ///     use mailparse::{parse_mail, MailHeaderMap, headers::Headers};
    ///     let mail = parse_mail(concat!(
    ///             "Subject: Test\n",
    ///             "\n",
    ///             "This is a test message").as_bytes())
    ///         .unwrap();
    ///     assert_eq!(mail.get_headers().get_first_value("Subject"), Some("Test".to_string()));
    /// ```
    fn get_first_value(&self, key: &str) -> Option<String> {
        self.headers.get_first_value(key)
    }

    fn get_first_header(&self, key: &str) -> Option<&MailHeader> {
        self.headers.get_first_header(key)
    }

    /// # Examples
    /// ```
    ///     use mailparse::{parse_mail, MailHeaderMap, headers::Headers};
    ///     let mail = parse_mail(concat!(
    ///             "Key: Value1\n",
    ///             "Key: Value2").as_bytes())
    ///         .unwrap();
    ///     assert_eq!(mail.get_headers().get_all_values("Key"),
    ///         vec!["Value1".to_string(), "Value2".to_string()]);
    /// ```
    fn get_all_values(&self, key: &str) -> Vec<String> {
        self.headers.get_all_values(key)
    }

    fn get_all_headers(&self, key: &str) -> Vec<&MailHeader> {
        self.headers.get_all_headers(key)
    }
}
