use std::fmt;

use crate::header::HeaderToken;
use crate::{MailHeader, MailParseError};

/// A representation of a single mailbox. Each mailbox has
/// a routing address `addr` and an optional display name.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SingleInfo {
    pub display_name: Option<String>,
    pub addr: String,
}

impl SingleInfo {
    fn new(name: Option<String>, addr: String) -> Result<Self, MailParseError> {
        if addr.contains('@') {
            Ok(SingleInfo {
                display_name: name,
                addr,
            })
        } else {
            Err(MailParseError::Generic(
                "Invalid address found: must contain a '@' symbol",
            ))
        }
    }
}

impl fmt::Display for SingleInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.display_name {
            write!(f, r#""{}" <{}>"#, name.replace('"', r#"\""#), self.addr)
        } else {
            write!(f, "{}", self.addr)
        }
    }
}

/// A representation of a group address. It has a name and
/// a list of mailboxes.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GroupInfo {
    pub group_name: String,
    pub addrs: Vec<SingleInfo>,
}

impl GroupInfo {
    fn new(name: String, addrs: Vec<SingleInfo>) -> Self {
        GroupInfo {
            group_name: name,
            addrs,
        }
    }
}

impl fmt::Display for GroupInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, r#""{}":"#, self.group_name.replace('"', r#"\""#))?;
        for (i, addr) in self.addrs.iter().enumerate() {
            if i == 0 {
                write!(f, " ")?;
            } else {
                write!(f, ", ")?;
            }
            addr.fmt(f)?;
        }
        write!(f, ";")
    }
}

/// An abstraction over the two different kinds of top-level addresses allowed
/// in email headers. Group addresses have a name and a list of mailboxes. Single
/// addresses are just a mailbox. Each mailbox consists of what you would consider
/// an email address (e.g. foo@bar.com) and optionally a display name ("Foo Bar").
/// Groups are represented in email headers with colons and semicolons, e.g.
///    To: my-peeps: foo@peeps.org, bar@peeps.org;
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum MailAddr {
    Group(GroupInfo),
    Single(SingleInfo),
}

#[derive(Debug)]
enum AddrParseState {
    Initial,
    QuotedName,
    EscapedChar,
    AfterQuotedName,
    BracketedAddr,
    AfterBracketedAddr,
    Unquoted,
    NameWithEncodedWord,
    Comment,
}

/// A simple wrapper around `Vec<MailAddr>`. This is primarily here so we can
/// implement the Display trait on it, and allow user code to easily convert
/// the return value from `addrparse` back into a string. However there are some
/// additional utility functions on this wrapper as well.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MailAddrList(Vec<MailAddr>);

impl std::ops::Deref for MailAddrList {
    type Target = Vec<MailAddr>;

    fn deref(&self) -> &Vec<MailAddr> {
        &self.0
    }
}

impl std::ops::DerefMut for MailAddrList {
    fn deref_mut(&mut self) -> &mut Vec<MailAddr> {
        &mut self.0
    }
}

impl fmt::Display for MailAddrList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut last_was_group = false;
        for (i, addr) in self.iter().enumerate() {
            if i > 0 {
                if last_was_group {
                    write!(f, " ")?;
                } else {
                    write!(f, ", ")?;
                }
            }
            match addr {
                MailAddr::Group(g) => {
                    g.fmt(f)?;
                    last_was_group = true;
                }
                MailAddr::Single(s) => {
                    s.fmt(f)?;
                    last_was_group = false;
                }
            }
        }
        Ok(())
    }
}

impl From<Vec<MailAddr>> for MailAddrList {
    fn from(addrs: Vec<MailAddr>) -> Self {
        MailAddrList(addrs)
    }
}

impl MailAddrList {
    /// Count the number of `SingleInfo` instances in this list of addresses.
    pub fn count_addrs(&self) -> usize {
        self.iter().fold(0, |acc, elem| match elem {
            MailAddr::Single(_) => acc + 1,
            MailAddr::Group(g) => acc + g.addrs.len(),
        })
    }

    /// Convenience function to check if this list of addresses contains exactly
    /// one `SingleInfo`, and if it does, to return it. If there is not exactly
    /// one `SingleInfo`, this function returns None.
    pub fn extract_single_info(self) -> Option<SingleInfo> {
        if self.len() == 1 {
            match &self[0] {
                MailAddr::Group(_) => None,
                MailAddr::Single(s) => Some(s.clone()),
            }
        } else {
            None
        }
    }

    /// Consumes the `MailAddrList`, returning the wrapped value.
    pub fn into_inner(self) -> Vec<MailAddr> {
        self.0
    }
}

enum HeaderTokenItem<'a> {
    Char(char),
    Whitespace(&'a str),
    Newline(String),
    DecodedWord(String),
}

struct HeaderTokenWalker<'a> {
    tokens: Vec<HeaderToken<'a>>,
    cur_token: usize,
    cur_char_offset: usize,
}

impl<'a> Iterator for HeaderTokenWalker<'a> {
    type Item = HeaderTokenItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.cur_token >= self.tokens.len() {
                return None;
            }
            match &self.tokens[self.cur_token] {
                HeaderToken::Text(s) => {
                    let s = &s[self.cur_char_offset..];
                    let mut chars = s.char_indices();
                    let c = chars.next();
                    if let Some((_, c)) = c {
                        self.cur_char_offset += chars.next().map(|(o, _)| o).unwrap_or(s.len());
                        return Some(HeaderTokenItem::Char(c));
                    } else {
                        self.cur_char_offset = 0;
                        self.cur_token += 1;
                        continue;
                    }
                }
                HeaderToken::Whitespace(ws) => {
                    self.cur_token += 1;
                    return Some(HeaderTokenItem::Whitespace(ws));
                }
                HeaderToken::Newline(Some(ws)) => {
                    self.cur_token += 1;
                    return Some(HeaderTokenItem::Newline(String::from(ws)));
                }
                HeaderToken::Newline(None) => {
                    panic!("Should never reach here");
                }
                HeaderToken::DecodedWord(word) => {
                    self.cur_token += 1;
                    return Some(HeaderTokenItem::DecodedWord(String::from(word)));
                }
            }
        }
    }
}

impl<'a> HeaderTokenWalker<'a> {
    fn new(tokens: Vec<HeaderToken<'a>>) -> Self {
        Self {
            tokens,
            cur_token: 0,
            cur_char_offset: 0,
        }
    }
}

/// Convert an address field from an email header into a structured type.
/// This function handles the most common formatting of to/from/cc/bcc fields
/// found in email headers. Note that if you are attempting to parse the
/// value of a `MailHeader`, it is better (both for correctness and performance
/// to use the `addrparse_header` function instead of this one. Correctness
/// is impacted because of the way encoded words within the header are
/// processed; using `MailHeader::get_value()` will decode encoded words,
/// which may then contain characters like commas that affect how `addrparse`
/// parses the value. This can produce incorrect results in some cases; using
/// `addrparse_header` will avoid this problem.
///
/// # Examples
/// ```
///     use mailparse::{addrparse, MailAddr, SingleInfo};
///     match &addrparse("John Doe <john@doe.com>").unwrap()[0] {
///         MailAddr::Single(info) => {
///             assert_eq!(info.display_name, Some("John Doe".to_string()));
///             assert_eq!(info.addr, "john@doe.com".to_string());
///         }
///         _ => panic!()
///     };
/// ```
pub fn addrparse(addrs: &str) -> Result<MailAddrList, MailParseError> {
    let v = vec![HeaderToken::Text(addrs)];
    let mut w = HeaderTokenWalker::new(v);
    addrparse_inner(&mut w, false)
}

/// Take a `MailHeader` that contains addresses in the value (e.g. from/to/cc/bcc)
/// and produce a structured type representing those addresses.
///
/// # Examples
/// ```
///     use mailparse::{addrparse_header, parse_mail, MailAddr, MailHeaderMap, SingleInfo};
///     let mail = parse_mail(b"From: John Doe <john@doe.com>\n\nBlah Blah").unwrap();
///     match &addrparse_header(mail.headers.get_first_header("From").unwrap()).unwrap()[0] {
///         MailAddr::Single(info) => {
///             assert_eq!(info.display_name, Some("John Doe".to_string()));
///             assert_eq!(info.addr, "john@doe.com".to_string());
///         }
///         _ => panic!()
///     };
/// ```
pub fn addrparse_header(header: &MailHeader) -> Result<MailAddrList, MailParseError> {
    let chars = header.decode_utf8_or_latin1();
    let v = crate::header::normalized_tokens(&chars);
    let mut w = HeaderTokenWalker::new(v);
    addrparse_inner(&mut w, false)
}

fn addrparse_inner(
    it: &mut HeaderTokenWalker,
    in_group: bool,
) -> Result<MailAddrList, MailParseError> {
    let mut result = vec![];
    let mut state = AddrParseState::Initial;

    let mut hti = match it.next() {
        None => return Ok(MailAddrList(vec![])),
        Some(v) => v,
    };

    let mut name = None;
    let mut addr = None;
    let mut post_quote_ws = None;
    let mut comment_return = None;

    loop {
        match state {
            AddrParseState::Initial => {
                match hti {
                    HeaderTokenItem::Char(c) => {
                        if c.is_whitespace() {
                            // continue in same state
                        } else if c == '"' {
                            state = AddrParseState::QuotedName;
                            name = Some(String::new());
                        } else if c == '<' {
                            state = AddrParseState::BracketedAddr;
                            addr = Some(String::new());
                        } else if c == ';' {
                            if !in_group {
                                return Err(MailParseError::Generic(
                                    "Unexpected group terminator found in initial list",
                                ));
                            }
                            return Ok(MailAddrList(result));
                        } else {
                            state = AddrParseState::Unquoted;
                            addr = Some(String::new());
                            addr.as_mut().unwrap().push(c);
                        }
                    }
                    HeaderTokenItem::Whitespace(_) => {
                        // continue in same state
                    }
                    HeaderTokenItem::Newline(_) => {
                        // continue in same state
                    }
                    HeaderTokenItem::DecodedWord(word) => {
                        state = AddrParseState::NameWithEncodedWord;
                        addr = Some(String::new());
                        addr.as_mut().unwrap().push_str(&word);
                    }
                }
            }
            AddrParseState::QuotedName => match hti {
                HeaderTokenItem::Char(c) => {
                    if c == '\\' {
                        state = AddrParseState::EscapedChar;
                    } else if c == '"' {
                        state = AddrParseState::AfterQuotedName;
                    } else {
                        name.as_mut().unwrap().push(c);
                    }
                }
                HeaderTokenItem::Whitespace(ws) => {
                    name.as_mut().unwrap().push_str(ws);
                }
                HeaderTokenItem::Newline(ws) => {
                    name.as_mut().unwrap().push_str(&ws);
                }
                HeaderTokenItem::DecodedWord(word) => {
                    name.as_mut().unwrap().push_str(&word);
                }
            },
            AddrParseState::EscapedChar => match hti {
                HeaderTokenItem::Char(c) => {
                    state = AddrParseState::QuotedName;
                    name.as_mut().unwrap().push(c);
                }
                HeaderTokenItem::Whitespace(ws) => {
                    state = AddrParseState::QuotedName;
                    name.as_mut().unwrap().push_str(ws);
                }
                HeaderTokenItem::Newline(ws) => {
                    state = AddrParseState::QuotedName;
                    name.as_mut().unwrap().push_str(&ws);
                }
                HeaderTokenItem::DecodedWord(_) => {
                    return Err(MailParseError::Generic(
                        "Unexpected encoded word found inside a quoted name",
                    ));
                }
            },
            AddrParseState::AfterQuotedName => {
                match hti {
                    HeaderTokenItem::Char(c) => {
                        if c.is_whitespace() {
                            if post_quote_ws.is_none() {
                                post_quote_ws = Some(String::new());
                            }
                            post_quote_ws.as_mut().unwrap().push(c);
                        } else if c == '<' {
                            state = AddrParseState::BracketedAddr;
                            addr = Some(String::new());
                        } else if c == ':' {
                            if in_group {
                                return Err(MailParseError::Generic(
                                    "Found unexpected nested group",
                                ));
                            }
                            let group_addrs = addrparse_inner(it, true)?;
                            state = AddrParseState::Initial;
                            result.push(MailAddr::Group(GroupInfo::new(
                                name.unwrap(),
                                group_addrs
                                    .0
                                    .into_iter()
                                    .map(|addr| match addr {
                                        MailAddr::Single(s) => s,
                                        MailAddr::Group(_) => {
                                            panic!("Unexpected nested group encountered")
                                        }
                                    })
                                    .collect(),
                            )));
                            name = None;
                        } else {
                            // I think technically not valid, but this occurs in real-world corpus, so
                            // handle gracefully
                            if c == '"' {
                                if let Some(ws) = post_quote_ws {
                                    name.as_mut().unwrap().push_str(&ws)
                                }
                                state = AddrParseState::QuotedName;
                            } else {
                                if let Some(ws) = post_quote_ws {
                                    name.as_mut().unwrap().push_str(&ws)
                                }
                                name.as_mut().unwrap().push(c);
                            }
                            post_quote_ws = None;
                        }
                    }
                    HeaderTokenItem::Whitespace(ws) => {
                        if post_quote_ws.is_none() {
                            post_quote_ws = Some(String::new());
                        }
                        post_quote_ws.as_mut().unwrap().push_str(ws);
                    }
                    HeaderTokenItem::Newline(ws) => {
                        if post_quote_ws.is_none() {
                            post_quote_ws = Some(String::new());
                        }
                        post_quote_ws.as_mut().unwrap().push_str(&ws);
                    }
                    HeaderTokenItem::DecodedWord(word) => {
                        if let Some(ws) = post_quote_ws {
                            name.as_mut().unwrap().push_str(&ws)
                        }
                        name.as_mut().unwrap().push_str(&word);
                        post_quote_ws = None;
                    }
                }
            }
            AddrParseState::BracketedAddr => match hti {
                HeaderTokenItem::Char(c) => {
                    if c == '>' {
                        state = AddrParseState::AfterBracketedAddr;
                        result.push(MailAddr::Single(SingleInfo::new(name, addr.unwrap())?));
                        name = None;
                        addr = None;
                    } else {
                        addr.as_mut().unwrap().push(c);
                    }
                }
                HeaderTokenItem::Whitespace(ws) => {
                    addr.as_mut().unwrap().push_str(ws);
                }
                HeaderTokenItem::Newline(ws) => {
                    addr.as_mut().unwrap().push_str(&ws);
                }
                HeaderTokenItem::DecodedWord(_) => {
                    return Err(MailParseError::Generic(
                        "Unexpected encoded word found inside bracketed address",
                    ));
                }
            },
            AddrParseState::AfterBracketedAddr => {
                match hti {
                    HeaderTokenItem::Char(c) => {
                        if c.is_whitespace() {
                            // continue in same state
                        } else if c == ',' {
                            state = AddrParseState::Initial;
                        } else if c == ';' {
                            if in_group {
                                return Ok(MailAddrList(result));
                            }
                            // Technically not valid, but a similar case occurs in real-world corpus, so handle it gracefully
                            state = AddrParseState::Initial;
                        } else if c == '(' {
                            comment_return = Some(AddrParseState::AfterBracketedAddr);
                            state = AddrParseState::Comment;
                        } else {
                            return Err(MailParseError::Generic(
                                "Unexpected char found after bracketed address",
                            ));
                        }
                    }
                    HeaderTokenItem::Whitespace(_) => {
                        // continue in same state
                    }
                    HeaderTokenItem::Newline(_) => {
                        // continue in same state
                    }
                    HeaderTokenItem::DecodedWord(_) => {
                        return Err(MailParseError::Generic(
                            "Unexpected encoded word found after bracketed address",
                        ));
                    }
                }
            }
            AddrParseState::NameWithEncodedWord => match hti {
                HeaderTokenItem::Char(c) => {
                    if c == '<' {
                        state = AddrParseState::BracketedAddr;
                        name = addr.map(|s| s.trim_end().to_owned());
                        addr = Some(String::new());
                    } else if c == ':' {
                        if in_group {
                            return Err(MailParseError::Generic("Found unexpected nested group"));
                        }
                        let group_addrs = addrparse_inner(it, true)?;
                        state = AddrParseState::Initial;
                        result.push(MailAddr::Group(GroupInfo::new(
                            addr.unwrap().trim_end().to_owned(),
                            group_addrs
                                .0
                                .into_iter()
                                .map(|addr| match addr {
                                    MailAddr::Single(s) => s,
                                    MailAddr::Group(_) => {
                                        panic!("Unexpected nested group encountered")
                                    }
                                })
                                .collect(),
                        )));
                        addr = None;
                    } else {
                        addr.as_mut().unwrap().push(c);
                    }
                }
                HeaderTokenItem::Whitespace(ws) => {
                    addr.as_mut().unwrap().push_str(ws);
                }
                HeaderTokenItem::Newline(ws) => {
                    addr.as_mut().unwrap().push_str(&ws);
                }
                HeaderTokenItem::DecodedWord(word) => {
                    addr.as_mut().unwrap().push_str(&word);
                }
            },
            AddrParseState::Unquoted => {
                match hti {
                    HeaderTokenItem::Char(c) => {
                        if c == '<' {
                            state = AddrParseState::BracketedAddr;
                            name = addr.map(|s| s.trim_end().to_owned());
                            addr = Some(String::new());
                        } else if c == ',' {
                            state = AddrParseState::Initial;
                            result.push(MailAddr::Single(SingleInfo::new(
                                None,
                                addr.unwrap().trim_end().to_owned(),
                            )?));
                            addr = None;
                        } else if c == ';' {
                            result.push(MailAddr::Single(SingleInfo::new(
                                None,
                                addr.unwrap().trim_end().to_owned(),
                            )?));
                            if in_group {
                                return Ok(MailAddrList(result));
                            }
                            // Technically not valid, but occurs in real-world corpus, so handle it gracefully
                            state = AddrParseState::Initial;
                            addr = None;
                        } else if c == ':' {
                            if in_group {
                                return Err(MailParseError::Generic(
                                    "Found unexpected nested group",
                                ));
                            }
                            let group_addrs = addrparse_inner(it, true)?;
                            state = AddrParseState::Initial;
                            result.push(MailAddr::Group(GroupInfo::new(
                                addr.unwrap().trim_end().to_owned(),
                                group_addrs
                                    .0
                                    .into_iter()
                                    .map(|addr| match addr {
                                        MailAddr::Single(s) => s,
                                        MailAddr::Group(_) => {
                                            panic!("Unexpected nested group encountered")
                                        }
                                    })
                                    .collect(),
                            )));
                            addr = None;
                        } else if c == '(' {
                            comment_return = Some(AddrParseState::Unquoted);
                            state = AddrParseState::Comment;
                        } else {
                            addr.as_mut().unwrap().push(c);
                        }
                    }
                    HeaderTokenItem::Whitespace(ws) => {
                        addr.as_mut().unwrap().push_str(ws);
                    }
                    HeaderTokenItem::Newline(ws) => {
                        addr.as_mut().unwrap().push_str(&ws);
                    }
                    HeaderTokenItem::DecodedWord(word) => {
                        state = AddrParseState::NameWithEncodedWord;
                        addr.as_mut().unwrap().push_str(&word);
                    }
                }
            }
            AddrParseState::Comment => {
                match hti {
                    HeaderTokenItem::Char(c) => {
                        if c == ')' {
                            state = comment_return.take().unwrap();
                        }
                    }
                    HeaderTokenItem::Whitespace(_) => {
                        // ignore and stay in same state
                    }
                    HeaderTokenItem::Newline(_) => {
                        // ignore and stay in same state
                    }
                    HeaderTokenItem::DecodedWord(_) => {
                        // ignore and stay in same state
                    }
                }
            }
        }

        hti = match it.next() {
            None => break,
            Some(v) => v,
        };
    }

    if in_group {
        return Err(MailParseError::Generic("Found unterminated group address"));
    }

    match state {
        AddrParseState::QuotedName
        | AddrParseState::EscapedChar
        | AddrParseState::AfterQuotedName
        | AddrParseState::BracketedAddr
        | AddrParseState::Comment
        | AddrParseState::NameWithEncodedWord => Err(MailParseError::Generic(
            "Address string unexpectedly terminated",
        )),
        AddrParseState::Unquoted => {
            result.push(MailAddr::Single(SingleInfo::new(
                None,
                addr.unwrap().trim_end().to_owned(),
            )?));
            Ok(MailAddrList(result))
        }
        _ => Ok(MailAddrList(result)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic() {
        assert_eq!(
            addrparse("foo bar <foo@bar.com>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("foo bar".to_string()), "foo@bar.com".to_string()).unwrap()
            )])
        );
        assert_eq!(
            addrparse("\"foo bar\" <foo@bar.com>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("foo bar".to_string()), "foo@bar.com".to_string()).unwrap()
            )])
        );
        assert_eq!(
            addrparse("foo@bar.com ").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(None, "foo@bar.com".to_string()).unwrap()
            )])
        );
        assert_eq!(
            addrparse("foo <bar@baz.com>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("foo".to_string()), "bar@baz.com".to_string()).unwrap()
            )])
        );
        assert_eq!(
            addrparse("\"foo\" <bar@baz.com>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("foo".to_string()), "bar@baz.com".to_string()).unwrap()
            )])
        );
        assert_eq!(
            addrparse("\"foo \" <bar@baz.com>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("foo ".to_string()), "bar@baz.com".to_string()).unwrap()
            )])
        );
    }

    #[test]
    fn parse_backslashes() {
        assert_eq!(
            addrparse(r#" "First \"nick\" Last" <user@host.tld> "#).unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(
                    Some("First \"nick\" Last".to_string()),
                    "user@host.tld".to_string()
                )
                .unwrap()
            )])
        );
        assert_eq!(
            addrparse(r#" First \"nick\" Last <user@host.tld> "#).unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(
                    Some("First \\\"nick\\\" Last".to_string()),
                    "user@host.tld".to_string()
                )
                .unwrap()
            )])
        );
    }

    #[test]
    fn parse_multi() {
        assert_eq!(
            addrparse("foo <ba@r>, jo@e, baz <qu@ux>").unwrap(),
            MailAddrList(vec![
                MailAddr::Single(
                    SingleInfo::new(Some("foo".to_string()), "ba@r".to_string()).unwrap()
                ),
                MailAddr::Single(SingleInfo::new(None, "jo@e".to_string()).unwrap()),
                MailAddr::Single(
                    SingleInfo::new(Some("baz".to_string()), "qu@ux".to_string()).unwrap()
                ),
            ])
        );
    }

    #[test]
    fn parse_empty_group() {
        assert_eq!(
            addrparse("empty-group:;").unwrap(),
            MailAddrList(vec![MailAddr::Group(GroupInfo::new(
                "empty-group".to_string(),
                vec![]
            ))])
        );
        assert_eq!(
            addrparse(" empty-group : ; ").unwrap(),
            MailAddrList(vec![MailAddr::Group(GroupInfo::new(
                "empty-group".to_string(),
                vec![]
            ))])
        );
    }

    #[test]
    fn parse_simple_group() {
        assert_eq!(
            addrparse("bar-group: foo <foo@bar.com>;").unwrap(),
            MailAddrList(vec![MailAddr::Group(GroupInfo::new(
                "bar-group".to_string(),
                vec![SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string()).unwrap(),]
            ))])
        );
        assert_eq!(
            addrparse("bar-group: foo <foo@bar.com>, baz@bar.com;").unwrap(),
            MailAddrList(vec![MailAddr::Group(GroupInfo::new(
                "bar-group".to_string(),
                vec![
                    SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string()).unwrap(),
                    SingleInfo::new(None, "baz@bar.com".to_string()).unwrap(),
                ]
            ))])
        );
    }

    #[test]
    fn parse_mixed() {
        assert_eq!(
            addrparse("joe@bloe.com, bar-group: foo <foo@bar.com>;").unwrap(),
            MailAddrList(vec![
                MailAddr::Single(SingleInfo::new(None, "joe@bloe.com".to_string()).unwrap()),
                MailAddr::Group(GroupInfo::new(
                    "bar-group".to_string(),
                    vec![
                        SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string())
                            .unwrap(),
                    ]
                )),
            ])
        );
        assert_eq!(
            addrparse("bar-group: foo <foo@bar.com>; joe@bloe.com").unwrap(),
            MailAddrList(vec![
                MailAddr::Group(GroupInfo::new(
                    "bar-group".to_string(),
                    vec![
                        SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string())
                            .unwrap(),
                    ]
                )),
                MailAddr::Single(SingleInfo::new(None, "joe@bloe.com".to_string()).unwrap()),
            ])
        );
        assert_eq!(
            addrparse("flim@flam.com, bar-group: foo <foo@bar.com>; joe@bloe.com").unwrap(),
            MailAddrList(vec![
                MailAddr::Single(SingleInfo::new(None, "flim@flam.com".to_string()).unwrap()),
                MailAddr::Group(GroupInfo::new(
                    "bar-group".to_string(),
                    vec![
                        SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string())
                            .unwrap(),
                    ]
                )),
                MailAddr::Single(SingleInfo::new(None, "joe@bloe.com".to_string()).unwrap()),
            ])
        );
        assert_eq!(
            addrparse("first-group:; flim@flam.com, bar-group: foo <foo@bar.com>; joe@bloe.com, final-group: zi@p, za@p, \"Zaphod\" <zaphod@beeblebrox>;").unwrap(),
            MailAddrList(vec![
                MailAddr::Group(GroupInfo::new("first-group".to_string(), vec![])),
                MailAddr::Single(SingleInfo::new(None, "flim@flam.com".to_string()).unwrap()),
                MailAddr::Group(GroupInfo::new("bar-group".to_string(), vec![
                    SingleInfo::new(Some("foo".to_string()), "foo@bar.com".to_string()).unwrap(),
                ])),
                MailAddr::Single(SingleInfo::new(None, "joe@bloe.com".to_string()).unwrap()),
                MailAddr::Group(GroupInfo::new("final-group".to_string(), vec![
                    SingleInfo::new(None, "zi@p".to_string()).unwrap(),
                    SingleInfo::new(None, "za@p".to_string()).unwrap(),
                    SingleInfo::new(Some("Zaphod".to_string()), "zaphod@beeblebrox".to_string()).unwrap(),
                ])),
            ])
        );
    }

    #[test]
    fn real_world_examples() {
        // taken from a real "From" header. This might not be valid according to the RFC
        // but obviously made it through the internet so we should at least not crash.
        assert_eq!(
            addrparse("\"The Foo of Bar\" Course Staff <foo-no-reply@bar.edx.org>").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(
                    Some("The Foo of Bar Course Staff".to_string()),
                    "foo-no-reply@bar.edx.org".to_string()
                )
                .unwrap()
            )])
        );

        // This one has a comment tacked on to the end. Adding proper support for comments seems
        // complicated so I just added trailer comment support.
        assert_eq!(
            addrparse("John Doe <support@github.com> (GitHub Staff)").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(
                    Some("John Doe".to_string()),
                    "support@github.com".to_string()
                )
                .unwrap()
            )])
        );

        // Taken from a real world "To" header. It was spam, but still...
        assert_eq!(
            addrparse("foo@bar.com;").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(None, "foo@bar.com".to_string()).unwrap()
            )])
        );

        // From https://github.com/deltachat/deltachat-core-rust/pull/1476#issuecomment-629681157
        assert_eq!(
            addrparse("mailer-daemon@hq5.merlinux.eu (mail delivery system)").unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(None, "mailer-daemon@hq5.merlinux.eu".to_string()).unwrap()
            )])
        );
    }

    #[test]
    fn stringify_single() {
        let tc = SingleInfo::new(Some("John Doe".to_string()), "john@doe.com".to_string()).unwrap();
        assert_eq!(tc.to_string(), r#""John Doe" <john@doe.com>"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Single(tc)])
        );

        let tc = SingleInfo::new(
            Some(r#"John "Jack" Doe"#.to_string()),
            "john@doe.com".to_string(),
        )
        .unwrap();
        assert_eq!(tc.to_string(), r#""John \"Jack\" Doe" <john@doe.com>"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Single(tc)])
        );

        let tc = SingleInfo::new(None, "foo@bar.com".to_string()).unwrap();
        assert_eq!(tc.to_string(), r#"foo@bar.com"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Single(tc)])
        );
    }

    #[test]
    fn stringify_group() {
        let tc = GroupInfo::new(
            "group-name".to_string(),
            vec![
                SingleInfo::new(None, "foo@bar.com".to_string()).unwrap(),
                SingleInfo::new(Some("A".to_string()), "a@b".to_string()).unwrap(),
            ],
        );
        assert_eq!(tc.to_string(), r#""group-name": foo@bar.com, "A" <a@b>;"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Group(tc)])
        );

        let tc = GroupInfo::new("empty-group".to_string(), vec![]);
        assert_eq!(tc.to_string(), r#""empty-group":;"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Group(tc)])
        );

        let tc = GroupInfo::new(r#"group-with"quote"#.to_string(), vec![]);
        assert_eq!(tc.to_string(), r#""group-with\"quote":;"#);
        assert_eq!(
            addrparse(&tc.to_string()).unwrap(),
            MailAddrList(vec![MailAddr::Group(tc)])
        );
    }

    #[test]
    fn stringify_list() {
        let tc = MailAddrList(vec![
            MailAddr::Group(GroupInfo::new(
                "marvel".to_string(),
                vec![
                    SingleInfo::new(None, "ironman@marvel.com".to_string()).unwrap(),
                    SingleInfo::new(None, "spiderman@marvel.com".to_string()).unwrap(),
                ],
            )),
            MailAddr::Single(
                SingleInfo::new(Some("b-man".to_string()), "b@man.com".to_string()).unwrap(),
            ),
            MailAddr::Group(GroupInfo::new(
                "dc".to_string(),
                vec![
                    SingleInfo::new(None, "batman@dc.com".to_string()).unwrap(),
                    SingleInfo::new(None, "superman@dc.com".to_string()).unwrap(),
                ],
            )),
            MailAddr::Single(
                SingleInfo::new(Some("d-woman".to_string()), "d@woman.com".to_string()).unwrap(),
            ),
        ]);
        assert_eq!(
            tc.to_string(),
            r#""marvel": ironman@marvel.com, spiderman@marvel.com; "b-man" <b@man.com>, "dc": batman@dc.com, superman@dc.com; "d-woman" <d@woman.com>"#
        );
    }

    #[test]
    fn count_addrs() {
        let tc = MailAddrList(vec![
            MailAddr::Group(GroupInfo::new(
                "marvel".to_string(),
                vec![
                    SingleInfo::new(None, "ironman@marvel.com".to_string()).unwrap(),
                    SingleInfo::new(None, "spiderman@marvel.com".to_string()).unwrap(),
                ],
            )),
            MailAddr::Single(
                SingleInfo::new(Some("b-man".to_string()), "b@man.com".to_string()).unwrap(),
            ),
            MailAddr::Group(GroupInfo::new(
                "dc".to_string(),
                vec![
                    SingleInfo::new(None, "batman@dc.com".to_string()).unwrap(),
                    SingleInfo::new(None, "superman@dc.com".to_string()).unwrap(),
                ],
            )),
            MailAddr::Single(
                SingleInfo::new(Some("d-woman".to_string()), "d@woman.com".to_string()).unwrap(),
            ),
        ]);
        assert_eq!(tc.count_addrs(), 6);
        assert_eq!(tc.extract_single_info(), None);

        let tc = MailAddrList(vec![]);
        assert_eq!(tc.count_addrs(), 0);
        assert_eq!(tc.extract_single_info(), None);

        let tc = MailAddrList(vec![MailAddr::Group(GroupInfo::new(
            "group".to_string(),
            vec![SingleInfo::new(None, "foo@bar.com".to_string()).unwrap()],
        ))]);
        assert_eq!(tc.count_addrs(), 1);
        assert_eq!(tc.extract_single_info(), None);

        let tc = MailAddrList(vec![MailAddr::Single(
            SingleInfo::new(None, "foo@bar.com".to_string()).unwrap(),
        )]);
        assert_eq!(tc.count_addrs(), 1);
        assert_eq!(
            tc.extract_single_info(),
            Some(SingleInfo::new(None, "foo@bar.com".to_string()).unwrap())
        );

        let tc = MailAddrList(vec![
            MailAddr::Group(GroupInfo::new("group".to_string(), vec![])),
            MailAddr::Group(GroupInfo::new("group".to_string(), vec![])),
        ]);
        assert_eq!(tc.count_addrs(), 0);
        assert_eq!(tc.extract_single_info(), None);
    }

    #[test]
    fn parse_invalid() {
        assert!(addrparse("foo").is_err());
        assert!(addrparse("foo <bar>").is_err());
        assert!(addrparse("group: foo <bar>;").is_err());
    }

    #[test]
    fn parse_with_encoded() {
        let (parsed, _) = crate::parse_header(
            b"From: =?UTF-8?B?0JjQvNGPLCDQpNCw0LzQuNC70LjRjw==?= <foobar@example.com>",
        )
        .unwrap();
        assert_eq!(
            addrparse_header(&parsed).unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(
                    Some("Имя, Фамилия".to_string()),
                    "foobar@example.com".to_string()
                )
                .unwrap()
            )])
        );
    }

    #[test]
    fn parse_quoted_encoded() {
        let (parsed, _) =
            crate::parse_header(b"From: \"=?utf-8?q?G=C3=B6tz?= C\" <g@c.de>").unwrap();
        assert_eq!(
            addrparse_header(&parsed).unwrap(),
            MailAddrList(vec![MailAddr::Single(
                SingleInfo::new(Some("Götz C".to_string()), "g@c.de".to_string()).unwrap()
            )])
        );
    }

    #[test]
    fn parse_second_encoded() {
        let (parsed, _) = crate::parse_header(
            b"To: foo <foo@example.org>,=?UTF-8?B?Zm9v8J+Qm2Jhcg==?= <bar@example.org>",
        )
        .unwrap();
        assert_eq!(
            addrparse_header(&parsed).unwrap(),
            MailAddrList(vec![
                MailAddr::Single(
                    SingleInfo::new(Some("foo".to_string()), "foo@example.org".to_string())
                        .unwrap()
                ),
                MailAddr::Single(
                    SingleInfo::new(
                        Some("foo\u{1f41b}bar".to_string()),
                        "bar@example.org".to_string()
                    )
                    .unwrap()
                )
            ])
        );
    }
}
