use std::fmt;

use crate::MailParseError;

/// A simple wrapper around `Vec<String>`. This is primarily here so we can
/// implement the Display trait on it, and allow user code to easily convert
/// the return value from `msgidparse` back into a string. This also allows
/// to add additional methods on this type in the future.
#[derive(Clone, Debug, PartialEq)]
pub struct MessageIdList(Vec<String>);

impl std::ops::Deref for MessageIdList {
    type Target = Vec<String>;

    fn deref(&self) -> &Vec<String> {
        &self.0
    }
}

impl std::ops::DerefMut for MessageIdList {
    fn deref_mut(&mut self) -> &mut Vec<String> {
        &mut self.0
    }
}

impl fmt::Display for MessageIdList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for msgid in self.iter() {
            if !first {
                write!(f, " ")?;
            }
            write!(f, "<{}>", msgid)?;
            first = false;
        }
        Ok(())
    }
}

/// Parse an email header into a structured type holding a list of message ids.
/// This function can be used to parse headers containing message IDs, such as
/// `Message-ID`, `In-Reply-To`, and `References`.
/// This function is currently mostly trivial (splits on whitespace and strips
/// angle-brackets) but may be enhanced in the future to strip comments (which
/// are technically allowed by the RFCs but never really used in practice).
///
/// # Examples
/// ```
///     use mailparse::{msgidparse, MessageIdList};
///     let parsed_ids = msgidparse("<msg_one@foo.com>  <msg_two@bar.com>").unwrap();
///     assert_eq!(parsed_ids[0], "msg_one@foo.com");
///     assert_eq!(parsed_ids[1], "msg_two@bar.com");
/// ```
pub fn msgidparse(ids: &str) -> Result<MessageIdList, MailParseError> {
    let mut msgids = Vec::new();
    for id in ids.split_whitespace() {
        if !id.starts_with('<') {
            return Err(MailParseError::Generic("Message IDs must start with <"));
        }
        if !id.ends_with('>') {
            return Err(MailParseError::Generic("Message IDs must end with >"));
        }
        msgids.push(id[1..id.len() - 1].to_string());
    }
    Ok(MessageIdList(msgids))
}
