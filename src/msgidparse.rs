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

    // The remaining section of the header, not yet chomped
    let mut remaining = ids.trim_start();
    // While we have some value of the header remaining
    while !remaining.is_empty() {
        // The next character should be the start of a Message ID
        if !remaining.starts_with('<') {
            return Err(MailParseError::Generic("Message IDs must start with <"));
        }
        // The ID ends at the next '>'
        let end_index = remaining
            .find('>')
            .ok_or(MailParseError::Generic("Message IDs must end with >"))?;
        msgids.push(remaining[1..end_index].to_string());

        // Chomp the part of the string we just processed, and any trailing whitespace
        remaining = remaining[end_index + 1..].trim_start();
    }
    Ok(MessageIdList(msgids))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_message_ids() {
        assert_eq!(
            msgidparse("").expect("Empty string"),
            MessageIdList(Vec::new())
        );
        assert_eq!(
            msgidparse("<msg_one@foo.com>").expect("Single reference"),
            MessageIdList(vec!["msg_one@foo.com".to_string()])
        );
        assert_eq!(
            msgidparse(" <msg_one@foo.com>").expect("Single reference, leading whitespace"),
            MessageIdList(vec!["msg_one@foo.com".to_string()])
        );
        assert_eq!(
            msgidparse("<msg_one@foo.com> ").expect("Single reference, trailing whitespace"),
            MessageIdList(vec!["msg_one@foo.com".to_string()])
        );
        assert_eq!(
            msgidparse("<msg_one@foo.com> <msg_two@bar.com>")
                .expect("Multiple references separated by space"),
            MessageIdList(vec![
                "msg_one@foo.com".to_string(),
                "msg_two@bar.com".to_string(),
            ])
        );
        assert_eq!(
            msgidparse("\n<msg_one@foo.com> <msg_two@bar.com>\t<msg_three@qux.com>\r ")
                .expect("Multiple references separated by various whitespace"),
            MessageIdList(vec![
                "msg_one@foo.com".to_string(),
                "msg_two@bar.com".to_string(),
                "msg_three@qux.com".to_string(),
            ])
        );

        // Non whitespace separator tests
        assert_eq!(
            msgidparse("<msg_one@foo.com><msg_two@bar.com>")
                .expect("Multiple references, no whitespace"),
            MessageIdList(vec![
                "msg_one@foo.com".to_string(),
                "msg_two@bar.com".to_string(),
            ])
        );
        assert_eq!(
            msgidparse("<msg_one@foo.com><msg_two@bar.com> <msg_three@spam.com> ")
                .expect("Mixed whitespace/non-whitespace separator"),
            MessageIdList(vec![
                "msg_one@foo.com".to_string(),
                "msg_two@bar.com".to_string(),
                "msg_three@spam.com".to_string(),
            ])
        );
    }
}
