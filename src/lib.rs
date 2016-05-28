#[derive(Debug)]
pub struct MailHeader<'a> {
    key: &'a str,
    value: &'a str,
}

#[derive(Debug)]
pub struct MailParseError {
    description: String,
    position: usize,
}

enum HeaderParseState {
    Initial,
    Key,
    PreValue,
    Value,
    ValueNewline,
}

pub fn parse_header(raw_data: &str) -> Result<(MailHeader, usize), MailParseError> {
    let mut it = raw_data.chars();
    let mut ix = 0;
    let mut c = match it.next() {
        None => return Err(MailParseError {
                description: "Empty string provided".to_string(),
                position: 0,
            }),
        Some(v) => v,
    };

    let mut ix_key_end = None;
    let mut ix_value_start = 0;
    let mut ix_value_end = 0;

    let mut state = HeaderParseState::Initial;
    loop {
        match state {
            HeaderParseState::Initial => {
                if c == ' ' {
                    return Err(MailParseError {
                        description: "Header cannot start with a space; it is likely an overhanging line from a previous header".to_string(),
                        position: ix,
                    });
                };
                state = HeaderParseState::Key;
                continue;
            },
            HeaderParseState::Key => {
                if c == ':' {
                    ix_key_end = Some(ix);
                    state = HeaderParseState::PreValue;
                } else if c == '\n' {
                    return Err(MailParseError {
                        description: "Unexpected newline in header key".to_string(),
                        position: ix,
                    });
                }
            }
            HeaderParseState::PreValue => {
                if c != ' ' {
                    ix_value_start = ix;
                    ix_value_end = ix;
                    state = HeaderParseState::Value;
                    continue;
                }
            }
            HeaderParseState::Value => {
                if c == '\n' {
                    state = HeaderParseState::ValueNewline;
                } else {
                    ix_value_end = ix + 1;
                }
            }
            HeaderParseState::ValueNewline => {
                if c == ' ' {
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
            Some(v) => v,
        };
    }
    match ix_key_end {
        Some(v) => Ok((MailHeader {
            key: &raw_data[0..v],
            value: &raw_data[ix_value_start..ix_value_end],
        }, ix)),

        None => Err(MailParseError {
            description: "Unable to determine end of the header key component".to_string(),
            position: ix,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic_header() {
        let (parsed, _) = parse_header("Key: Value").unwrap();
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Value");

        let (parsed, _) = parse_header("Key :  Value ").unwrap();
        assert_eq!(parsed.key, "Key ");
        assert_eq!(parsed.value, "Value ");

        let (parsed, _) = parse_header("Key:").unwrap();
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "");

        let (parsed, _) = parse_header(":\n").unwrap();
        assert_eq!(parsed.key, "");
        assert_eq!(parsed.value, "");

        let (parsed, _) = parse_header("Key:Multi-line\n value").unwrap();
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Multi-line\n value");

        let (parsed, _) = parse_header("Key:  Multi\n  line\n value\n").unwrap();
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Multi\n  line\n value");

        let (parsed, _) = parse_header("Key: One\nKey2: Two").unwrap();
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "One");

        parse_header(" Leading: Space").unwrap_err();
        parse_header("Just a string").unwrap_err();
        parse_header("Key\nBroken: Value").unwrap_err();
    }
}
