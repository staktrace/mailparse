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

pub fn parse_header(raw_data: &str) -> Result<MailHeader, MailParseError> {
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
        c = match it.next() {
            None => break,
            Some(v) => v,
        };
        ix = ix + 1;
    }
    match ix_key_end {
        Some(v) => Ok(MailHeader {
            key: &raw_data[0..v],
            value: &raw_data[ix_value_start..ix_value_end],
        }),

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
        let mut parsed = parse_header("Key: Value").expect("");
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Value");

        parsed = parse_header("Key :  Value ").expect("");
        assert_eq!(parsed.key, "Key ");
        assert_eq!(parsed.value, "Value ");

        parsed = parse_header("Key:").expect("");
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "");

        parsed = parse_header(":\n").expect("");
        assert_eq!(parsed.key, "");
        assert_eq!(parsed.value, "");

        parsed = parse_header("Key:Multi-line\n value").expect("");
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Multi-line\n value");

        parsed = parse_header("Key:  Multi\n  line\n value\n").expect("");
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "Multi\n  line\n value");

        parsed = parse_header("Key: One\nKey2: Two").expect("");
        assert_eq!(parsed.key, "Key");
        assert_eq!(parsed.value, "One");
    }
}
