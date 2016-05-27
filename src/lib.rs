#[derive(Debug)]
pub struct MailHeader<'a> {
    name: &'a str,
    value: &'a str,
}

#[derive(Debug)]
pub struct MailParseError {
    description: String,
    position: usize,
}

pub fn parse_header(raw_data: &str) -> Result<MailHeader, MailParseError> {
    let ix = raw_data.find(':').ok_or(MailParseError {
        description: "No ':' found in header".to_string(),
        position: raw_data.len(),
    });
    return ix.map(|ix| {
        MailHeader {
            name: &raw_data[0..ix],
            value: &raw_data[ix + 1..],
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_basic_header() {
        let parsed = parse_header("Key: Value").expect("");
        assert_eq!(parsed.name, "Key");
        assert_eq!(parsed.value, " Value");
    }
}
