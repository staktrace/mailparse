use charset::Charset;

use crate::find_from;

/// Some types of tokens that might be present in a MIME header. This
/// list is incomplete relative the types of tokens defined in the RFC,
/// but can be expanded as needed. Currently the list of tokens is
/// sufficient to properly handle encoded words and line unfolding.
pub enum HeaderToken<'a> {
    /// A bunch of not-encoded text. This can include whitespace and
    /// non-whitespace chars.
    Text(&'a str),
    /// A bunch of text that is purely whitespace.
    Whitespace(&'a str),
    /// An end-of-line marker. If it contains None, then it represents
    /// a raw CRLF that has not yet been line-unfolded. If it contains
    /// a string, that represents the whitespace that was produced
    /// around that CRLF during line unfolding. This may include whitespace
    /// from the end of the previous line.
    Newline(Option<String>),
    /// The decoded value of an encoded word found in the header.
    DecodedWord(String),
}

fn is_boundary(line: &str, ix: Option<usize>) -> bool {
    ix.and_then(|v| line.chars().nth(v))
        .map(|c| {
            c.is_whitespace()
                || c == '"'
                || c == '('
                || c == ')'
                || c == '<'
                || c == '>'
                || c == ','
        })
        .unwrap_or(true)
}

fn decode_word(encoded: &str) -> Option<String> {
    let ix_delim1 = encoded.find('?')?;
    let ix_delim2 = find_from(encoded, ix_delim1 + 1, "?")?;

    let charset = &encoded[0..ix_delim1];
    let transfer_coding = &encoded[ix_delim1 + 1..ix_delim2];
    let input = &encoded[ix_delim2 + 1..];

    let decoded = match transfer_coding {
        "B" | "b" => data_encoding::BASE64_MIME_PERMISSIVE
            .decode(input.as_bytes())
            .ok()?,
        "Q" | "q" => {
            // The quoted_printable module does a trim_end on the input, so if
            // that affects the output we should save and restore the trailing
            // whitespace
            let to_decode = input.replace('_', " ");
            let trimmed = to_decode.trim_end();
            let mut d = quoted_printable::decode(trimmed, quoted_printable::ParseMode::Robust);
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

/// Tokenizes a single line of the header and produces a vector of
/// tokens. Because this only processes a single line, it will never
/// generate `HeaderToken::Newline` tokens.
fn tokenize_header_line(line: &str) -> Vec<HeaderToken> {
    fn maybe_whitespace(text: &str) -> HeaderToken {
        if text.trim_end().is_empty() {
            HeaderToken::Whitespace(text)
        } else {
            HeaderToken::Text(text)
        }
    }

    let mut result = Vec::new();
    let mut ix_search = 0;
    loop {
        match find_from(line, ix_search, "=?") {
            Some(v) => {
                let ix_begin = v + 2;
                if !is_boundary(line, ix_begin.checked_sub(3)) {
                    result.push(HeaderToken::Text(&line[ix_search..ix_begin]));
                    ix_search = ix_begin;
                    continue;
                }
                result.push(maybe_whitespace(&line[ix_search..ix_begin - 2]));
                let mut ix_end_search = ix_begin;
                loop {
                    match find_from(line, ix_end_search, "?=") {
                        Some(ix_end) => {
                            if !is_boundary(line, ix_end.checked_add(2)) {
                                ix_end_search = ix_end + 2;
                                continue;
                            }
                            match decode_word(&line[ix_begin..ix_end]) {
                                Some(v) => result.push(HeaderToken::DecodedWord(v)),
                                None => {
                                    result.push(HeaderToken::Text(&line[ix_begin - 2..ix_end + 2]));
                                }
                            };
                            ix_search = ix_end;
                        }
                        None => {
                            result.push(HeaderToken::Text("=?"));
                            ix_search = ix_begin - 2;
                        }
                    };
                    break;
                }
                ix_search += 2;
                continue;
            }
            None => {
                result.push(maybe_whitespace(&line[ix_search..]));
                break;
            }
        };
    }
    result
}

/// Tokenize an entire header, including newlines. This includes
/// decoded words, but doesn't do line unfolding, so any `HeaderToken::Newline`
/// tokens will always have a `None` inner value. Whitespace preceding
/// the newline will be in a separate `HeaderToken::Whitespace` or
/// `HeaderToken::Text` token. Semantically the `HeaderToken::Newline`
/// tokens that come out of this still represent the CRLF newline.
fn tokenize_header(value: &str) -> Vec<HeaderToken> {
    let mut tokens = Vec::new();
    let mut lines = value.lines();
    let mut first = true;
    while let Some(line) = lines.next().map(str::trim_start) {
        if first {
            first = false;
        } else {
            tokens.push(HeaderToken::Newline(None));
        }
        let mut line_tokens = tokenize_header_line(line);
        tokens.append(&mut line_tokens);
    }
    tokens
}

/// Takes in a list of tokens and processes them to normalize the whitespace
/// per the RFC. This includes dropping any whitespace between two adjacent
/// encoded words, and also doing line unfolding. As a result, the `HeaderToken::Newline`
/// tokens that come out of this no longer represent the CRLF newline, but instead
/// their contained `Option<String>` will be populated with whatever whitespace gets
/// generated from unfolding the line. This might include end-of-line whitespace from
/// the previous line.
fn normalize_header_whitespace(tokens: Vec<HeaderToken>) -> Vec<HeaderToken> {
    let mut result = Vec::<HeaderToken>::new();

    let mut saved_token = None;
    // See RFC 2047 section 6.2 for what's going on here. Basically whitespace
    // that's between two adjacent encoded words should be thrown away.
    for tok in tokens {
        match &tok {
            HeaderToken::Text(_) => {
                // If we saved some whitespace, put it in since we encountered
                // non-whitespace chars that weren't part of an encoded word.
                if let Some(HeaderToken::Whitespace(_)) = &saved_token {
                    result.push(saved_token.unwrap());
                } else if let Some(HeaderToken::Newline(Some(_))) = &saved_token {
                    result.push(saved_token.unwrap());
                }
                // Also put the actual non-whitespace chars.
                result.push(tok);
                saved_token = None;
            }
            HeaderToken::Whitespace(_) => {
                // If the previous token was an encoded word, save the whitespace
                // as whitespace that's between two encoded words should be dropped.
                // We only know if this whitespace goes into `result` after parsing
                // the next token.
                if let Some(HeaderToken::DecodedWord(_)) = saved_token {
                    saved_token = Some(tok);
                } else {
                    result.push(tok);
                    saved_token = None;
                }
            }
            HeaderToken::Newline(_) => {
                // If we saved whitespace at the end of the line, add an extra space
                // to it from the line unfolding.
                if let Some(HeaderToken::Whitespace(ws)) = saved_token {
                    let new_ws = ws.to_owned() + " ";
                    saved_token = Some(HeaderToken::Newline(Some(new_ws)));
                // If the end of the line had an encoded word, save the space from
                // line unfolding.
                } else if let Some(HeaderToken::DecodedWord(_)) = saved_token {
                    saved_token = Some(HeaderToken::Newline(Some(" ".to_string())));
                } else {
                    result.push(HeaderToken::Newline(Some(" ".to_string())));
                    saved_token = None;
                }
            }
            HeaderToken::DecodedWord(_) => {
                // Note that saved_token might be a whitespace thing here. But we
                // throw it away because that means it fell between two adjacent
                // encoded words.
                saved_token = Some(HeaderToken::DecodedWord(String::new()));
                result.push(tok);
            }
        }
    }
    result
}

pub fn normalized_tokens(raw_value: &str) -> Vec<HeaderToken> {
    normalize_header_whitespace(tokenize_header(raw_value))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_boundary_multibyte() {
        // Bug #26, Incorrect unwrap() guard in is_boundary()
        // 6x'REPLACEMENT CHARACTER', but 18 bytes of data:
        let test = "\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}\u{FFFD}";
        assert!(is_boundary(test, Some(8)));
    }
}
