//! Word-at-a-time byte scanning for the two hot loops in the parser: the MIME
//! boundary search and the whitespace strip before base64 decoding.
//! Uses the classic haszero/hasless bit tricks (Bit Twiddling Hacks) on usize
//! words loaded with from_le_bytes. No unsafe, no dependencies.

// clippy suggests as_chunks here, but that needs Rust 1.88.
#![allow(clippy::chunks_exact_to_as_chunks)]

const WORD: usize = core::mem::size_of::<usize>();
const LO: usize = usize::from_ne_bytes([0x01; WORD]);
const HI: usize = usize::from_ne_bytes([0x80; WORD]);

/// Sets the high bit of every zero byte in `x`. Only the lowest mark is exact
/// (the borrow can mark bytes above it), so callers re-check the rest.
#[inline]
fn zero_byte_mask(x: usize) -> usize {
    x.wrapping_sub(LO) & !x & HI
}

/// Same as zero_byte_mask, but marks bytes below `n` (n <= 128).
#[inline]
fn below_mask(x: usize, n: u8) -> usize {
    x.wrapping_sub(LO.wrapping_mul(n as usize)) & !x & HI
}

/// Load one word little-endian, so mask bit `p` means byte `p / 8`.
#[inline]
fn word(chunk: &[u8]) -> usize {
    let mut bytes = [0u8; WORD];
    bytes.copy_from_slice(chunk);
    usize::from_le_bytes(bytes)
}

/// Byte index of the lowest set bit of a non-zero mask.
#[inline]
fn first_hit(mask: usize) -> usize {
    (mask.trailing_zeros() / 8) as usize
}

/// The byte search checks 8 words (64 bytes) per iteration.
const STEP_WORDS: usize = 8;
const STEP: usize = STEP_WORDS * WORD;

/// Index of the first `needle` in `haystack`.
pub(crate) fn find_byte(haystack: &[u8], needle: u8) -> Option<usize> {
    let repeated = LO.wrapping_mul(needle as usize);
    let mut steps = haystack.chunks_exact(STEP);
    let mut offset = 0;
    for step in &mut steps {
        let mut masks = [0usize; STEP_WORDS];
        let mut any = 0;
        for (k, m) in masks.iter_mut().enumerate() {
            *m = zero_byte_mask(word(&step[k * WORD..(k + 1) * WORD]) ^ repeated);
            any |= *m;
        }
        if any != 0 {
            let (k, m) = masks
                .iter()
                .copied()
                .enumerate()
                .find(|&(_, m)| m != 0)
                .unwrap();
            return Some(offset + k * WORD + first_hit(m));
        }
        offset += STEP;
    }
    steps
        .remainder()
        .iter()
        .position(|&b| b == needle)
        .map(|i| offset + i)
}

/// First occurrence of `key` in `haystack`: scan for key[0], then compare the rest.
pub(crate) fn find(haystack: &[u8], key: &[u8]) -> Option<usize> {
    let first = *key.first()?;
    let mut at = 0;
    while let Some(i) = find_byte(&haystack[at..], first) {
        let candidate = at + i;
        if haystack[candidate..].starts_with(key) {
            return Some(candidate);
        }
        at = candidate + 1;
    }
    None
}

/// `body` without ASCII whitespace (same set as u8::is_ascii_whitespace).
///
/// All whitespace bytes are below 0x21, so a word with no byte below 0x21 is
/// copied through untouched. Candidates are re-checked with is_ascii_whitespace
/// (so 0x0B etc. are kept, as before), and runs are copied in one piece.
pub(crate) fn strip_ascii_whitespace(body: &[u8]) -> Vec<u8> {
    let mut cleaned = Vec::with_capacity(body.len());
    let mut run_start = 0;
    let mut offset = 0;
    let mut words = body.chunks_exact(WORD);
    for chunk in &mut words {
        let mut mask = below_mask(word(chunk), 0x21);
        while mask != 0 {
            let i = offset + first_hit(mask);
            if body[i].is_ascii_whitespace() {
                cleaned.extend_from_slice(&body[run_start..i]);
                run_start = i + 1;
            }
            mask &= mask - 1;
        }
        offset += WORD;
    }
    for (i, &b) in words.remainder().iter().enumerate() {
        if b.is_ascii_whitespace() {
            cleaned.extend_from_slice(&body[run_start..offset + i]);
            run_start = offset + i + 1;
        }
    }
    cleaned.extend_from_slice(&body[run_start..]);
    cleaned
}

#[cfg(test)]
mod tests {
    use super::{find, find_byte, strip_ascii_whitespace};

    fn naive_find(haystack: &[u8], key: &[u8]) -> Option<usize> {
        if key.is_empty() || haystack.len() < key.len() {
            return None;
        }
        (0..=haystack.len() - key.len()).find(|&i| &haystack[i..i + key.len()] == key)
    }

    fn naive_strip(body: &[u8]) -> Vec<u8> {
        body.iter()
            .filter(|c| !c.is_ascii_whitespace())
            .cloned()
            .collect()
    }

    /// Small-alphabet pseudo-random inputs at every length, so needles hit at every alignment.
    fn corpus() -> Vec<Vec<u8>> {
        let mut out = Vec::new();
        let mut state: u32 = 0x9E37_79B9;
        for len in 0..80 {
            for _ in 0..4 {
                let mut v = Vec::with_capacity(len);
                for _ in 0..len {
                    state ^= state << 13;
                    state ^= state >> 17;
                    state ^= state << 5;
                    const ALPHABET: &[u8; 12] = b"-\r\n \tab=\x0c\x0b\x00\xff";
                    v.push(ALPHABET[(state % 12) as usize]);
                }
                out.push(v);
            }
        }
        out
    }

    #[test]
    fn find_byte_matches_position() {
        for h in corpus() {
            for needle in [b'-', b'\n', b'a', 0x00, 0xff, b'z'] {
                assert_eq!(
                    find_byte(&h, needle),
                    h.iter().position(|&b| b == needle),
                    "{:?} / {:?}",
                    h,
                    needle
                );
            }
        }
    }

    #[test]
    fn find_matches_naive_search() {
        let keys: &[&[u8]] = &[
            b"-",
            b"--",
            b"--ab",
            b"\n",
            b"\r\n",
            b"ab=",
            b"zz",
            b"\x00\xff",
        ];
        for h in corpus() {
            for key in keys {
                assert_eq!(find(&h, key), naive_find(&h, key), "{:?} / {:?}", h, key);
            }
        }
        assert_eq!(find(b"abc", b""), None);
        assert_eq!(find(b"", b"a"), None);
        assert_eq!(find(b"ab", b"abc"), None);
    }

    #[test]
    fn strip_matches_filter() {
        for h in corpus() {
            assert_eq!(strip_ascii_whitespace(&h), naive_strip(&h), "{:?}", h);
        }
        for b in 0u8..=255 {
            let single = [b];
            assert_eq!(
                strip_ascii_whitespace(&single),
                naive_strip(&single),
                "{:?}",
                b
            );
            let mixed = [
                b' ', b, b'\t', b, b'\r', b'\n', b, b, b, b, b, b, b, b, b, b, b'\x0c', b,
            ];
            assert_eq!(
                strip_ascii_whitespace(&mixed),
                naive_strip(&mixed),
                "{:?}",
                b
            );
        }
        // vertical tab is below 0x21 but is not ASCII whitespace: kept
        assert_eq!(
            strip_ascii_whitespace(b"a\x0bb\x0b\x0b\x0b\x0b\x0b\x0bc"),
            b"a\x0bb\x0b\x0b\x0b\x0b\x0b\x0bc"
        );
    }
}
