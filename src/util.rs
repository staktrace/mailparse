pub fn is_boundary(line: &str, ix: Option<usize>) -> bool {
    ix.and_then(|v| line.chars().nth(v))
        .map(|c| c.is_whitespace() || c == '"' || c == '(' || c == ')' || c == '<' || c == '>')
        .unwrap_or(true)
}

pub fn find_from(line: &str, ix_start: usize, key: &str) -> Option<usize> {
    line[ix_start..].find(key).map(|v| ix_start + v)
}

pub fn find_from_u8(line: &[u8], ix_start: usize, key: &[u8]) -> Option<usize> {
    assert!(!key.is_empty());
    assert!(ix_start < line.len());
    if line.len() < key.len() {
        return None;
    }
    let ix_end = line.len() - key.len();
    if ix_start <= ix_end {
        for i in ix_start..ix_end {
            if line[i] == key[0] {
                let mut success = true;
                for j in 1..key.len() {
                    if line[i + j] != key[j] {
                        success = false;
                        break;
                    }
                }
                if success {
                    return Some(i);
                }
            }
        }
    }
    None
}

#[test]
fn test_find_from_u8() {
    assert_eq!(find_from_u8(b"hello world", 0, b"hell"), Some(0));
    assert_eq!(find_from_u8(b"hello world", 0, b"o"), Some(4));
    assert_eq!(find_from_u8(b"hello world", 4, b"o"), Some(4));
    assert_eq!(find_from_u8(b"hello world", 5, b"o"), Some(7));
    assert_eq!(find_from_u8(b"hello world", 8, b"o"), None);
    assert_eq!(find_from_u8(b"hello world", 10, b"d"), None);
}
