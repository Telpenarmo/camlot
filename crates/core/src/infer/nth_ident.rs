pub(super) fn nth_ident(mut n: u16) -> String {
    let mut chars: Vec<u8> = Vec::new();
    while n >= 26 {
        let offset = n % 26;
        let offset8: u8 = offset.try_into().unwrap();
        chars.push(b'a' + offset8);
        n -= offset;
        n /= 26;
        n -= 1;
    }
    let offset8: u8 = n.try_into().unwrap();
    chars.push(b'a' + offset8);
    chars.reverse();
    String::from_utf8(chars).unwrap()
}

#[cfg(test)]
mod tests {
    use super::nth_ident;

    #[test]
    fn nth_ident_tests() {
        assert_eq!(nth_ident(0), "a");
        assert_eq!(nth_ident(1), "b");
        assert_eq!(nth_ident(25), "z");
        assert_eq!(nth_ident(26), "aa");
        assert_eq!(nth_ident(27), "ab");
        assert_eq!(nth_ident(2 * 26), "ba");
        assert_eq!(nth_ident(2 * 26 + 1), "bb");
    }

    #[test]
    fn nth_ident_returns_different_values() {
        let mut seen = std::collections::HashSet::new();

        for i in 0..1000 {
            assert!(seen.insert(nth_ident(i)));
        }
    }
}
