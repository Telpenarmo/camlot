use crate::SyntaxKind;

#[derive(Clone, Copy)]
pub(crate) struct TokenSet([u64; 1]);

impl TokenSet {
    pub(crate) const fn new(tokens: &[SyntaxKind]) -> Self {
        let mut set = Self([0; 1]);
        let mut i = 0;
        while i < tokens.len() {
            set.0[0] |= 1 << tokens[i] as u64;
            i += 1;
        }
        set
    }

    pub(crate) const fn union(self, other: Self) -> Self {
        TokenSet([self.0[0] | other.0[0]])
    }

    pub(crate) const fn contains(self, kind: SyntaxKind) -> bool {
        (self.0[0] & (1 << kind as u64)) != 0
    }
}

#[cfg(test)]
mod tests {
    use crate::token_set::TokenSet;

    #[test]
    fn token_set() {
        let set = TokenSet::new(&[super::SyntaxKind::EOF]);
        assert!(set.contains(super::SyntaxKind::EOF));
        assert!(!set.contains(super::SyntaxKind::IDENT));

        let set = TokenSet::new(&[super::SyntaxKind::EOF, super::SyntaxKind::IDENT]);
        assert!(set.contains(super::SyntaxKind::EOF));
        assert!(set.contains(super::SyntaxKind::IDENT));
        assert!(!set.contains(super::SyntaxKind::COLON));
    }

    #[test]
    fn token_set_union() {
        let set = TokenSet::new(&[super::SyntaxKind::EOF])
            .union(TokenSet::new(&[super::SyntaxKind::IDENT]));
        assert!(set.contains(super::SyntaxKind::EOF));
        assert!(set.contains(super::SyntaxKind::IDENT));
        assert!(!set.contains(super::SyntaxKind::ARROW));
    }
}
