use std::{cell::Cell, ops::Range};

use crate::SyntaxKind;

pub struct Token<'input> {
    pub kind: SyntaxKind,
    pub text: &'input str,
    pub range: Range<usize>,
}

pub(crate) struct Source<'t, 'input> {
    tokens: &'t [Token<'input>],
    cursor: usize,
    fuel: Cell<u32>,
}

impl<'t, 'input> Source<'t, 'input> {
    pub fn new(tokens: &'t [Token<'input>]) -> Self {
        Self {
            tokens,
            cursor: 0,
            fuel: Cell::new(256),
        }
    }

    pub fn bump(&mut self) {
        assert!(self.current() != SyntaxKind::EOF);
        self.fuel.set(256);
        self.cursor += 1;
    }

    pub fn current(&mut self) -> SyntaxKind {
        if self.fuel.get() == 0 {
            panic!("parser is stuck")
        }

        self.fuel.set(self.fuel.get() - 1);

        self.eat_trivia();
        self.peek_kind_raw().unwrap_or(SyntaxKind::EOF)
    }

    // pub fn peek_token(&mut self) -> &Token {
    //     self.eat_trivia();
    //     self.peek_token_raw().unwrap_or_default()
    // }

    fn eat_trivia(&mut self) {
        while self.at_trivia() {
            self.cursor += 1;
        }
    }

    fn at_trivia(&self) -> bool {
        self.peek_kind_raw()
            .map_or(false, |kind| kind == SyntaxKind::WHITESPACE)
    }

    // pub fn last_token_range(&self) -> Option<&Range<usize>> {
    //     self.tokens.last().map(|Token { range, .. }| range)
    // }

    fn peek_kind_raw(&self) -> Option<SyntaxKind> {
        self.peek_token_raw().map(|Token { kind, .. }| *kind)
    }

    fn peek_token_raw(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }
}
