use crate::event::{ErrorPlacement, ParseError};
use crate::source::Token;
use crate::{event::Event, RideMLLanguage};
use crate::{Parse, SyntaxError};
use rowan::{GreenNodeBuilder, Language};
use std::mem;

pub(crate) struct Sink<'t, 'input> {
    builder: GreenNodeBuilder<'static>,
    tokens: &'t [Token<'input>],
    cursor: usize,
    prev: usize,
    events: Vec<Event>,
    errors: Vec<SyntaxError>,
}

impl<'t, 'input> Sink<'t, 'input> {
    pub(crate) fn new(tokens: &'t [Token<'input>], events: Vec<Event>) -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
            tokens,
            cursor: 0,
            prev: 0,
            events,
            errors: Vec::new(),
        }
    }

    pub(crate) fn finish(mut self) -> Parse {
        for idx in 0..self.events.len() {
            match mem::replace(&mut self.events[idx], Event::UnmatchedOpen) {
                Event::Open {
                    kind,
                    forward_parent,
                } => {
                    let mut kinds = vec![kind];

                    let mut idx = idx;
                    let mut forward_parent = forward_parent;

                    // Walk through the forward parent of the forward parent and the forward parent
                    // of that, and of that, etc. until we reach a StartNode event without a forward
                    // parent.
                    while let Some(fp) = forward_parent {
                        idx += fp;

                        forward_parent = if let Event::Open {
                            kind,
                            forward_parent,
                        } =
                            mem::replace(&mut self.events[idx], Event::UnmatchedOpen)
                        {
                            kinds.push(kind);
                            forward_parent
                        } else {
                            unreachable!()
                        };
                    }

                    for kind in kinds.into_iter().rev() {
                        self.builder.start_node(RideMLLanguage::kind_to_raw(kind));
                    }
                }
                Event::Advance => {
                    self.prev = self.cursor;
                    self.token();
                }
                Event::Close => self.builder.finish_node(),
                Event::Error(error) => self.error(error),
                Event::UnmatchedOpen => {}
            }

            self.eat_trivia();
        }

        Parse {
            green_node: self.builder.finish(),
            errors: self.errors,
        }
    }

    fn eat_trivia(&mut self) {
        while let Some(token) = self.tokens.get(self.cursor) {
            if !token.kind.is_trivial() {
                break;
            }

            self.token();
        }
    }

    fn token(&mut self) {
        let Token { kind, text, .. } = self.tokens[self.cursor];

        self.builder.token(RideMLLanguage::kind_to_raw(kind), text);

        self.cursor += 1;
    }

    fn error(&mut self, err: ParseError) {
        fn get_range(s: &Sink<'_, '_>, err_pos: ErrorPlacement) -> std::ops::Range<usize> {
            match err_pos {
                ErrorPlacement::PrevTokenEnd => {
                    if let Some(Token { range, .. }) = s.tokens.get(s.prev) {
                        range.end..range.end
                    } else {
                        0..0
                    }
                }
                ErrorPlacement::NextToken => {
                    if let Some(Token { range, .. }) = s.tokens.get(s.cursor) {
                        range.clone()
                    } else {
                        let end = s.tokens.last().unwrap().range.end;
                        end..end
                    }
                }
            }
        }

        let range = get_range(self, err.location);
        self.errors.push(SyntaxError {
            message: err.message,
            range,
        });
    }
}
