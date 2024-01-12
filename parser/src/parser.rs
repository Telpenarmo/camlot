use crate::{event::Event, source::Source, SyntaxKind};

#[allow(dead_code)]
pub(crate) struct Parser<'t, 'input> {
    source: Source<'t, 'input>,
    events: Vec<Event>,
}

/// Transform the source into events
impl<'t, 'input> Parser<'t, 'input> {
    pub fn new(source: Source<'t, 'input>) -> Self {
        Self {
            source,
            events: Vec::new(),
        }
    }
    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    /// Open a new node
    pub fn open(&mut self) -> Marker {
        let pos = self.events.len();
        self.events.push(Event::UnmatchedOpen);
        Marker::new(pos)
    }

    /// Close a node
    pub fn close(&mut self, marker: Marker, kind: SyntaxKind) -> CompletedMarker {
        let event_at_pos = &mut self.events[marker.pos];
        assert_eq!(*event_at_pos, Event::UnmatchedOpen);

        *event_at_pos = Event::Open {
            kind,
            forward_parent: None,
        };

        self.events.push(Event::Close);

        marker.complete()
    }

    /// Open a new node above the given marker's node
    pub fn open_before(&mut self, marker: CompletedMarker) -> Marker {
        let new_m = self.open();

        if let Event::Open {
            ref mut forward_parent,
            ..
        } = self.events[marker.pos]
        {
            *forward_parent = Some(new_m.pos - marker.pos);
        } else {
            unreachable!("Expected open event at {}", marker.pos);
        }

        new_m
    }

    /// Check if the current token is the given kind
    pub fn at(&mut self, kind: SyntaxKind) -> bool {
        self.current() == kind
    }

    /// Get the current token, ignoring trivia
    pub fn current(&mut self) -> SyntaxKind {
        self.source.current()
    }

    /// Advance to the next token
    pub fn advance(&mut self) {
        self.source.bump();
        self.events.push(Event::Advance);
    }

    /// Advance if the current token is the given kind
    pub fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.at(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Advance if the current token is the given kind, otherwise emit an error
    pub fn expect(&mut self, kind: SyntaxKind) {
        if self.eat(kind) {
            return;
        }

        let msg = format!("Expected {:#?} but found {:?}", kind, self.current());
        eprintln!("{}", msg);
        self.error(msg)
    }

    pub fn error(&mut self, message: String) {
        self.events.push(Event::Error(message));
    }
}

#[must_use]
pub(crate) struct Marker {
    pos: usize,
    completed: bool,
}

impl Marker {
    fn new(pos: usize) -> Self {
        Self {
            pos,
            completed: false,
        }
    }

    fn complete(mut self) -> CompletedMarker {
        self.completed = true;

        CompletedMarker { pos: self.pos }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        if !self.completed {
            panic!("Marker dropped without completion")
        }
    }
}

pub(crate) struct CompletedMarker {
    pub pos: usize,
}

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_nothing() {
        check(
            crate::PrefixEntryPoint::Module,
            "",
            expect![[r#"
            MODULE@0..0
        "#]],
        );
    }

    #[test]
    fn parse_whitespace() {
        check(
            crate::PrefixEntryPoint::Module,
            "   ",
            expect![[r#"
                MODULE@0..3
                  WHITESPACE@0..3 "   "
            "#]],
        );
    }
}
