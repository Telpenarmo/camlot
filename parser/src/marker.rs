#[must_use]
pub(crate) struct Marker {
    pub pos: usize,
    completed: bool,
}

impl Marker {
    pub(crate) fn new(pos: usize) -> Self {
        Self {
            pos,
            completed: false,
        }
    }

    pub(crate) fn complete(mut self) -> CompletedMarker {
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
