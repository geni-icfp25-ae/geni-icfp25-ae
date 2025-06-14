use std::sync::atomic::{AtomicUsize, Ordering};

pub struct IDManager {
    counter: AtomicUsize,
    prefix: &'static str,
}

impl IDManager {
    /// Creates a new fresh variable generator with a given prefix
    pub const fn new(prefix: &'static str) -> Self {
        Self {
            counter: AtomicUsize::new(0),
            prefix,
        }
    }

    /// Returns a fresh variable name
    pub fn next(&self) -> String {
        let id = self.counter.fetch_add(1, Ordering::SeqCst);
        format!("{}{}", self.prefix, id)
    }

    pub fn next_counter(&self) -> usize {
        self.counter.fetch_add(1, Ordering::SeqCst)
    }
}
