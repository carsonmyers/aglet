pub(crate) struct StateStack {
    stack: Vec<StateFlags>,
}

impl StateStack {
    pub fn new() -> Self {
        StateStack { stack: vec![StateFlags::default()] }
    }

    pub fn push(&mut self, state: State) {
        let flags = self.stack.last()
            .map(|sf| sf.flags.clone())
            .unwrap_or(Flags::default());

        self.stack.push(StateFlags{state, flags});
    }

    pub fn swap(&mut self, state: State) {
        let flags = self.stack.pop()
            .map(|sf| sf.flags)
            .expect("no state on tokenizer stack to swap with");

        self.stack.push(StateFlags{state, flags});
    }

    pub fn pop(&mut self) {
        if self.stack.len() == 1 {
            panic!("popped final state off tokenizer stack");
        }

        self.stack.pop();
    }

    pub fn flags(&self) -> &Flags {
        let top = self.stack.last()
            .expect("no states on tokenizer stack");

        &top.flags
    }

    pub fn flags_mut(&mut self) -> &mut Flags {
        let top = self.stack.last_mut()
            .expect("no states on tokenizer stack");

        &mut top.flags
    }

    pub fn get(&self) -> &State {
        self.stack.last()
            .map(|sf| &sf.state)
            .expect("tokenizer state stack is empty")
    }
}

#[derive(Clone, Copy, PartialEq)]
pub(crate) enum State {
    Main,
    Group,
    Class,
    ClassName,
    Range,
    UnicodeProperties,
}

impl Default for State {
    fn default() -> Self {
        State::Main
    }
}

#[derive(Default, Clone, Copy)]
pub(crate) struct Flags {
    pub(crate) ignore_space: bool,
}

#[derive(Default)]
pub(crate) struct StateFlags {
    state: State,
    flags: Flags,
}