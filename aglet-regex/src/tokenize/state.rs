pub(crate) struct StateStack {
    stack: Vec<State>,
}

impl StateStack {
    pub fn new() -> Self {
        StateStack { stack: vec![State::Main] }
    }

    pub fn push(&mut self, state: State) {
        self.stack.push(state);
    }

    pub fn swap(&mut self, state: State) {
        self.stack.pop();
        self.stack.push(state);
    }

    pub fn pop(&mut self) {
        if self.stack.len() == 1 {
            panic!("popped final state off tokenizer stack");
        }

        self.stack.pop();
    }

    pub fn get(&self) -> &State {
        self.stack.last()
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