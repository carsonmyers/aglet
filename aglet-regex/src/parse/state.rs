use crate::parse::error::StateError;

pub(crate) struct StateStack {
    pub(crate) stack: Vec<State>,
}

impl StateStack {
    pub(crate) fn new() -> Self {
        StateStack {
            stack: vec![State::default()],
        }
    }

    pub(crate) fn push(&mut self, state: State) { self.stack.push(state); }

    pub(crate) fn pop(&mut self) -> Result<State, StateError> {
        if self.stack.len() == 1 {
            return Err(StateError::PoppedFinalState);
        }

        self.stack.pop().ok_or(StateError::NoStateOnStack)
    }

    pub(crate) fn get(&self) -> Result<&State, StateError> {
        self.stack.last().ok_or(StateError::NoStateOnStack)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum State {
    Main,
    Group,
    Class,
    Range,
}

impl Default for State {
    fn default() -> Self { State::Main }
}
