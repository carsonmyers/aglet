use std::fmt;

use crate::tokenize::error::StateError;

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

    pub fn swap(&mut self, state: State) -> Result<State, StateError> {
        let former = self.stack.pop()
            .ok_or(StateError::NoStateOnStack)?;
        let flags = former.flags.clone();

        self.stack.push(StateFlags{state, flags});
        Ok(former.state)
    }

    pub fn pop(&mut self) -> Result<State, StateError> {
        if self.stack.len() == 1 {
            return Err(StateError::PoppedFinalState);
        }

        self.stack.pop()
            .map(|sf| sf.state)
            .ok_or(StateError::NoStateOnStack)
    }

    pub fn flags(&self) -> Result<&Flags, StateError> {
        let top = self.stack.last()
            .ok_or(StateError::NoStateOnStack)?;

        Ok(&top.flags)
    }

    pub fn flags_mut(&mut self) -> Result<&mut Flags, StateError> {
        let top = self.stack.last_mut()
            .ok_or(StateError::NoStateOnStack)?;

        Ok(&mut top.flags)
    }

    pub fn get(&self) -> Result<&State, StateError> {
        self.stack.last()
            .map(|sf| &sf.state)
            .ok_or(StateError::NoStateOnStack)
    }
}

impl fmt::Debug for StateStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();
        self.stack.iter()
            .for_each(|sf| {
                list.entry(&sf);
            });

        list.finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

impl fmt::Debug for Flags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = f.debug_list();
        if self.ignore_space {
            list.entry(&'x');
        }

        list.finish()
    }
}

#[derive(Default)]
pub(crate) struct StateFlags {
    state: State,
    flags: Flags,
}

impl fmt::Debug for StateFlags {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("State")
            .field(&self.state)
            .field(&self.flags)
            .finish()
    }
}