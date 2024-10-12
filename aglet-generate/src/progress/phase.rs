use crate::progress;
use crate::progress::data;
use indicatif::{ProgressBar, ProgressStyle};

pub trait Phase: Send + Sync {
    fn init(&self) -> Vec<ProgressBar> {
        let style = ProgressStyle::default_spinner().tick_chars(progress::SPINNER_SCAN);
        vec![ProgressBar::new_spinner().with_style(style)]
    }

    fn tick(&mut self) {}

    fn update_msg(&self, #[allow(unused)] index: usize) -> Option<String> {
        None
    }

    fn update_percent(&self, #[allow(unused)] index: usize) -> Option<data::PercentComplete> {
        None
    }

    fn finish(&self) -> Option<String> {
        None
    }
}

pub struct SimplePhase(String, Option<String>);

impl SimplePhase {
    pub fn new(msg: String, finish: Option<String>) -> Self {
        Self(msg, finish)
    }
}

impl Phase for SimplePhase {
    fn init(&self) -> Vec<ProgressBar> {
        let style = ProgressStyle::default_spinner().tick_chars(progress::SPINNER_SCAN);
        vec![ProgressBar::new_spinner()
            .with_style(style)
            .with_message(self.0.clone())]
    }

    fn finish(&self) -> Option<String> {
        self.1.clone()
    }
}

pub fn phase<S: Into<String>>(msg: S) -> Box<dyn Phase> {
    Box::new(SimplePhase::new(msg.into(), None))
}
