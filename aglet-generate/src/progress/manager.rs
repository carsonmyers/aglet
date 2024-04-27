use chrono::{DateTime, Utc};
use console::style;
use indicatif::{MultiProgress, ProgressBar};
use tokio::sync::mpsc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::task::JoinHandle;
use tokio::time::{sleep, Duration};

use crate::progress::phase::{phase, Phase};

pub struct Manager {
    start_time: DateTime<Utc>,
    tx:         UnboundedSender<Message>,
    handle:     Option<JoinHandle<()>>,
}

impl Manager {
    pub fn new() -> Self {
        let start_time = Utc::now();

        let (tx, rx) = mpsc::unbounded_channel::<Message>();
        let handle = Some(tokio::spawn(run_progress(rx)));

        Self {
            start_time,
            tx,
            handle,
        }
    }

    pub fn phase(&self, phase: Box<dyn Phase>) -> eyre::Result<()> {
        self.tx.send(Message::NewPhase(phase))?;
        Ok(())
    }

    pub fn pause(&self) -> eyre::Result<()> {
        self.tx.send(Message::Pause)?;
        Ok(())
    }

    pub async fn finish(mut self) -> eyre::Result<()> {
        let Some(handle) = self.handle.take() else {
            return Ok(());
        };

        let end_time = Utc::now();
        self.tx.send(Message::Finish(end_time - self.start_time))?;

        handle.await?;
        Ok(())
    }

    pub fn shutdown(&mut self) -> eyre::Result<()> {
        let Some(_) = self.handle.take() else {
            return Ok(());
        };

        self.tx.send(Message::Shutdown)?;

        Ok(())
    }
}

impl Drop for Manager {
    fn drop(&mut self) {
        self.shutdown().unwrap_or_else(|err| {
            eprintln!("error shutting down progress handler during drop: {}", err)
        });
    }
}

enum Message {
    NewPhase(Box<dyn Phase>),
    Pause,
    Finish(chrono::Duration),
    Shutdown,
}

async fn run_progress(mut rx: UnboundedReceiver<Message>) {
    let mut state = ProgressState::new();

    loop {
        state.update();

        tokio::select! {
            Some(message) = rx.recv() => match message {
                Message::NewPhase(phase) => state.next_phase(phase),
                Message::Pause => state.pause(),
                Message::Finish(duration) => {
                    state.finish(duration);
                    break;
                },
                Message::Shutdown => break,
            },
            _ = sleep(Duration::from_millis(100)) => continue,
            else => break,
        }
    }

    state.clear();
    rx.close();
}

struct ProgressState {
    phase: Option<Box<dyn Phase>>,
    bars:  Vec<ProgressBar>,
    group: MultiProgress,
}

impl ProgressState {
    fn new() -> Self {
        Self {
            phase: None,
            bars:  Vec::new(),
            group: MultiProgress::new(),
        }
    }

    fn finish_phase(&mut self) {
        let Some(phase) = &self.phase else {
            return;
        };

        let Some(msg) = phase.finish() else {
            return;
        };

        eprintln!("\u{2713} {}", style(msg).bright().green());
    }

    fn next_phase(&mut self, phase: Box<dyn Phase>) {
        self.clear();
        self.finish_phase();

        for bar in phase.init() {
            self.bars.push(self.group.add(bar));
        }

        self.phase = Some(phase);
    }

    fn pause(&mut self) {
        self.clear();
        self.finish_phase();

        self.phase = None;
    }

    fn update(&mut self) {
        let Some(ref mut phase) = &mut self.phase else {
            return;
        };

        phase.tick();
        for (i, bar) in self.bars.iter().enumerate() {
            if let Some(msg) = phase.update_msg(i) {
                bar.set_message(msg);
            }

            if let Some(percent) = phase.update_percent(i) {
                bar.set_position(percent.0);
                bar.set_length(percent.1);
            }

            bar.tick();
        }
    }

    fn finish(&mut self, duration: chrono::Duration) {
        self.clear();
        self.finish_phase();

        if duration.num_seconds() < 5 {
            return;
        }

        let formatted = [
            duration.num_hours(),
            duration.num_minutes() % 60,
            duration.num_seconds() % 60,
        ]
        .into_iter()
        .filter(|seg| *seg > 0)
        .map(|seg| format!("{}", seg))
        .collect::<Vec<_>>();

        let formatted = match formatted.len() {
            0 => "0s".to_string(),
            1 => format!("{}s", formatted[0]),
            _ => formatted.join(":"),
        };

        eprintln!(
            "\u{2713} {}",
            style(format!("Finished in {}", formatted)).bright().green()
        );
    }

    fn clear(&mut self) {
        self.group.clear().expect("clear progress group");
        self.bars.clear();
    }
}
