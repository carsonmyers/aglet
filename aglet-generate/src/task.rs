use console::style;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::Notify;

pub struct TaskCounter {
    state: Arc<SharedCounterState>,
}

impl TaskCounter {
    pub fn new() -> Self {
        Self {
            state: Arc::new(SharedCounterState {
                count: AtomicUsize::new(0),
                notify: Notify::new(),
            }),
        }
    }

    pub fn count_task(&self) -> CountedTask {
        self.state.count.fetch_add(1, Ordering::SeqCst);

        CountedTask {
            state: self.state.clone(),
        }
    }

    pub async fn wait_finish(&self) {
        self.state.notify.notified().await
    }

    pub fn finished(&self) -> bool {
        self.state.count.load(Ordering::SeqCst) == 0
    }
}

struct SharedCounterState {
    count: AtomicUsize,
    notify: Notify,
}

pub struct CountedTask {
    state: Arc<SharedCounterState>,
}

impl Drop for CountedTask {
    fn drop(&mut self) {
        if self.state.count.fetch_sub(1, Ordering::SeqCst) == 1 {
            self.state.notify.notify_one();
        };
    }
}
