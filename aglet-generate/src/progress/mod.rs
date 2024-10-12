pub mod data;
mod manager;
pub(crate) mod phase;
pub mod stats;

pub use manager::{Manager, ManagerOptions};
pub use phase::*;

pub const SPINNER_SCAN: &str = "⣿⣾⣶⣵⣭⣫⣛⢟⠿⡿⣿ ";
pub const SPINNER_SCAN_LONG: &str = "⣿⣾⣶⣴⣤⣠⣀⢀⠀⠁⠉⠋⠛⠟⠿⡿⣿ ";
