pub mod file_coords;

pub const VERSION_MAJOR: u8 = 0;
pub const VERSION_MINOR: u8 = 1;
pub const BUILD_FILE_PATH: &'static str = "./build/";

#[macro_export]
macro_rules! info {
    ($($arg:tt)*) => (
        println!("[INFO] {}", format_args!($($arg)*));
    )
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => (
        println!("[WARN] {}", format_args!($($arg)*));
    )
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => (
        println!("[ERROR] {}", format_args!($($arg)*));
    )
}
