use super::goto_definition;
use super::modules::*;
use super::utils::*;

use log::{Level, Metadata, Record};

struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Trace
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}
const LOGGER: SimpleLogger = SimpleLogger;

pub fn init_log() {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(log::LevelFilter::Trace))
        .unwrap()
}

#[test]

fn goto_definition_test() {
    init_log();
    let mut m = Modules::new("./tests");
    let mut v = goto_definition::Visitor::new("./tests/sources/test.move", 2, 22);
    m.run_visitor(&mut v);
    println!("{:?}", v.result.unwrap());
}
