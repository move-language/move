use super::goto_definition;
use super::modules::*;

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
    let m = Modules::new("./tests/goto_definition/");
    let mut v = goto_definition::Visitor::new(
        "/home/yuyang/projects/move/language/move-analyzer/tests/goto_definition/sources/test.move",
        1,
        25,
    );
    m.run_visitor(&mut v);
    println!("{:?}", v.result.unwrap());
}
