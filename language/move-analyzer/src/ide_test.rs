use std::path::PathBuf;

use crate::utils::path_concat;

use super::goto_definition;
use super::modules::*;

use log::{Level, Metadata, Record};

struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }
    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            eprintln!("{} - {}", record.level(), record.args());
        }
    }
    fn flush(&self) {}
}

const LOGGER: SimpleLogger = SimpleLogger;

pub fn init_log() {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(log::LevelFilter::Info))
        .unwrap()
}

fn concat_current_working_dir(s: &str) -> PathBuf {
    path_concat(
        std::env::current_dir().unwrap().as_path(),
        PathBuf::from(s).as_path(),
    )
}

#[test]
fn goto_definition_test() {
    init_log();
    let m = Modules::new(concat_current_working_dir("./tests/goto_definition"));
    let mut v = goto_definition::Visitor::new(
        concat_current_working_dir("./tests/goto_definition/sources/test.move"),
        1,
        21,
    );
    m.run_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}

#[test]
fn goto_definition_test3() {
    init_log();
    let m = Modules::new(concat_current_working_dir("./tests/goto_definition"));
    let mut v = goto_definition::Visitor::new(
        concat_current_working_dir("./tests/goto_definition/sources/test.move"),
        1,
        21,
    );
    m.run_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}

#[test]
fn goto_definition_test4() {
    init_log();
    let m = Modules::new("/home/yuyang/projects/test-move2");
    let mut v =
        goto_definition::Visitor::new("/home/yuyang/projects/test-move2/sources/test.move", 4, 110);
    m.run_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}

#[test]
fn goto_definition_test2() {
    init_log();
    let m = Modules::new(concat_current_working_dir(
        "/home/yuyang/projects/test-move",
    ));
    let mut v = goto_definition::Visitor::new(
        concat_current_working_dir("/home/yuyang/projects/test-move/sources/Hello.move"),
        119,
        21,
    );
    m.run_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}
