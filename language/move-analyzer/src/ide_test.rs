use super::completion;
use super::goto_definition;
use super::modules::*;
use crate::utils::path_concat;
use log::{Level, Metadata, Record};
use std::path::PathBuf;
struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Trace
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
        .map(|()| log::set_max_level(log::LevelFilter::Trace))
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
    m.run_full_visitor(&mut v);
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
    m.run_full_visitor(&mut v);
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

    m.run_full_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}

#[test]
fn goto_definition_test4() {
    init_log();
    let m = Modules::new("/Users/yuyang/projects/test-move");
    let mut v =
        goto_definition::Visitor::new("/Users/yuyang/projects/test-move/sources/some.move", 4, 25);
    m.run_full_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}

#[test]
fn completion() {
    init_log();
    let m = Modules::new("/Users/yuyang/projects/test-move");
    let mut v =
        completion::Visitor::new("/Users/yuyang/projects/test-move/sources/some.move", 3, 28);
    m.run_full_visitor(&mut v);
    for x in v.result.unwrap().iter() {
        eprintln!("completion items:{:?} {:?} ", x.label, x.kind)
    }
}

#[test]
fn completion3() {
    init_log();
    let m = Modules::new("/Users/yuyang/projects/aptos-core/aptos-move/framework/aptos-framework");
    let mut v =
        completion::Visitor::new("/Users/yuyang/projects/aptos-core/aptos-move/framework/aptos-framework/sources/account.spec.move", 68, 50);
    m.run_full_visitor(&mut v);
    for x in v.result.unwrap().iter() {
        eprintln!("completion items:{:?} {:?} ", x.label, x.kind)
    }
}

#[test]
fn completion2() {
    init_log();
    let m = Modules::new("/Users/yuyang/projects/test-move2");
    let mut v =
        completion::Visitor::new("/Users/yuyang/projects/test-move2/sources/some.move", 4, 31);
    m.run_full_visitor(&mut v);
    for x in v.result.unwrap().iter() {
        eprintln!("completion items:{:?} {:?} ", x.label, x.kind)
    }
}

#[test]
fn goto_definition_test5() {
    init_log();
    let m = Modules::new("/Users/yuyang/projects/test-move2");
    let mut v =
        goto_definition::Visitor::new("/Users/yuyang/projects/test-move2/sources/some.move", 5, 22);
    m.run_full_visitor(&mut v);
    eprintln!("{:?}", v.result.unwrap());
}
