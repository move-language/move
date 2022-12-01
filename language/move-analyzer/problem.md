
// TODO 调用下载就崩溃。
// use move_cli::base::reroot_path;
// std::env::set_current_dir("/home/yuyang/projects/test-move");
// build_cfg.download_deps_for_package(&reroot_path(None).unwrap(), &mut std::io::stderr())?;

                     Caused by:
            Unable to find package manifest for 'MoveStdlib' at "Move.toml/./.."', language/move-analyzer/src/completion2.rs:175:28
        stack backtrace:
           0: rust_begin_unwind
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/std/src/panicking.rs:584:5
           1: core::panicking::panic_fmt
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/panicking.rs:142:14
           2: core::result::unwrap_failed
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/result.rs:1785:5
           3: core::result::Result<T,E>::unwrap
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/result.rs:1107:23
           4: move_analyzer::completion2::Modules::new
                     at /home/yuyang/projects/move/language/move-analyzer/src/completion2.rs:175:13
           5: move_analyzer::completion2::xxx
                     at /home/yuyang/projects/move/language/move-analyzer/src/completion2.rs:165:13
           6: move_analyzer::completion2::xxx::{{closure}}
                     at /home/yuyang/projects/move/language/move-analyzer/src/completion2.rs:164:1
           7: core::ops::function::FnOnce::call_once
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/ops/function.rs:248:5
           8: core::ops::function::FnOnce::call_once
                     at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/ops/function.rs:248:5
        note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace.
        test completion2::xxx ... FAILED

Self 看起来可以引入这种。
