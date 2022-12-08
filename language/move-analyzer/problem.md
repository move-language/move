
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

hread 'main' panicked at 'internal error: entered unreachable code: looks like impossible addr:00000000000000000000000000000001 module:"simple_map" item:StructName("SimpleMap") x:struct SimpleMap', language/move-analyzer/src/types.rs:322:26
stack backtrace:
   0: rust_begin_unwind
             at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/std/src/panicking.rs:584:5
   1: core::panicking::panic_fmt
             at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/panicking.rs:142:14
   2: move_analyzer::types::ResolvedType::struct_ref_to_struct::{{closure}}
             at /home/yuyang/projects/move/language/move-analyzer/src/types.rs:322:26
   3: move_analyzer::scopes::Scopes::query_item
             at /home/yuyang/projects/move/language/move-analyzer/src/scopes.rs:67:14
   4: move_analyzer::types::ResolvedType::struct_ref_to_struct
             at /home/yuyang/projects/move/language/move-analyzer/src/types.rs:319:62
   5: move_analyzer::types::ResolvedType::collect_type_parameter_name
             at /home/yuyang/projects/move/language/move-analyzer/src/types.rs:62:35
   6: move_analyzer::scopes::Scopes::resolve_type
             at /home/yuyang/projects/move/language/move-analyzer/src/scopes.rs:376:29
   7: move_analyzer::module_visitor::<impl move_analyzer::modules::Modules>::run_visitor_for_manifest::{{closure}}::{{closure}}
             at /home/yuyang/projects/move/language/move-analyzer/src/module_visitor.rs:69:38
   8: move_analyzer::scopes::Scopes::enter_scope
             at /home/yuyang/projects/move/language/move-analyzer/src/scopes.rs:92:17
   9: move_analyzer::module_visitor::<impl move_analyzer::modules::Modules>::run_visitor_for_manifest::{{closure}}
             at /home/yuyang/projects/move/language/move-analyzer/src/module_visitor.rs:57:13
  10: move_analyzer::modules::Modules::with_struct::{{closure}}
             at /home/yuyang/projects/move/language/move-analyzer/src/modules.rs:217:40
  11: move_analyzer::modules::Modules::with_module_member
             at /home/yuyang/projects/move/language/move-analyzer/src/modules.rs:154:29
  12: move_analyzer::modules::Modules::with_struct
             at /home/yuyang/projects/move/language/move-analyzer/src/modules.rs:216:9
  13: move_analyzer::module_visitor::<impl move_analyzer::modules::Modules>::run_visitor_for_manifest
             at /home/yuyang/projects/move/language/move-analyzer/src/module_visitor.rs:55:9
  14: move_analyzer::module_visitor::<impl move_analyzer::modules::Modules>::run_visitor
             at /home/yuyang/projects/move/language/move-analyzer/src/module_visitor.rs:158:13
  15: move_analyzer::goto_definition::on_go_to_def_request
             at /home/yuyang/projects/move/language/move-analyzer/src/goto_definition.rs:42:5
  16: move_analyzer::on_request
             at /home/yuyang/projects/move/language/move-analyzer/src/bin/move-analyzer.rs:213:13
  17: move_analyzer::main
             at /home/yuyang/projects/move/language/move-analyzer/src/bin/move-analyzer.rs:185:54
  18: core::ops::function::FnOnce::call_once
             at /rustc/897e37553bba8b42751c67658967889d11ecd120/library/core/src/ops/function.rs:248:5
note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace.

不理解的msl语法
Invariant  看起来可以声明范型参数，这个在那使用呢。
Variable   也有范型参数，看起来有点诡异。还有is_global看起来就是在指是不是全局的。
forall 有返回值吗？返回值是最有一个语句？ 返回值只能是true或者false这种bool值？

~~~
forall <binding>, ..., <binding> [ where <exp> ] : <exp>
~~~

//TODO
don't enter type parameter in scope,enter in some fun spec structure.
