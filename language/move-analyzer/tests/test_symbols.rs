mod test {
    use move_analyzer::symbols::*;
    use std::{
        collections::{HashMap, BTreeMap, BTreeSet},
        path::{Path, PathBuf},
        sync::{Arc, Mutex},
        thread,
    };
    use move_symbol_pool::Symbol;
    use move_command_line_common::files::FileHash;

    struct UseDefMap(BTreeMap<u32, BTreeSet<UseDef>>);
    impl UseDefMap {
        fn new() -> Self {
            Self(BTreeMap::new())
        }
    
        fn insert(&mut self, key: u32, val: UseDef) {
            self.0.entry(key).or_insert_with(BTreeSet::new).insert(val);
        }
    
        fn get(&self, key: u32) -> Option<BTreeSet<UseDef>> {
            self.0.get(&key).cloned()
        }
    
        fn elements(self) -> BTreeMap<u32, BTreeSet<UseDef>> {
            self.0
        }
    
        fn extend(&mut self, use_defs: BTreeMap<u32, BTreeSet<UseDef>>) {
            self.0.extend(use_defs);
        }
    }
    
    #[cfg(test)]
    fn assert_use_def_with_doc_string(
        mod_symbols: &UseDefMap,
        file_name_mapping: &BTreeMap<FileHash, Symbol>,
        use_idx: usize,
        use_line: u32,
        use_col: u32,
        def_line: u32,
        def_col: u32,
        def_file: &str,
        type_str: &str,
        type_def: Option<(u32, u32, &str)>,
        doc_string: &str,
    ) {
        let uses = mod_symbols.get(use_line).unwrap();
        let use_def = uses.iter().nth(use_idx).unwrap();
        assert!(use_def.get_col_start() == use_col);
        assert!(use_def.get_def_loc_start().line == def_line);
        assert!(use_def.get_def_loc_start().character == def_col);
        assert!(file_name_mapping
            .get(&use_def.get_def_loc_fhash())
            .unwrap()
            .as_str()
            .ends_with(def_file));
        assert!(type_str == format!("{}", use_def.get_use_type()));

        assert!(doc_string == use_def.get_doc_string());
        match use_def.get_type_def_start() {
            Some(type_def_loc) => {
                let tdef_line = type_def.unwrap().0;
                let tdef_col = type_def.unwrap().1;
                assert!(type_def_loc.line == tdef_line);
                assert!(type_def_loc.character == tdef_col);
            }
            None => assert!(type_def.is_none()),
        }
        match use_def.get_type_def_fhash() {
            Some(fhash) => {
                let tdef_file = type_def.unwrap().2;
                assert!(file_name_mapping
                    .get(fhash)
                    .unwrap()
                    .as_str()
                    .ends_with(tdef_file));
            }
            None => assert!(type_def.is_none()),
        }
    }

    #[cfg(test)]
    fn assert_use_def(
        mod_symbols: &UseDefMap,
        file_name_mapping: &BTreeMap<FileHash, Symbol>,
        use_idx: usize,
        use_line: u32,
        use_col: u32,
        def_line: u32,
        def_col: u32,
        def_file: &str,
        type_str: &str,
        type_def: Option<(u32, u32, &str)>,
    ) {
        assert_use_def_with_doc_string(
            mod_symbols,
            file_name_mapping,
            use_idx,
            use_line,
            use_col,
            def_line,
            def_col,
            def_file,
            type_str,
            type_def,
            "",
        )
    }

    #[test]
    /// Tests if symbolication + doc_string information for documented Move constructs is constructed correctly.
    fn docstring_test() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        path.push("tests/symbols");

        let (symbols_opt, _) = Symbolicator::get_symbols(path.as_path()).unwrap();
        let symbols = symbols_opt.unwrap();

        let mut fpath = path.clone();
        fpath.push("sources/M6.move");
        let cpath = dunce::canonicalize(&fpath).unwrap();

        let mod_symbols = symbols.get_file_use_defs(&cpath).unwrap();
        let use_def_map = UseDefMap(mod_symbols.into_iter().map(|(k, v)| (k, v.into_iter().collect())).collect());

        // struct def name
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            4,
            11,
            4,
            11,
            "M6.move",
            "Symbols::M6::DocumentedStruct",
            Some((4, 11, "M6.move")),
            "This is a documented struct\nWith a multi-line docstring\n",
        );

        // const def name
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            10,
            10,
            10,
            10,
            "M6.move",
            "u64",
            None,
            "Constant containing the answer to the universe\n",
        );

        // function def name
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            14,
            8,
            14,
            8,
            "M6.move",
            "fun Symbols::M6::unpack(s: Symbols::M6::DocumentedStruct): u64",
            None,
            "A documented function that unpacks a DocumentedStruct\n",
        );
        // param var (unpack function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            14,
            15,
            14,
            15,
            "M6.move",
            "Symbols::M6::DocumentedStruct",
            Some((4, 11, "M6.move")),
            "A documented function that unpacks a DocumentedStruct\n",
        );
        // struct name in param type (unpack function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            14,
            18,
            4,
            11,
            "M6.move",
            "Symbols::M6::DocumentedStruct",
            Some((4, 11, "M6.move")),
            "This is a documented struct\nWith a multi-line docstring\n",
        );
        // struct name in unpack (unpack function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            15,
            12,
            4,
            11,
            "M6.move",
            "Symbols::M6::DocumentedStruct",
            Some((4, 11, "M6.move")),
            "This is a documented struct\nWith a multi-line docstring\n",
        );
        // field name in unpack (unpack function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            15,
            31,
            6,
            8,
            "M6.move",
            "u64",
            None,
            "A documented field\n",
        );
        // moved var in unpack assignment (unpack function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            15,
            59,
            14,
            15,
            "M6.move",
            "Symbols::M6::DocumentedStruct",
            Some((4, 11, "M6.move")),
            "A documented function that unpacks a DocumentedStruct\n",
        );

        // docstring construction for multi-line /** .. */ based strings
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            26,
            8,
            26,
            8,
            "M6.move",
            "fun Symbols::M6::other_doc_struct(): Symbols::M7::OtherDocStruct",
            Some((3, 11, "M7.move")),
            "\nThis is a multiline docstring\n\nThis docstring has empty lines.\n\nIt uses the ** format instead of ///\n\n",
        );

        // docstring construction for single-line /** .. */ based strings
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            31,
            8,
            31,
            8,
            "M6.move",
            "fun Symbols::M6::acq(addr: address): u64 acquires Symbols::M6::DocumentedStruct",
            None,
            "Asterix based single-line docstring\n",
        );

        /* Test doc_string construction for struct/function imported from another module */

        // other module struct name (other_doc_struct function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            26,
            41,
            3,
            11,
            "M7.move",
            "Symbols::M7::OtherDocStruct",
            Some((3, 11, "M7.move")),
            "Documented struct in another module\n",
        );

        // function name in a call (other_doc_struct function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            27,
            21,
            9,
            15,
            "M7.move",
            "fun Symbols::M7::create_other_struct(v: u64): Symbols::M7::OtherDocStruct",
            Some((3, 11, "M7.move")),
            "Documented initializer in another module\n",
        );

        // const in param (other_doc_struct function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            27,
            41,
            10,
            10,
            "M6.move",
            "u64",
            None,
            "Constant containing the answer to the universe\n",
        );

        // // other documented struct name imported (other_doc_struct_import function)
        assert_use_def_with_doc_string(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            38,
            35,
            3,
            11,
            "M7.move",
            "Symbols::M7::OtherDocStruct",
            Some((3, 11, "M7.move")),
            "Documented struct in another module\n",
        );
    }

    #[test]
    /// Tests if symbolication information for specific Move constructs has been constructed correctly.
    fn symbols_test() {
        let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        path.push("tests/symbols");

        let (symbols_opt, _) = Symbolicator::get_symbols(path.as_path()).unwrap();
        let symbols = symbols_opt.unwrap();

        let mut fpath = path.clone();
        fpath.push("sources/M1.move");
        let cpath = dunce::canonicalize(&fpath).unwrap();

        let mod_symbols = symbols.get_file_use_defs(&cpath).unwrap();
        let use_def_map = UseDefMap(mod_symbols.into_iter().map(|(k, v)| (k, v.into_iter().collect())).collect());
            
        // struct def name
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            2,
            11,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // const def name
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            6,
            10,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // function def name
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            9,
            8,
            9,
            8,
            "M1.move",
            "fun Symbols::M1::unpack(s: Symbols::M1::SomeStruct): u64",
            None,
        );
        // param var (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            9,
            15,
            9,
            15,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // struct name in param type (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            9,
            18,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // struct name in unpack (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            10,
            12,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // field name in unpack (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            10,
            25,
            3,
            8,
            "M1.move",
            "u64",
            None,
        );
        // bound variable in unpack (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            10,
            37,
            10,
            37,
            "M1.move",
            "u64",
            None,
        );
        // moved var in unpack assignment (unpack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            10,
            47,
            9,
            15,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // copied var in an assignment (cp function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            15,
            18,
            14,
            11,
            "M1.move",
            "u64",
            None,
        );
        // struct name return type (pack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            20,
            18,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // struct name in pack (pack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            20,
            18,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // field name in pack (pack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            20,
            31,
            3,
            8,
            "M1.move",
            "u64",
            None,
        );
        // const in pack (pack function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            20,
            43,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // other module struct name (other_mod_struct function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            24,
            41,
            2,
            11,
            "M2.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // function name in a call (other_mod_struct function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            25,
            21,
            6,
            15,
            "M2.move",
            "fun Symbols::M2::some_other_struct(v: u64): Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // const in param (other_mod_struct function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            25,
            39,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // other module struct name imported (other_mod_struct_import function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            30,
            35,
            2,
            11,
            "M2.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // function name (acq function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            34,
            8,
            34,
            8,
            "M1.move",
            "fun Symbols::M1::acq(addr: address): u64 acquires Symbols::M1::SomeStruct",
            None,
        );
        // struct name in acquires (acq function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            34,
            41,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // struct name in builtin type param (acq function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            35,
            32,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // param name in builtin (acq function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            35,
            44,
            34,
            12,
            "M1.move",
            "address",
            None,
        );
        // const in first param (multi_arg_call function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            40,
            22,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // const in second param (multi_arg_call function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            40,
            34,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // function name (vec function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            43,
            8,
            43,
            8,
            "M1.move",
            "fun Symbols::M1::vec(): vector<Symbols::M1::SomeStruct>",
            None,
        );
        // vector constructor type (vec function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            45,
            15,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // vector constructor first element struct type (vec function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            45,
            27,
            2,
            11,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // vector constructor first element struct field (vec function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            45,
            39,
            3,
            8,
            "M1.move",
            "u64",
            None,
        );
        // vector constructor second element var (vec function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            45,
            57,
            44,
            12,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // borrow local (mut function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            56,
            21,
            55,
            12,
            "M1.move",
            "&mut u64",
            None,
        );
        // LHS in mutation statement (mut function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            57,
            9,
            56,
            12,
            "M1.move",
            "&mut u64",
            None,
        );
        // RHS in mutation statement (mut function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            57,
            13,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // function name (ret function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            61,
            8,
            61,
            8,
            "M1.move",
            "fun Symbols::M1::ret(p1: bool, p2: u64): u64",
            None,
        );
        // returned value (ret function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            63,
            19,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // function name (abort_call function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            68,
            8,
            68,
            8,
            "M1.move",
            "fun Symbols::M1::abort_call()",
            None,
        );
        // abort value (abort_call function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            69,
            14,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // dereference (deref function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            75,
            9,
            74,
            12,
            "M1.move",
            "& u64",
            None,
        );
        // unary operator (unary function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            79,
            9,
            78,
            14,
            "M1.move",
            "bool",
            None,
        );
        // temp borrow (temp_borrow function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            83,
            19,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // chain access first element (chain_access function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            94,
            8,
            93,
            12,
            "M1.move",
            "& Symbols::M1::OuterStruct",
            Some((87, 11, "M1.move")),
        );
        // chain second element (chain_access function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            94,
            14,
            88,
            8,
            "M1.move",
            "Symbols::M1::OuterStruct",
            Some((87, 11, "M1.move")),
        );
        // chain access third element (chain_access function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            94,
            26,
            3,
            8,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // chain second element after the block (chain_access_block function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            102,
            10,
            88,
            8,
            "M1.move",
            "Symbols::M1::OuterStruct",
            Some((87, 11, "M1.move")),
        );
        // chain access first element when borrowing (chain_access_borrow function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            108,
            17,
            107,
            12,
            "M1.move",
            "& Symbols::M1::OuterStruct",
            Some((87, 11, "M1.move")),
        );
        // chain second element when borrowing (chain_access_borrow function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            108,
            23,
            88,
            8,
            "M1.move",
            "Symbols::M1::OuterStruct",
            Some((87, 11, "M1.move")),
        );
        // chain access third element when borrowing (chain_access_borrow function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            108,
            35,
            3,
            8,
            "M1.move",
            "Symbols::M1::SomeStruct",
            Some((2, 11, "M1.move")),
        );
        // variable in cast (cast function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            114,
            9,
            113,
            12,
            "M1.move",
            "u128",
            None,
        );
        // constant in an annotation (annot function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            118,
            19,
            6,
            10,
            "M1.move",
            "u64",
            None,
        );
        // struct type param def (struct_param function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            122,
            21,
            122,
            21,
            "M1.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // struct type param use (struct_param function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            123,
            8,
            122,
            21,
            "M1.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // struct type local var def (struct_var function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            127,
            12,
            127,
            12,
            "M1.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );
        // struct type local var use (struct_var function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            129,
            12,
            127,
            12,
            "M1.move",
            "Symbols::M2::SomeOtherStruct",
            Some((2, 11, "M2.move")),
        );

        let mut fpath = path.clone();
        fpath.push("sources/M3.move");
        let cpath = dunce::canonicalize(&fpath).unwrap();

        let mod_symbols = symbols.get_file_use_defs(&cpath).unwrap();
        let use_def_map = UseDefMap(mod_symbols.into_iter().map(|(k, v)| (k, v.into_iter().collect())).collect());

        // generic type in struct definition
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            2,
            23,
            2,
            23,
            "M3.move",
            "T",
            None,
        );
        // generic type in struct field definition
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            3,
            20,
            2,
            23,
            "M3.move",
            "T",
            None,
        );
        // generic type in generic type definition (type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            6,
            23,
            6,
            23,
            "M3.move",
            "T",
            None,
        );
        // parameter (type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            6,
            39,
            6,
            39,
            "M3.move",
            "T",
            None,
        );
        // generic type in param type (type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            3,
            6,
            46,
            6,
            23,
            "M3.move",
            "T",
            None,
        );
        // generic type in return type (type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            4,
            6,
            50,
            6,
            23,
            "M3.move",
            "T",
            None,
        );
        // generic type in struct param type (struct_type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            4,
            10,
            52,
            10,
            30,
            "M3.move",
            "T",
            None,
        );
        // generic type in struct return type (struct_type_param_arg function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            6,
            10,
            69,
            10,
            30,
            "M3.move",
            "T",
            None,
        );
        // generic type in pack (pack_type_param function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            15,
            20,
            14,
            24,
            "M3.move",
            "T",
            None,
        );
        // field type in struct field definition which itself is a struct
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            23,
            20,
            2,
            11,
            "M3.move",
            "Symbols::M3::ParamStruct<T>",
            Some((2, 11, "M3.move")),
        );
        // generic type in struct field definition which itself is a struct
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            2,
            23,
            32,
            22,
            30,
            "M3.move",
            "T",
            None,
        );

        let mut fpath = path.clone();
        fpath.push("sources/M4.move");
        let cpath = dunce::canonicalize(&fpath).unwrap();

        let mod_symbols = symbols.get_file_use_defs(&cpath).unwrap();
        let use_def_map = UseDefMap(mod_symbols.into_iter().map(|(k, v)| (k, v.into_iter().collect())).collect());

        // param name in RHS (if_cond function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            4,
            18,
            2,
            16,
            "M4.move",
            "u64",
            None,
        );
        // param name in RHS (if_cond function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            6,
            22,
            4,
            12,
            "M4.move",
            "u64",
            None,
        );
        // var in if's true branch (if_cond function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            7,
            12,
            4,
            12,
            "M4.move",
            "u64",
            None,
        );
        // redefined var in if's false branch (if_cond function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            10,
            12,
            9,
            16,
            "M4.move",
            "u64",
            None,
        );
        // var name in while loop condition (while_loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            20,
            15,
            18,
            12,
            "M4.move",
            "u64",
            None,
        );
        // var name in while loop's inner block (while_loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            23,
            26,
            18,
            12,
            "M4.move",
            "u64",
            None,
        );
        // redefined var name in while loop's inner block (while_loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            24,
            23,
            23,
            20,
            "M4.move",
            "u64",
            None,
        );
        // var name in while loop's main block (while_loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            26,
            12,
            18,
            12,
            "M4.move",
            "u64",
            None,
        );
        // redefined var name in while loop's inner block (loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            1,
            40,
            23,
            39,
            20,
            "M4.move",
            "u64",
            None,
        );
        // var name in loop's main block (loop function)
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            43,
            16,
            34,
            12,
            "M4.move",
            "u64",
            None,
        );
        // const in a different module in the same file
        assert_use_def(
            &use_def_map,
            &symbols.get_file_name_mapping(),
            0,
            55,
            10,
            55,
            10,
            "M4.move",
            "u64",
            None,
        );
    }
}