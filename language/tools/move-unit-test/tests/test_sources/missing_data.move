module 0x1::MissingData {
    struct Missing has key { }

    #[test]
    fun missing_data() acquires Missing {
        borrow_global<Missing>(@0x0);
    }

    #[test]
    fun missing_data_from_other_function() acquires Missing {
        // This call should create a stack trace entry
        missing_data()
    }
}
