/// Module providing debug functionality.
module std::debug {
    native public fun print<T>(x: &T);

    native public fun print_stack_trace();

    spec module {
        native fun intrinsic_trace_value<T>(x: &T);
        native fun intrinsic_trace_stack();
    }

    spec print {
        pragma intrinsic = intrinsic_trace_value;
    }

    spec print_stack_trace {
        pragma intrinsic = intrinsic_trace_stack;
    }
}
