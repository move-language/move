/// Utilities for working with global memory.
module std::globals {

    /// Tests whether the resource exists in global memory.
    public native fun exists_at<R:key>(addr: address): bool;
}
