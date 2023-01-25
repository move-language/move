/// Represent references to global memory which can freely passed around functions. In core Move, references
/// like `&R` and `&mut R` can only be passed downwards the stack if obtained from global memory. The types here
/// relax this restriction, for the cost deferring some checks to runtime instead of compile time.
module std::global {

    /// Represents an immutable reference to a global. This value can be copied and dropped but not stored.
    struct Ref<phantom T> has drop, copy {
        handle: u64
    }

    /// Represents a mutable reference to global. This value is linear and cannot be stored.
    struct RefMut<phantom T> has drop {
        handle: u64
    }

    /// Borrows the resource at `addr` and returns a `Ref<T>` for it.
    /// - This function will not type check if there is an active `&T` (indicated by the informal acquires)
    /// - This function aborts if there is any active `RefMut<T>` for this location.
    public native fun borrow<T:key>(addr: address): Ref<T> /* acquires T */;

    /// Exclusively borrows the resource at `addr` and returns `RefMut<T>` for it.
    /// - This function will not type check if there is an active `&T` (indicated by the informal acquires)
    /// - This function aborts if there is any active `Ref<T>` or `RefMut<T>` for this location.
    public native fun borrow_mut<T:key>(addr: address): RefMut<T> /* acquires T */;

    /// Returns a native reference for this `Ref<T>` type.
    /// - This function will not type check if there is an active `&T` (indicated by the informal acquires)
    /// - This function always succeeds.
    public native fun ref<T>(rc: Ref<T>): &T /* acquires T*/;

    /// Returns a mutable native reference for this `RefMut<T>` type, consumging it.
    /// - This function will not type check if there is an active `&T` (indicated by the informal acquires)
    /// - This function always succeeds.
    public native fun ref_mut<T>(rc: &RefMut<T>): &mut T /* acquires T */;
}
