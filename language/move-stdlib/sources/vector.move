#[defines_primitive(vector)]
/// A variable-sized container that can hold any type. Indexing is 0-based, and
/// vectors are growable. This module has many native functions.
/// Verification of modules that use this one uses model functions that are implemented
/// directly in Boogie. The specification language has built-in functions operations such
/// as `singleton_vector`. There are some helper functions defined here for specifications in other
/// modules as well.
///
/// >Note: We did not verify most of the
/// Move functions here because many have loops, requiring loop invariants to prove, and
/// the return on investment didn't seem worth it for these simple functions.
module std::vector {

    /// The index into the vector is out of bounds
    const EINDEX_OUT_OF_BOUNDS: u64 = 0x20000;

    #[bytecode_instruction]
    /// Create an empty vector.
    native public fun empty<Element>(): vector<Element>;

    #[bytecode_instruction]
    /// Return the length of the vector.
    native public fun length<Element>(self: &vector<Element>): u64;

    #[bytecode_instruction]
    /// Acquire an immutable reference to the `i`th element of the vector `self`.
    /// Aborts if `i` is out of bounds.
    native public fun borrow<Element>(self: &vector<Element>, i: u64): &Element;

    #[bytecode_instruction]
    /// Add element `e` to the end of the vector `self`.
    native public fun push_back<Element>(self: &mut vector<Element>, e: Element);

    #[bytecode_instruction]
    /// Return a mutable reference to the `i`th element in the vector `self`.
    /// Aborts if `i` is out of bounds.
    native public fun borrow_mut<Element>(self: &mut vector<Element>, i: u64): &mut Element;

    #[bytecode_instruction]
    /// Pop an element from the end of vector `self`.
    /// Aborts if `self` is empty.
    native public fun pop_back<Element>(self: &mut vector<Element>): Element;

    #[bytecode_instruction]
    /// Destroy the vector `self`.
    /// Aborts if `self` is not empty.
    native public fun destroy_empty<Element>(self: vector<Element>);

    #[bytecode_instruction]
    /// Swaps the elements at the `i`th and `j`th indices in the vector `self`.
    /// Aborts if `i` or `j` is out of bounds.
    native public fun swap<Element>(self: &mut vector<Element>, i: u64, j: u64);

    /// Return an vector of size one containing element `e`.
    public fun singleton<Element>(e: Element): vector<Element> {
        let v = empty();
        v.push_back(e);
        v
    }
    spec singleton {
        // TODO: when using opaque here, we get verification errors.
        // pragma opaque;
        aborts_if false;
        ensures result == vec(e);
    }

    /// Reverses the order of the elements in the vector `self` in place.
    public fun reverse<Element>(self: &mut vector<Element>) {
        let len = length(self);
        if (len == 0) return ();

        let front_index = 0;
        let back_index = len -1;
        while (front_index < back_index) {
            self.swap(front_index, back_index);
            front_index = front_index + 1;
            back_index = back_index - 1;
        }
    }
    spec reverse {
        pragma intrinsic = true;
    }


    /// Pushes all of the elements of the `other` vector into the `self` vector.
    public fun append<Element>(self: &mut vector<Element>, other: vector<Element>) {
        other.reverse();
        while (!is_empty(&other)) self.push_back(pop_back(&mut other));
        destroy_empty(other);
    }
    spec append {
        pragma intrinsic = true;
    }
    spec is_empty {
        pragma intrinsic = true;
    }


    /// Return `true` if the vector `self` has no elements and `false` otherwise.
    public fun is_empty<Element>(self: &vector<Element>): bool {
        self.length() == 0
    }

    /// Return true if `e` is in the vector `self`.
    /// Otherwise, returns false.
    public fun contains<Element>(self: &vector<Element>, e: &Element): bool {
        let i = 0;
        let len = self.length();
        while (i < len) {
            if (self.borrow(i) == e) return true;
            i = i + 1;
        };
        false
    }
    spec contains {
        pragma intrinsic = true;
    }

    /// Return `(true, i)` if `e` is in the vector `self` at index `i`.
    /// Otherwise, returns `(false, 0)`.
    public fun index_of<Element>(self: &vector<Element>, e: &Element): (bool, u64) {
        let i = 0;
        let len = self.length();
        while (i < len) {
            if (self.borrow(i) == e) return (true, i);
            i = i + 1;
        };
        (false, 0)
    }
    spec index_of {
        pragma intrinsic = true;
    }

    /// Remove the `i`th element of the vector `self`, shifting all subsequent elements.
    /// This is O(n) and preserves ordering of elements in the vector.
    /// Aborts if `i` is out of bounds.
    public fun remove<Element>(self: &mut vector<Element>, i: u64): Element {
        let len = self.length();
        // i out of bounds; abort
        if (i >= len) abort EINDEX_OUT_OF_BOUNDS;

        len = len - 1;
        while (i < len) self.swap(i, { i = i + 1; i });
        self.pop_back()
    }
    spec remove {
        pragma intrinsic = true;
    }

    /// Insert `e` at position `i` in the vector `self`.
    /// If `i` is in bounds, this shifts the old `self[i]` and all subsequent elements to the right.
    /// If `i == length(self)`, this adds `e` to the end of the vector.
    /// This is O(n) and preserves ordering of elements in the vector.
    /// Aborts if `i > length(self)`
    public fun insert<Element>(self: &mut vector<Element>, e: Element, i: u64) {
        let len = self.length();
        // i too big abort
        if (i > len) abort EINDEX_OUT_OF_BOUNDS;

        self.push_back(e);
        while (i < len) {
            self.swap(i, len);
            i = i + 1
        }
    }
    spec insert {
        pragma intrinsic = true;
    }

    /// Swap the `i`th element of the vector `self` with the last element and then pop the vector.
    /// This is O(1), but does not preserve ordering of elements in the vector.
    /// Aborts if `i` is out of bounds.
    public fun swap_remove<Element>(self: &mut vector<Element>, i: u64): Element {
        assert!(!self.is_empty(), EINDEX_OUT_OF_BOUNDS);
        let last_idx = self.length() - 1;
        self.swap(i, last_idx);
        self.pop_back()
    }
    spec swap_remove {
        pragma intrinsic = true;
    }

    // =================================================================
    // Module Specification

    spec module {} // Switch to module documentation context

    /// # Helper Functions

    spec module {
        /// Check if `v1` is equal to the result of adding `e` at the end of `v2`
        fun eq_push_back<Element>(v1: vector<Element>, v2: vector<Element>, e: Element): bool {
            len(v1) == len(v2) + 1 &&
            v1[len(v1)-1] == e &&
            v1[0..len(v1)-1] == v2[0..len(v2)]
        }

        /// Check if `v` is equal to the result of concatenating `v1` and `v2`
        fun eq_append<Element>(v: vector<Element>, v1: vector<Element>, v2: vector<Element>): bool {
            len(v) == len(v1) + len(v2) &&
            v[0..len(v1)] == v1 &&
            v[len(v1)..len(v)] == v2
        }

        /// Check `v1` is equal to the result of removing the first element of `v2`
        fun eq_pop_front<Element>(v1: vector<Element>, v2: vector<Element>): bool {
            len(v1) + 1 == len(v2) &&
            v1 == v2[1..len(v2)]
        }

        /// Check that `v1` is equal to the result of removing the element at index `i` from `v2`.
        fun eq_remove_elem_at_index<Element>(i: u64, v1: vector<Element>, v2: vector<Element>): bool {
            len(v1) + 1 == len(v2) &&
            v1[0..i] == v2[0..i] &&
            v1[i..len(v1)] == v2[i + 1..len(v2)]
        }
    }
}
