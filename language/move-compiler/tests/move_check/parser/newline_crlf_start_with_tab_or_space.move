/// This is a test.
module 0x8675309::M {
    /**
     * One can have /* nested */
     * // block comments
     */
    fun f() { }
    
    /* This is a nested /* regular comment // */ */
    fun g() {}
    
    // This is a line comment which contains unbalanced /* delimiter.
    fun h() {}
}

