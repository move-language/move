script {
    fun generic_1<T: copy + drop>(x: T) {
        let _y = x;
    }
}