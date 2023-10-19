script {
    fun empty_loop() {
        let iterations: u64 = 10;

        while (iterations > 0) {
            iterations = iterations - 1;
        }
    }
}

script {
    fun empty_loop_param(iterations: u64) {
        while (iterations > 0) {
            iterations = iterations - 1;
        }
    }
}
