script {
    use TestAccount::BasicCoin;

    fun main(s: signer) {
        BasicCoin::publish_balance(&s);
    }
}
