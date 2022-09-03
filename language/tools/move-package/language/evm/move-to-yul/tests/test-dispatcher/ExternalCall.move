#[contract]
module 0x2::M {
    use Evm::U256::{U256, u256_from_words};
    use Std::Vector;

    #[external(sig=b"noPara()")]
    public native fun no_para(contract: address);

    #[external(sig=b"safeTransferFrom(address,address,uint256,bytes)")]
    public native fun safe_transfer_form(contract: address, from: address, to: address, tokenId: U256, data: vector<u8>);

    #[external(sig=b"isApprovedForAll(address,address)returns(bool)"), view]
    public native fun is_approved_for_all(contract: address, account: address, operator: address): bool;

    #[external(sig=b"multiRet(uint,uint[])returns(uint[], uint)"), view]
    public native fun multi_ret(contract: address, v: U256, vec: vector<U256>): (vector<U256>, U256);

    #[callable]
    fun test_no_para() {
        let contract_addr = @3;
        no_para(contract_addr);
    }

    #[callable]
    fun test_safe_transfer_from(x: u128, y: u128) {
        let contract_addr = @3;
        let from_addr = @4;
        let to_addr = @5;
        let token_id = u256_from_words(x, y);
        let data = Vector::empty<u8>();
        safe_transfer_form(contract_addr, from_addr, to_addr, token_id, data)
    }

    #[callable]
    fun test_is_approved_for_all(): bool {
        let contract_addr = @3;
        let account = @4;
        let operator = @5;
        is_approved_for_all(contract_addr, account, operator)
    }

    #[callable]
    fun test_multi_ret(): (vector<U256>, U256) {
        let contract_addr = @3;
        let v = u256_from_words(0, 0);
        let data = Vector::empty<U256>();
        multi_ret(contract_addr, v, data)
    }

}
