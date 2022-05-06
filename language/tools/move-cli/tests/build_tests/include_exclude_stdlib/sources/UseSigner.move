#[evm_contract] // for passing evm test flavor
module 0x1::Example {
  use Std::Signer;

  public fun f(account: &signer): address {
    Signer::address_of(account)
  }
}
