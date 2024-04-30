/// Module managing dual attestation.
module DiemFramework::DualAttestation {
    use DiemFramework::CoreAddresses;
    use DiemFramework::XDX::XDX;
    use DiemFramework::Diem;
    use DiemFramework::DiemTimestamp;
    use DiemFramework::Roles;
    use DiemFramework::Signature;
    use DiemFramework::VASP;
    use std::bcs;
    use std::errors;
    use std::event::{Self, EventHandle};
    use std::signer;
    use std::vector;
    friend DiemFramework::DiemAccount;

    /// This resource holds an entity's globally unique name and all of the metadata it needs to
    /// participate in off-chain protocols.
    struct Credential has key {
        /// The human readable name of this entity. Immutable.
        human_name: vector<u8>,
        /// The base_url holds the URL to be used for off-chain communication. This contains the
        /// entire URL. Mutable.
        base_url: vector<u8>,
        /// 32 byte Ed25519 public key whose counterpart must be used to sign
        /// (1) the payment metadata for on-chain transactions that require dual attestation (e.g.,
        ///     transactions subject to the travel rule)
        /// (2) information exchanged in the off-chain protocols (e.g., KYC info in the travel rule
        ///     protocol)
        /// Note that this is different than `authentication_key` used in DiemAccount, which is
        /// a hash of a public key + signature scheme identifier, not a public key. Mutable.
        compliance_public_key: vector<u8>,
        /// Expiration date in microseconds from unix epoch. For V1, it is always set to
        /// U64_MAX. Mutable, but only by DiemRoot.
        expiration_date: u64,
        /// Event handle for `compliance_public_key` rotation events. Emitted
        /// every time this `compliance_public_key` is rotated.
        compliance_key_rotation_events: EventHandle<ComplianceKeyRotationEvent>,
        /// Event handle for `base_url` rotation events. Emitted every time this `base_url` is rotated.
        base_url_rotation_events: EventHandle<BaseUrlRotationEvent>,
    }

    /// Struct to store the limit on-chain
    struct Limit has key {
        micro_xdx_limit: u64,
    }

    /// The message sent whenever the compliance public key for a `DualAttestation` resource is rotated.
    struct ComplianceKeyRotationEvent has drop, store {
        /// The new `compliance_public_key` that is being used for dual attestation checking.
        new_compliance_public_key: vector<u8>,
        /// The time at which the `compliance_public_key` was rotated
        time_rotated_seconds: u64,
    }

    /// The message sent whenever the base url for a `DualAttestation` resource is rotated.
    struct BaseUrlRotationEvent has drop, store {
        /// The new `base_url` that is being used for dual attestation checking
        new_base_url: vector<u8>,
        /// The time at which the `base_url` was rotated
        time_rotated_seconds: u64,
    }

    const MAX_U64: u128 = 18446744073709551615;

    // Error codes
    /// A credential is not or already published.
    const ECREDENTIAL: u64 = 0;
    /// A limit is not or already published.
    const ELIMIT: u64 = 1;
    /// Cannot parse this as an ed25519 public key
    const EINVALID_PUBLIC_KEY: u64 = 2;
    /// Cannot parse this as an ed25519 signature (e.g., != 64 bytes)
    const EMALFORMED_METADATA_SIGNATURE: u64 = 3;
    /// Signature does not match message and public key
    const EINVALID_METADATA_SIGNATURE: u64 = 4;
    /// The recipient of a dual attestation payment needs to set a compliance public key
    const EPAYEE_COMPLIANCE_KEY_NOT_SET: u64 = 5;
    /// The recipient of a dual attestation payment needs to set a base URL
    const EPAYEE_BASE_URL_NOT_SET: u64 = 6;

    /// Value of the dual attestation limit at genesis
    const INITIAL_DUAL_ATTESTATION_LIMIT: u64 = 1000;
    /// Suffix of every signed dual attestation message
    const DOMAIN_SEPARATOR: vector<u8> = b"@@$$DIEM_ATTEST$$@@";
    /// A year in microseconds
    const ONE_YEAR: u64 = 31540000000000;
    const U64_MAX: u64 = 18446744073709551615;

    /// Publish a `Credential` resource with name `human_name` under `created` with an empty
    /// `base_url` and `compliance_public_key`. Before receiving any dual attestation payments,
    /// the `created` account must send a transaction that invokes `rotate_base_url` and
    /// `rotate_compliance_public_key` to set these fields to a valid URL/public key.
    public(friend) fun publish_credential(
        created: &signer,
        creator: &signer,
        human_name: vector<u8>,
    ) {
        Roles::assert_parent_vasp_or_designated_dealer(created);
        Roles::assert_treasury_compliance(creator);
        assert!(
            !exists<Credential>(signer::address_of(created)),
            errors::already_published(ECREDENTIAL)
        );
        move_to(created, Credential {
            human_name,
            base_url: vector::empty(),
            compliance_public_key: vector::empty(),
            // For testnet and V1, so it should never expire. So set to u64::MAX
            expiration_date: U64_MAX,
            compliance_key_rotation_events: event::new_event_handle<ComplianceKeyRotationEvent>(created),
            base_url_rotation_events: event::new_event_handle<BaseUrlRotationEvent>(created),
        })
    }
    spec publish_credential {
        /// The permission "RotateDualAttestationInfo" is granted to ParentVASP and DesignatedDealer [[H17]][PERMISSION].
        include Roles::AbortsIfNotParentVaspOrDesignatedDealer{account: created};
        include Roles::AbortsIfNotTreasuryCompliance{account: creator};
        aborts_if spec_has_credential(signer::address_of(created)) with errors::ALREADY_PUBLISHED;
    }
    spec fun spec_has_credential(addr: address): bool {
        exists<Credential>(addr)
    }

    /// Rotate the base URL for `account` to `new_url`
    public fun rotate_base_url(account: &signer, new_url: vector<u8>) acquires Credential {
        let addr = signer::address_of(account);
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        let credential = borrow_global_mut<Credential>(addr);
        credential.base_url = copy new_url;
        event::emit_event(&mut credential.base_url_rotation_events, BaseUrlRotationEvent {
            new_base_url: new_url,
            time_rotated_seconds: DiemTimestamp::now_seconds(),
        });
    }
    spec rotate_base_url {
        include RotateBaseUrlAbortsIf;
        include RotateBaseUrlEnsures;
        include RotateBaseUrlEmits;
    }
    spec schema RotateBaseUrlAbortsIf {
        account: signer;
        let sender = signer::address_of(account);

        /// Must abort if the account does not have the resource Credential [[H17]][PERMISSION].
        include AbortsIfNoCredential{addr: sender};

        include DiemTimestamp::AbortsIfNotOperating;
    }
    spec schema AbortsIfNoCredential {
        addr: address;
        aborts_if !spec_has_credential(addr) with errors::NOT_PUBLISHED;
    }
    spec schema RotateBaseUrlEnsures {
        account: signer;
        new_url: vector<u8>;
        let sender = signer::address_of(account);

        ensures global<Credential>(sender).base_url == new_url;
        /// The sender can only rotate its own base url [[H17]][PERMISSION].
        ensures forall addr1:address where addr1 != sender:
            global<Credential>(addr1).base_url == old(global<Credential>(addr1).base_url);
    }
    spec schema RotateBaseUrlEmits {
        account: signer;
        new_url: vector<u8>;
        let sender = signer::address_of(account);
        let handle = global<Credential>(sender).base_url_rotation_events;
        let msg = BaseUrlRotationEvent {
            new_base_url: new_url,
            time_rotated_seconds: DiemTimestamp::spec_now_seconds(),
        };
        emits msg to handle;
    }

    /// Rotate the compliance public key for `account` to `new_key`.
    public fun rotate_compliance_public_key(
        account: &signer,
        new_key: vector<u8>,
    ) acquires Credential {
        let addr = signer::address_of(account);
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        assert!(Signature::ed25519_validate_pubkey(copy new_key), errors::invalid_argument(EINVALID_PUBLIC_KEY));
        let credential = borrow_global_mut<Credential>(addr);
        credential.compliance_public_key = copy new_key;
        event::emit_event(&mut credential.compliance_key_rotation_events, ComplianceKeyRotationEvent {
            new_compliance_public_key: new_key,
            time_rotated_seconds: DiemTimestamp::now_seconds(),
        });

    }
    spec rotate_compliance_public_key {
        include RotateCompliancePublicKeyAbortsIf;
        include RotateCompliancePublicKeyEnsures;
        include RotateCompliancePublicKeyEmits;
    }
    spec schema RotateCompliancePublicKeyAbortsIf {
        account: signer;
        new_key: vector<u8>;

        let sender = signer::address_of(account);
        /// Must abort if the account does not have the resource Credential [[H17]][PERMISSION].
        include AbortsIfNoCredential{addr: sender};

        include DiemTimestamp::AbortsIfNotOperating;
        aborts_if !Signature::ed25519_validate_pubkey(new_key) with errors::INVALID_ARGUMENT;
    }
    spec schema RotateCompliancePublicKeyEnsures {
        account: signer;
        new_key: vector<u8>;

        let sender = signer::address_of(account);
        ensures global<Credential>(sender).compliance_public_key == new_key;
        /// The sender only rotates its own compliance_public_key [[H17]][PERMISSION].
        ensures forall addr1: address where addr1 != sender:
            global<Credential>(addr1).compliance_public_key == old(global<Credential>(addr1).compliance_public_key);
    }
    spec schema RotateCompliancePublicKeyEmits {
        account: signer;
        new_key: vector<u8>;
        let sender = signer::address_of(account);
        let handle = global<Credential>(sender).compliance_key_rotation_events;
        let msg = ComplianceKeyRotationEvent {
            new_compliance_public_key: new_key,
            time_rotated_seconds: DiemTimestamp::spec_now_seconds(),
        };
        emits msg to handle;
    }

    /// Return the human-readable name for the VASP account.
    /// Aborts if `addr` does not have a `Credential` resource.
    public fun human_name(addr: address): vector<u8> acquires Credential {
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        *&borrow_global<Credential>(addr).human_name
    }
    spec human_name {
        pragma opaque;
        include AbortsIfNoCredential;
        ensures result == global<Credential>(addr).human_name;
    }

    /// Return the base URL for `addr`.
    /// Aborts if `addr` does not have a `Credential` resource.
    public fun base_url(addr: address): vector<u8> acquires Credential {
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        *&borrow_global<Credential>(addr).base_url
    }
    spec base_url {
        pragma opaque;
        include AbortsIfNoCredential;
        ensures result == global<Credential>(addr).base_url;
    }
    /// Spec version of `Self::base_url`.
    spec fun spec_base_url(addr: address): vector<u8> {
        global<Credential>(addr).base_url
    }

    /// Return the compliance public key for `addr`.
    /// Aborts if `addr` does not have a `Credential` resource.
    public fun compliance_public_key(addr: address): vector<u8> acquires Credential {
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        *&borrow_global<Credential>(addr).compliance_public_key
    }
    spec compliance_public_key {
        pragma opaque;
        include AbortsIfNoCredential;
        ensures result == spec_compliance_public_key(addr);
    }
    /// Spec version of `Self::compliance_public_key`.
    spec fun spec_compliance_public_key(addr: address): vector<u8> {
        global<Credential>(addr).compliance_public_key
    }

    /// Return the expiration date `addr`
    /// Aborts if `addr` does not have a `Credential` resource.
    public fun expiration_date(addr: address): u64  acquires Credential {
        assert!(exists<Credential>(addr), errors::not_published(ECREDENTIAL));
        *&borrow_global<Credential>(addr).expiration_date
    }
    spec expiration_date {
        pragma opaque;
        include AbortsIfNoCredential;
        ensures result == global<Credential>(addr).expiration_date;
    }

    ///////////////////////////////////////////////////////////////////////////
    // Dual attestation requirements and checking
    ///////////////////////////////////////////////////////////////////////////

    /// Return the address where the credentials for `addr` are stored
    fun credential_address(addr: address): address {
        if (VASP::is_child(addr)) VASP::parent_address(addr) else addr
    }
    spec credential_address {
        pragma opaque;
        aborts_if false;
        ensures result == spec_credential_address(addr);
    }
    spec fun spec_credential_address(addr: address): address {
        if (VASP::is_child(addr)) {
            VASP::spec_parent_address(addr)
        } else {
            addr
        }
    }

    /// Helper which returns true if dual attestion is required for a deposit.
    fun dual_attestation_required<Token>(
        payer: address, payee: address, deposit_value: u64
    ): bool acquires Limit {
        // travel rule applies for payments over a limit
        let travel_rule_limit_microdiem = get_cur_microdiem_limit();
        let approx_xdx_microdiem_value = Diem::approx_xdx_for_value<Token>(deposit_value);
        let above_limit = approx_xdx_microdiem_value >= travel_rule_limit_microdiem;
        if (!above_limit) {
            return false
        };
        // self-deposits never require dual attestation
        if (payer == payee) {
            return false
        };
        // dual attestation is required if the amount is above the limit AND between distinct
        // VASPs
        VASP::is_vasp(payer) && VASP::is_vasp(payee) &&
            VASP::parent_address(payer) != VASP::parent_address(payee)
    }
    spec dual_attestation_required {
        pragma opaque;
        include DualAttestationRequiredAbortsIf<Token>;
        ensures result == spec_dual_attestation_required<Token>(payer, payee, deposit_value);
    }
    spec schema DualAttestationRequiredAbortsIf<Token> {
        deposit_value: num;
        include Diem::ApproxXdmForValueAbortsIf<Token>{from_value: deposit_value};
        aborts_if !spec_is_published() with errors::NOT_PUBLISHED;
    }
    spec fun spec_is_inter_vasp(payer: address, payee: address): bool {
        VASP::is_vasp(payer) && VASP::is_vasp(payee)
            && VASP::spec_parent_address(payer) != VASP::spec_parent_address(payee)
    }
    /// Helper functions which simulates `Self::dual_attestation_required`.
    spec fun spec_dual_attestation_required<Token>(
        payer: address, payee: address, deposit_value: u64
    ): bool {
        Diem::spec_approx_xdx_for_value<Token>(deposit_value)
                    >= spec_get_cur_microdiem_limit() &&
        payer != payee &&
        spec_is_inter_vasp(payer, payee)
    }

    /// Helper to construct a message for dual attestation.
    /// Message is `metadata` | `payer` | `amount` | `DOMAIN_SEPARATOR`.
    fun dual_attestation_message(
        payer: address, metadata: vector<u8>, deposit_value: u64
    ): vector<u8> {
        let message = metadata;
        vector::append(&mut message, bcs::to_bytes(&payer));
        vector::append(&mut message, bcs::to_bytes(&deposit_value));
        vector::append(&mut message, DOMAIN_SEPARATOR);
        message
    }
    spec dual_attestation_message {
        /// Abstract from construction of message for the prover. Concatenation of results from `BCS::to_bytes`
        /// are difficult to reason about, so we avoid doing it. This is possible because the actual value of this
        /// message is not important for the verification problem, as long as the prover considers both
        /// messages which fail verification and which do not.
        pragma opaque;
        aborts_if false;
        ensures [abstract] result == spec_dual_attestation_message(payer, metadata, deposit_value);
    }
    /// Uninterpreted function for `Self::dual_attestation_message`. The actual value does not matter for
    /// the verification of callers.
    spec fun spec_dual_attestation_message(payer: address, metadata: vector<u8>, deposit_value: u64): vector<u8>;

    /// Helper function to check validity of a signature when dual attestion is required.
    fun assert_signature_is_valid(
        payer: address,
        payee: address,
        metadata_signature: vector<u8>,
        metadata: vector<u8>,
        deposit_value: u64
    ) acquires Credential {
        // sanity check of signature validity
        assert!(
            vector::length(&metadata_signature) == 64,
            errors::invalid_argument(EMALFORMED_METADATA_SIGNATURE)
        );
        // sanity check of payee compliance key validity
        let payee_compliance_key = compliance_public_key(credential_address(payee));
        assert!(
            !vector::is_empty(&payee_compliance_key),
            errors::invalid_state(EPAYEE_COMPLIANCE_KEY_NOT_SET)
        );
        // sanity check of payee base URL validity
        let payee_base_url = base_url(credential_address(payee));
        assert!(
            !vector::is_empty(&payee_base_url),
            errors::invalid_state(EPAYEE_BASE_URL_NOT_SET)
        );
        // cryptographic check of signature validity
        let message = dual_attestation_message(payer, metadata, deposit_value);
        assert!(
            Signature::ed25519_verify(metadata_signature, payee_compliance_key, message),
            errors::invalid_argument(EINVALID_METADATA_SIGNATURE),
        );
    }
    spec assert_signature_is_valid {
        pragma opaque = true;
        include AssertSignatureValidAbortsIf;
    }
    spec schema AssertSignatureValidAbortsIf {
        payer: address;
        payee: address;
        metadata_signature: vector<u8>;
        metadata: vector<u8>;
        deposit_value: u64;
        include AbortsIfNoCredential{addr: spec_credential_address(payee)};
        aborts_if vector::is_empty(spec_compliance_public_key(spec_credential_address(payee))) with errors::INVALID_STATE;
        aborts_if vector::is_empty(spec_base_url(spec_credential_address(payee))) with errors::INVALID_STATE;
        aborts_if !spec_signature_is_valid(payer, payee, metadata_signature, metadata, deposit_value)
            with errors::INVALID_ARGUMENT;
    }

    /// Returns true if signature is valid.
    spec fun spec_signature_is_valid(
        payer: address,
        payee: address,
        metadata_signature: vector<u8>,
        metadata: vector<u8>,
        deposit_value: u64
    ): bool {
        let payee_compliance_key = spec_compliance_public_key(spec_credential_address(payee));
        len(metadata_signature) == 64 &&
            !vector::is_empty(payee_compliance_key) &&
            Signature::ed25519_verify(
                metadata_signature,
                payee_compliance_key,
                spec_dual_attestation_message(payer, metadata, deposit_value)
            )
    }

    /// Public API for checking whether a payment of `value` coins of type `Currency`
    /// from `payer` to `payee` has a valid dual attestation. This returns without aborting if
    /// (1) dual attestation is not required for this payment, or
    /// (2) dual attestation is required, and `metadata_signature` can be verified on the message
    ///     `metadata` | `payer` | `value` | `DOMAIN_SEPARATOR` using the `compliance_public_key`
    ///     published in `payee`'s `Credential` resource
    /// It aborts with an appropriate error code if dual attestation is required, but one or more of
    /// the conditions in (2) is not met.
    public fun assert_payment_ok<Currency>(
        payer: address,
        payee: address,
        value: u64,
        metadata: vector<u8>,
        metadata_signature: vector<u8>
    ) acquires Credential, Limit {
        if (!vector::is_empty(&metadata_signature) || // allow opt-in dual attestation
            dual_attestation_required<Currency>(payer, payee, value)
        ) {
          assert_signature_is_valid(payer, payee, metadata_signature, metadata, value)
        }
    }
    spec assert_payment_ok {
        pragma opaque;
        include AssertPaymentOkAbortsIf<Currency>;
    }
    spec schema AssertPaymentOkAbortsIf<Currency> {
        payer: address;
        payee: address;
        value: u64;
        metadata: vector<u8>;
        metadata_signature: vector<u8>;
        include len(metadata_signature) == 0 ==> DualAttestationRequiredAbortsIf<Currency>{deposit_value: value};
        include (len(metadata_signature) != 0 || spec_dual_attestation_required<Currency>(payer, payee, value))
            ==> AssertSignatureValidAbortsIf{deposit_value: value};
    }

    ///////////////////////////////////////////////////////////////////////////
    // Creating and updating dual attestation limit
    ///////////////////////////////////////////////////////////////////////////

    /// Travel rule limit set during genesis
    public fun initialize(dr_account: &signer) {
        DiemTimestamp::assert_genesis();
        CoreAddresses::assert_diem_root(dr_account); // operational constraint.
        assert!(!exists<Limit>(@DiemRoot), errors::already_published(ELIMIT));
        let initial_limit = (INITIAL_DUAL_ATTESTATION_LIMIT as u128) * (Diem::scaling_factor<XDX>() as u128);
        assert!(initial_limit <= MAX_U64, errors::limit_exceeded(ELIMIT));
        move_to(
            dr_account,
            Limit {
                micro_xdx_limit: (initial_limit as u64)
            }
        )
    }
    spec initialize {
        include DiemTimestamp::AbortsIfNotGenesis;
        include CoreAddresses::AbortsIfNotDiemRoot{account: dr_account};
        aborts_if exists<Limit>(@DiemRoot) with errors::ALREADY_PUBLISHED;
        let initial_limit = INITIAL_DUAL_ATTESTATION_LIMIT * Diem::spec_scaling_factor<XDX>();
        aborts_if initial_limit > MAX_U64 with errors::LIMIT_EXCEEDED;
        include Diem::AbortsIfNoCurrency<XDX>; // for scaling_factor.
        ensures global<Limit>(@DiemRoot).micro_xdx_limit == initial_limit;
    }

    /// Return the current dual attestation limit in microdiem
    public fun get_cur_microdiem_limit(): u64 acquires Limit {
        assert!(exists<Limit>(@DiemRoot), errors::not_published(ELIMIT));
        borrow_global<Limit>(@DiemRoot).micro_xdx_limit
    }
    spec get_cur_microdiem_limit {
        pragma opaque;
        aborts_if !spec_is_published() with errors::NOT_PUBLISHED;
        ensures result == spec_get_cur_microdiem_limit();
    }

    /// Set the dual attestation limit to `micro_diem_limit`.
    /// Aborts if `tc_account` does not have the TreasuryCompliance role
    public fun set_microdiem_limit(tc_account: &signer, micro_xdx_limit: u64) acquires Limit {
        Roles::assert_treasury_compliance(tc_account);
        assert!(exists<Limit>(@DiemRoot), errors::not_published(ELIMIT));
        borrow_global_mut<Limit>(@DiemRoot).micro_xdx_limit = micro_xdx_limit;
    }
    spec set_microdiem_limit {
        /// Must abort if the signer does not have the TreasuryCompliance role [[H6]][PERMISSION].
        /// The permission UpdateDualAttestationLimit is granted to TreasuryCompliance.
        include Roles::AbortsIfNotTreasuryCompliance{account: tc_account};

        aborts_if !spec_is_published() with errors::NOT_PUBLISHED;
        ensures global<Limit>(@DiemRoot).micro_xdx_limit == micro_xdx_limit;
    }

    // **************************** SPECIFICATION ********************************
    spec module {} // switch documentation context back to module level

    /// # Initialization

    /// The Limit resource should be published after genesis
    spec module {
        invariant [suspendable] DiemTimestamp::is_operating() ==> spec_is_published();
    }

    /// # Helper Functions
    spec module {
        /// Helper function to determine whether the Limit is published.
        fun spec_is_published(): bool {
            exists<Limit>(@DiemRoot)
        }

        /// Mirrors `Self::get_cur_microdiem_limit`.
        fun spec_get_cur_microdiem_limit(): u64 {
            global<Limit>(@DiemRoot).micro_xdx_limit
        }
    }

    /// # Access Control
    spec module {
        /// Only TreasuryCompliance can change the limit [[H6]][PERMISSION].
        invariant update forall a: address where old(exists<Limit>(@DiemRoot)):
            spec_get_cur_microdiem_limit() != old(spec_get_cur_microdiem_limit())
		     ==> Roles::spec_signed_by_treasury_compliance_role();

        /// The permission "RotateDualAttestationInfo(addr)" is only granted to ParentVASP or DD [[H17]][PERMISSION].
        /// "Credential" resources are only published under ParentVASP or DD accounts.
        invariant forall addr1: address:
            spec_has_credential(addr1) ==>
                (Roles::spec_has_parent_VASP_role_addr(addr1) ||
                Roles::spec_has_designated_dealer_role_addr(addr1));

        /// Only the one who owns Credential can rotate the dual attenstation info [[H17]][PERMISSION].
        invariant update forall a: address where old(spec_has_credential(a)):
            global<Credential>(a).compliance_public_key != old(global<Credential>(a).compliance_public_key)
		     ==> signer::is_txn_signer_addr(a);

        invariant update forall a: address where old(spec_has_credential(a)):
            global<Credential>(a).base_url != old(global<Credential>(a).base_url)
		     ==> signer::is_txn_signer_addr(a);

        /// The permission "RotateDualAttestationInfo(addr)" is not transferred [[J17]][PERMISSION].
        /// resource struct `Credential` is persistent.
        invariant<CoinType> update forall a: address:
            old(spec_has_credential(a)) ==> spec_has_credential(a);
    }
}
