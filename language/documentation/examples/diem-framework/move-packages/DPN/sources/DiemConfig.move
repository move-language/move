/// Publishes configuration information for validators, and issues reconfiguration events
/// to synchronize configuration changes for the validators.
module DiemFramework::DiemConfig {
    use DiemFramework::CoreAddresses;
    use DiemFramework::DiemTimestamp;
    use DiemFramework::Roles;
    use std::errors;
    use std::event;
    use std::signer;
    friend DiemFramework::DiemVersion;
    friend DiemFramework::RegisteredCurrencies;
    friend DiemFramework::DiemTransactionPublishingOption;
    friend DiemFramework::DiemVMConfig;
    friend DiemFramework::DiemSystem;
    friend DiemFramework::DiemConsensusConfig;
    friend DiemFramework::ParallelExecutionConfig;

    /// A generic singleton resource that holds a value of a specific type.
    struct DiemConfig<Config: copy + drop + store> has key, store {
        /// Holds specific info for instance of `Config` type.
        payload: Config
    }

    /// Event that signals DiemBFT algorithm to start a new epoch,
    /// with new configuration information. This is also called a
    /// "reconfiguration event"
    struct NewEpochEvent has drop, store {
        epoch: u64,
    }

    /// Holds information about state of reconfiguration
    struct Configuration has key {
        /// Epoch number
        epoch: u64,
        /// Time of last reconfiguration. Only changes on reconfiguration events.
        last_reconfiguration_time: u64,
        /// Event handle for reconfiguration events
        events: event::EventHandle<NewEpochEvent>,
    }

    /// Accounts with this privilege can modify DiemConfig<TypeName> under Diem root address.
    struct ModifyConfigCapability<phantom TypeName> has key, store {}

    /// Reconfiguration disabled if this resource occurs under LibraRoot.
    struct DisableReconfiguration has key {}

    /// The `Configuration` resource is in an invalid state
    const ECONFIGURATION: u64 = 0;
    /// A `DiemConfig` resource is in an invalid state
    const EDIEM_CONFIG: u64 = 1;
    /// A `ModifyConfigCapability` is in a different state than was expected
    const EMODIFY_CAPABILITY: u64 = 2;
    /// An invalid block time was encountered.
    const EINVALID_BLOCK_TIME: u64 = 3;
    /// The largest possible u64 value
    const MAX_U64: u64 = 18446744073709551615;

    /// Publishes `Configuration` resource. Can only be invoked by Diem root, and only a single time in Genesis.
    public fun initialize(
        dr_account: &signer,
    ) {
        DiemTimestamp::assert_genesis();
        CoreAddresses::assert_diem_root(dr_account);
        assert!(!exists<Configuration>(@DiemRoot), errors::already_published(ECONFIGURATION));
        move_to<Configuration>(
            dr_account,
            Configuration {
                epoch: 0,
                last_reconfiguration_time: 0,
                events: event::new_event_handle<NewEpochEvent>(dr_account),
            }
        );
    }
    spec initialize {
        pragma opaque;
        include InitializeAbortsIf;
        include InitializeEnsures;
        modifies global<Configuration>(@DiemRoot);
        modifies global<event::EventHandleGenerator>(signer::address_of(dr_account));
    }
    spec schema InitializeAbortsIf {
        dr_account: signer;
        include DiemTimestamp::AbortsIfNotGenesis;
        include CoreAddresses::AbortsIfNotDiemRoot{account: dr_account};
        aborts_if spec_has_config() with errors::ALREADY_PUBLISHED;
    }
    spec schema InitializeEnsures {
        ensures spec_has_config();
        let post post_config = global<Configuration>(@DiemRoot);
        ensures post_config.epoch == 0;
        ensures post_config.last_reconfiguration_time == 0;
    }


    /// Returns a copy of `Config` value stored under `addr`.
    public fun get<Config: copy + drop + store>(): Config
    acquires DiemConfig {
        let addr = @DiemRoot;
        assert!(exists<DiemConfig<Config>>(addr), errors::not_published(EDIEM_CONFIG));
        *&borrow_global<DiemConfig<Config>>(addr).payload
    }
    spec get {
        pragma opaque;
        include AbortsIfNotPublished<Config>;
        ensures result == get<Config>();
    }
    spec schema AbortsIfNotPublished<Config> {
        aborts_if !exists<DiemConfig<Config>>(@DiemRoot) with errors::NOT_PUBLISHED;
    }

    /// Set a config item to a new value with the default capability stored under config address and trigger a
    /// reconfiguration. This function requires that the signer have a `ModifyConfigCapability<Config>`
    /// resource published under it.
    public(friend) fun set<Config: copy + drop + store>(account: &signer, payload: Config)
    acquires DiemConfig, Configuration {
        let signer_address = signer::address_of(account);
        // Next should always be true if properly initialized.
        assert!(exists<ModifyConfigCapability<Config>>(signer_address), errors::requires_capability(EMODIFY_CAPABILITY));

        let addr = @DiemRoot;
        assert!(exists<DiemConfig<Config>>(addr), errors::not_published(EDIEM_CONFIG));
        let config = borrow_global_mut<DiemConfig<Config>>(addr);
        config.payload = payload;

        reconfigure_();
    }
    spec set {
        pragma opaque;
        pragma delegate_invariants_to_caller;
        requires DiemTimestamp::is_operating() ==> spec_has_config();
        modifies global<Configuration>(@DiemRoot);
        modifies global<DiemConfig<Config>>(@DiemRoot);
        include SetAbortsIf<Config>;
        include SetEnsures<Config>;
    }
    spec schema SetAbortsIf<Config> {
        account: signer;
        include AbortsIfNotModifiable<Config>;
        include AbortsIfNotPublished<Config>;
        include ReconfigureAbortsIf;
    }
    spec schema AbortsIfNotModifiable<Config> {
        account: signer;
        aborts_if !exists<ModifyConfigCapability<Config>>(signer::address_of(account))
            with errors::REQUIRES_CAPABILITY;
    }
    spec schema SetEnsures<Config> {
        payload: Config;
        ensures spec_is_published<Config>();
        ensures get<Config>() == payload;
        ensures old(spec_has_config()) == spec_has_config();
    }

    /// Set a config item to a new value and trigger a reconfiguration. This function
    /// requires a reference to a `ModifyConfigCapability`, which is returned when the
    /// config is published using `publish_new_config_and_get_capability`.
    /// It is called by `DiemSystem::update_config_and_reconfigure`, which allows
    /// validator operators to change the validator set.  All other config changes require
    /// a Diem root signer.
    public(friend) fun set_with_capability_and_reconfigure<Config: copy + drop + store>(
        _cap: &ModifyConfigCapability<Config>,
        payload: Config
    ) acquires DiemConfig, Configuration {
        let addr = @DiemRoot;
        assert!(exists<DiemConfig<Config>>(addr), errors::not_published(EDIEM_CONFIG));
        let config = borrow_global_mut<DiemConfig<Config>>(addr);
        config.payload = payload;
        reconfigure_();
    }
    spec set_with_capability_and_reconfigure {
        pragma opaque;
        pragma delegate_invariants_to_caller;
        requires DiemTimestamp::is_operating() ==> spec_has_config();
        modifies global<Configuration>(@DiemRoot);
        include AbortsIfNotPublished<Config>;
        include ReconfigureAbortsIf;
        modifies global<DiemConfig<Config>>(@DiemRoot);
        include SetEnsures<Config>;
        include ReconfigureEmits;
    }

    /// Private function to temporarily halt reconfiguration.
    /// This function should only be used for offline WriteSet generation purpose and should never be invoked on chain.
    fun disable_reconfiguration(dr_account: &signer) {
        assert!(
            signer::address_of(dr_account) == @DiemRoot,
            errors::requires_address(EDIEM_CONFIG)
        );
        Roles::assert_diem_root(dr_account);
        assert!(reconfiguration_enabled(), errors::invalid_state(ECONFIGURATION));
        move_to(dr_account, DisableReconfiguration {} )
    }

    /// Private function to resume reconfiguration.
    /// This function should only be used for offline WriteSet generation purpose and should never be invoked on chain.
    fun enable_reconfiguration(dr_account: &signer) acquires DisableReconfiguration {
        assert!(
            signer::address_of(dr_account) == @DiemRoot,
            errors::requires_address(EDIEM_CONFIG)
        );
        Roles::assert_diem_root(dr_account);

        assert!(!reconfiguration_enabled(), errors::invalid_state(ECONFIGURATION));
        DisableReconfiguration {} = move_from<DisableReconfiguration>(signer::address_of(dr_account));
    }

    fun reconfiguration_enabled(): bool {
        !exists<DisableReconfiguration>(@DiemRoot)
    }

    /// Publishes a new config.
    /// The caller will use the returned ModifyConfigCapability to specify the access control
    /// policy for who can modify the config.
    /// Does not trigger a reconfiguration.
    public(friend) fun publish_new_config_and_get_capability<Config: copy + drop + store>(
        dr_account: &signer,
        payload: Config,
    ): ModifyConfigCapability<Config> {
        Roles::assert_diem_root(dr_account);
        assert!(
            !exists<DiemConfig<Config>>(signer::address_of(dr_account)),
            errors::already_published(EDIEM_CONFIG)
        );
        move_to(dr_account, DiemConfig { payload });
        ModifyConfigCapability<Config> {}
    }
    spec publish_new_config_and_get_capability {
        pragma opaque;
        pragma delegate_invariants_to_caller;
        modifies global<DiemConfig<Config>>(@DiemRoot);
        include Roles::AbortsIfNotDiemRoot{account: dr_account};
        include AbortsIfPublished<Config>;
        include SetEnsures<Config>;
    }
    spec schema AbortsIfPublished<Config> {
        aborts_if exists<DiemConfig<Config>>(@DiemRoot) with errors::ALREADY_PUBLISHED;
    }

    /// Publish a new config item. Only Diem root can modify such config.
    /// Publishes the capability to modify this config under the Diem root account.
    /// Does not trigger a reconfiguration.
    public(friend) fun publish_new_config<Config: copy + drop + store>(
        dr_account: &signer,
        payload: Config
    ) {
        let capability = publish_new_config_and_get_capability<Config>(dr_account, payload);
        assert!(
            !exists<ModifyConfigCapability<Config>>(signer::address_of(dr_account)),
            errors::already_published(EMODIFY_CAPABILITY)
        );
        move_to(dr_account, capability);
    }
    spec publish_new_config {
        pragma opaque;
        pragma delegate_invariants_to_caller;
        modifies global<DiemConfig<Config>>(@DiemRoot);
        modifies global<ModifyConfigCapability<Config>>(@DiemRoot);
        include PublishNewConfigAbortsIf<Config>;
        include PublishNewConfigEnsures<Config>;
    }
    spec schema PublishNewConfigAbortsIf<Config> {
        dr_account: signer;
        include Roles::AbortsIfNotDiemRoot{account: dr_account};
        aborts_if spec_is_published<Config>();
        aborts_if exists<ModifyConfigCapability<Config>>(signer::address_of(dr_account));
    }
    spec schema PublishNewConfigEnsures<Config> {
        dr_account: signer;
        payload: Config;
        include SetEnsures<Config>;
        ensures exists<ModifyConfigCapability<Config>>(signer::address_of(dr_account));
    }

    /// Signal validators to start using new configuration. Must be called by Diem root.
    public fun reconfigure(
        dr_account: &signer,
    ) acquires Configuration {
        Roles::assert_diem_root(dr_account);
        reconfigure_();
    }
    spec reconfigure {
        pragma opaque;
        modifies global<Configuration>(@DiemRoot);
        ensures old(spec_has_config()) == spec_has_config();
        include Roles::AbortsIfNotDiemRoot{account: dr_account};
        include ReconfigureAbortsIf;
        include ReconfigureEmits;
    }

    /// Private function to do reconfiguration.  Updates reconfiguration status resource
    /// `Configuration` and emits a `NewEpochEvent`
    fun reconfigure_() acquires Configuration {
        // Do not do anything if genesis has not finished.
        if (DiemTimestamp::is_genesis() || DiemTimestamp::now_microseconds() == 0 || !reconfiguration_enabled()) {
            return ()
        };

        let config_ref = borrow_global_mut<Configuration>(@DiemRoot);
        let current_time = DiemTimestamp::now_microseconds();

        // Do not do anything if a reconfiguration event is already emitted within this transaction.
        //
        // This is OK because:
        // - The time changes in every non-empty block
        // - A block automatically ends after a transaction that emits a reconfiguration event, which is guaranteed by
        //   DiemVM spec that all transactions comming after a reconfiguration transaction will be returned as Retry
        //   status.
        // - Each transaction must emit at most one reconfiguration event
        //
        // Thus, this check ensures that a transaction that does multiple "reconfiguration required" actions emits only
        // one reconfiguration event.
        //
        if (current_time == config_ref.last_reconfiguration_time) {
            return
        };

        assert!(current_time > config_ref.last_reconfiguration_time, errors::invalid_state(EINVALID_BLOCK_TIME));
        config_ref.last_reconfiguration_time = current_time;
        config_ref.epoch = config_ref.epoch + 1;

        event::emit_event<NewEpochEvent>(
            &mut config_ref.events,
            NewEpochEvent {
                epoch: config_ref.epoch,
            },
        );
    }
    spec fun spec_reconfigure_omitted(): bool {
       DiemTimestamp::is_genesis() || DiemTimestamp::spec_now_microseconds() == 0 || !reconfiguration_enabled()
    }
    spec reconfigure_ {
        pragma opaque;
        modifies global<Configuration>(@DiemRoot);
        requires DiemTimestamp::is_operating() ==> spec_has_config();
        ensures old(spec_has_config()) == spec_has_config();
        let config = global<Configuration>(@DiemRoot);
        let post post_config = global<Configuration>(@DiemRoot);
        let now = DiemTimestamp::spec_now_microseconds();
        let post post_now = DiemTimestamp::spec_now_microseconds();

        include !spec_reconfigure_omitted() || (config.last_reconfiguration_time == now)
            ==> InternalReconfigureAbortsIf && ReconfigureAbortsIf;

        ensures spec_reconfigure_omitted() || (config.last_reconfiguration_time == now)
            ==> post_config == config;

        ensures !(spec_reconfigure_omitted() || (config.last_reconfiguration_time == now))
            ==> post_config ==
                update_field(
                update_field(config,
                    epoch, config.epoch + 1),
                    last_reconfiguration_time, post_now);
        include ReconfigureEmits;
    }
    /// The following schema describes aborts conditions which we do not want to be propagated to the verification
    /// of callers, and which are therefore marked as `concrete` to be only verified against the implementation.
    /// These conditions are unlikely to happen in reality, and excluding them avoids formal noise.
    spec schema InternalReconfigureAbortsIf {
        let config = global<Configuration>(@DiemRoot);
        let current_time = DiemTimestamp::spec_now_microseconds();
        aborts_if [concrete] current_time < config.last_reconfiguration_time with errors::INVALID_STATE;
        aborts_if [concrete] config.epoch == MAX_U64
            && current_time != config.last_reconfiguration_time with EXECUTION_FAILURE;
    }
    /// This schema is to be used by callers of `reconfigure`
    spec schema ReconfigureAbortsIf {
        let config = global<Configuration>(@DiemRoot);
        let current_time = DiemTimestamp::spec_now_microseconds();
        aborts_if DiemTimestamp::is_operating()
            && reconfiguration_enabled()
            && DiemTimestamp::spec_now_microseconds() > 0
            && config.epoch < MAX_U64
            && current_time < config.last_reconfiguration_time
                with errors::INVALID_STATE;
    }
    spec schema ReconfigureEmits {
        let config = global<Configuration>(@DiemRoot);
        let post post_config = global<Configuration>(@DiemRoot);
        let post now = DiemTimestamp::spec_now_microseconds();
        let post msg = NewEpochEvent {
            epoch: post_config.epoch,
        };
        let handle = config.events;
        emits msg to handle if (!spec_reconfigure_omitted() && now != config.last_reconfiguration_time);
    }

    /// Emit a `NewEpochEvent` event. This function will be invoked by genesis directly to generate the very first
    /// reconfiguration event.
    fun emit_genesis_reconfiguration_event() acquires Configuration {
        assert!(exists<Configuration>(@DiemRoot), errors::not_published(ECONFIGURATION));
        let config_ref = borrow_global_mut<Configuration>(@DiemRoot);
        assert!(config_ref.epoch == 0 && config_ref.last_reconfiguration_time == 0, errors::invalid_state(ECONFIGURATION));
        config_ref.epoch = 1;

        event::emit_event<NewEpochEvent>(
            &mut config_ref.events,
            NewEpochEvent {
                epoch: config_ref.epoch,
            },
        );
    }
    spec emit_genesis_reconfiguration_event {
        let post config = global<Configuration>(@DiemRoot);
        let post handle = config.events;
        let post msg = NewEpochEvent {
                epoch: config.epoch,
        };
        ensures config.epoch == 1;
        emits msg to handle;
    }

    // =================================================================
    // Test-only functions

    #[test_only]
    public fun set_for_testing<Config: copy + drop + store>(account: &signer, payload: Config)
    acquires DiemConfig, Configuration {
        set(account, payload)
    }

    #[test_only]
    public fun publish_new_config_for_testing<Config: copy + drop + store>(
        dr_account: &signer,
        payload: Config
    ) {
        publish_new_config(dr_account, payload);
    }

    // =================================================================
    // Module Specification

    spec module {} // Switch to module documentation context

    /// # Initialization
    spec module {
        /// After genesis, the `Configuration` is published.
        invariant [suspendable] DiemTimestamp::is_operating() ==> spec_has_config();
    }

    /// # Invariants
    spec module {
        /// Configurations are only stored at the diem root address.
        invariant<ConfigType>
            forall config_address: address where exists<DiemConfig<ConfigType>>(config_address):
                config_address == @DiemRoot;

        /// Published configurations are persistent.
        invariant<ConfigType> update
            old(spec_is_published<ConfigType>()) ==> spec_is_published<ConfigType>();

        /// If `ModifyConfigCapability<Config>` is published, it is persistent.
        invariant<ConfigType> update
            old(exists<ModifyConfigCapability<ConfigType>>(@DiemRoot)) ==>
                exists<ModifyConfigCapability<ConfigType>>(@DiemRoot);
    }

    /// # Helper Functions
    spec module {
        fun spec_has_config(): bool {
            exists<Configuration>(@DiemRoot)
        }

        fun spec_is_published<Config>(): bool {
            exists<DiemConfig<Config>>(@DiemRoot)
        }

        fun spec_get_config<Config>(): Config {
            global<DiemConfig<Config>>(@DiemRoot).payload
        }
    }

}
