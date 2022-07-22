// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::loader::Loader;
use move_binary_format::{
    access::ModuleAccess,
    compatibility::Compatibility,
    errors::{verification_error, Location, PartialVMError, PartialVMResult, VMResult},
    normalized, CompiledModule, IndexKind,
};
use move_core_types::{account_address::AccountAddress, vm_status::StatusCode};
use move_vm_types::data_store::DataStore;
use std::collections::BTreeSet;
use tracing::warn;

pub(crate) fn validate_and_compile_modules<'a>(
    modules: &[Vec<u8>],
    sender: AccountAddress,
    data_store: &'a mut dyn DataStore,
    loader: &Loader,
) -> VMResult<Vec<CompiledModule>> {
    // deserialize the modules. Perform bounds check. After this indexes can be
    // used with the `[]` operator
    let compiled_modules = match modules
        .iter()
        .map(|blob| CompiledModule::deserialize(blob))
        .collect::<PartialVMResult<Vec<_>>>()
    {
        Ok(modules) => modules,
        Err(err) => {
            warn!("[VM] module deserialization failed {:?}", err);
            return Err(err.finish(Location::Undefined));
        }
    };

    // Make sure all modules' self addresses matches the transaction sender. The self address is
    // where the module will actually be published. If we did not check this, the sender could
    // publish a module under anyone's account.
    for module in &compiled_modules {
        if module.address() != &sender {
            return Err(verification_error(
                StatusCode::MODULE_ADDRESS_DOES_NOT_MATCH_SENDER,
                IndexKind::AddressIdentifier,
                module.self_handle_idx().0,
            )
            .finish(Location::Undefined));
        }
    }

    // Collect ids for modules that are published together
    let mut bundle_unverified = BTreeSet::new();

    // For now, we assume that all modules can be republished, as long as the new module is
    // backward compatible with the old module.
    //
    // TODO: in the future, we may want to add restrictions on module republishing, possibly by
    // changing the bytecode format to include an `is_upgradable` flag in the CompiledModule.
    for module in &compiled_modules {
        let module_id = module.self_id();
        if data_store.exists_module(&module_id)? {
            let old_module_ref = loader.load_module(&module_id, data_store)?;
            let old_module = old_module_ref.module();
            let old_m = normalized::Module::new(old_module);
            let new_m = normalized::Module::new(module);
            let compat = Compatibility::check(&old_m, &new_m);
            if !compat.is_fully_compatible() {
                return Err(
                    PartialVMError::new(StatusCode::BACKWARD_INCOMPATIBLE_MODULE_UPDATE)
                        .finish(Location::Undefined),
                );
            }
        }
        if !bundle_unverified.insert(module_id) {
            return Err(
                PartialVMError::new(StatusCode::DUPLICATE_MODULE_NAME).finish(Location::Undefined)
            );
        }
    }

    // Perform bytecode and loading verification. Modules must be sorted in topological order.
    loader.verify_module_bundle_for_publication(&compiled_modules, data_store)?;

    // NOTE: we want to (informally) argue that all modules pass the linking check before being
    // published to the data store.
    //
    // The linking check consists of two checks actually
    // - dependencies::verify_module(module, all_imm_deps)
    // - cyclic_dependencies::verify_module(module, fn_imm_deps, fn_imm_friends)
    //
    // [Claim 1]
    // We show that the `dependencies::verify_module` check is always satisfied whenever a
    // module M is published or updated and the `all_imm_deps` contains the actual modules
    // required by M.
    //
    // Suppose M depends on D, and we now consider the following scenarios:
    // 1) D does not appear in the bundle together with M
    // -- In this case, D must be either in the code cache or in the data store which can be
    //    loaded into the code cache (and pass all checks on D).
    //    - If D is missing, the linking will fail and return an error.
    //    - If D exists, D will be added to the `all_imm_deps` arg when checking M.
    //
    // 2) D appears in the bundle *before* M
    // -- In this case, regardless of whether D is in code cache or not, D will be put into the
    //    `bundle_verified` argument and modules in `bundle_verified` will be prioritized before
    //    returning a module in code cache.
    //
    // 3) D appears in the bundle *after* M
    // -- This technically should be discouraged but this is user input so we cannot have this
    //    assumption here. But nevertheless, we can still make the claim 1 even in this case.
    //    When M is verified, flow 1) is effectively activated, which means:
    //    - If the code cache or the data store does not contain a D' which has the same name
    //      with D, then the linking will fail and return an error.
    //    - If D' exists, and M links against D', then when verifying D in a later time point,
    //      a compatibility check will be invoked to ensure that D is compatible with D',
    //      meaning, whichever module that links against D' will have to link against D as well.
    //
    // [Claim 2]
    // We show that the `cyclic_dependencies::verify_module` check is always satisfied whenever
    // a module M is published or updated and the dep/friend modules returned by the transitive
    // dependency closure functions are valid.
    //
    // Currently, the code is written in a way that, from the view point of the
    // `cyclic_dependencies::verify_module` check, modules checked prior to module M in the same
    // bundle looks as if they have already been published and loaded to the code cache.
    //
    // Therefore, if M forms a cyclic dependency with module A in the same bundle that is
    // checked prior to M, such an error will be detected. However, if M forms a cyclic
    // dependency with a module X that appears in the same bundle *after* M. The cyclic
    // dependency can only be caught when X is verified.
    //
    // In summary: the code is written in a way that, certain checks are skipped while checking
    // each individual module in the bundle in order. But if every module in the bundle pass
    // all the checks, then the whole bundle can be published/upgraded together. Otherwise,
    // none of the module can be published/updated.

    // All modules verified, publish them to data cache
    Ok(compiled_modules)
}
