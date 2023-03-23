// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    errors::{PartialVMError, PartialVMResult},
    file_format::{AbilitySet, StructTypeParameter, Visibility},
    file_format_common::VERSION_5,
    normalized::Module,
};
use move_core_types::vm_status::StatusCode;
use std::collections::BTreeSet;

/// The result of a linking and layout compatibility check. Here is what the different combinations. NOTE that if `check_struct_layout` is false, type safety over a series of upgrades cannot be guaranteed.
/// mean:
/// `{ check_struct_and_pub_function_linking: true, check_struct_layout: true, check_friend_linking: true }`: fully backward compatible
/// `{ check_struct_and_pub_function_linking: true, check_struct_layout: true, check_friend_linking: false }`: Backward compatible, exclude the friend module declare and friend functions
/// `{ check_struct_and_pub_function_linking: false, check_struct_layout: true, check_friend_linking: false }`: Dependent modules that reference functions or types in this module may not link. However, fixing, recompiling, and redeploying all dependent modules will work--no data migration needed.
/// `{ check_struct_and_pub_function_linking: true, check_struct_layout: false, check_friend_linking: true }`: Attempting to read structs published by this module will now fail at runtime. However, dependent modules will continue to link. Requires data migration, but no changes to dependent modules.
/// `{ check_struct_and_pub_function_linking: false, check_struct_layout: false, check_friend_linking: false }`: Everything is broken. Need both a data migration and changes to dependent modules.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Compatibility {
    /// if false, do not ensure the dependent modules that reference public functions or structs in this module can link
    check_struct_and_pub_function_linking: bool,
    /// if false, do not ensure the struct layout capability
    check_struct_layout: bool,
    /// if false, treat `friend` as `private` when `check_struct_and_function_linking`.
    check_friend_linking: bool,
}

impl Default for Compatibility {
    fn default() -> Self {
        Self {
            check_struct_and_pub_function_linking: true,
            check_struct_layout: true,
            check_friend_linking: true,
        }
    }
}

impl Compatibility {
    pub fn full_check() -> Self {
        Self::default()
    }

    pub fn no_check() -> Self {
        Self {
            check_struct_and_pub_function_linking: false,
            check_struct_layout: false,
            check_friend_linking: false,
        }
    }

    pub fn new(
        check_struct_and_pub_function_linking: bool,
        check_struct_layout: bool,
        check_friend_linking: bool,
    ) -> Self {
        Self {
            check_struct_and_pub_function_linking,
            check_struct_layout,
            check_friend_linking,
        }
    }

    pub fn need_check_compat(&self) -> bool {
        self.check_struct_and_pub_function_linking
            || self.check_friend_linking
            || self.check_struct_layout
    }

    /// Check compatibility for `new_module` relative to old module `old_module`.
    pub fn check(&self, old_module: &Module, new_module: &Module) -> PartialVMResult<()> {
        let mut struct_and_pub_function_linking = true;
        let mut struct_layout = true;
        let mut friend_linking = true;

        // module's name and address are unchanged
        if old_module.address != new_module.address || old_module.name != new_module.name {
            struct_and_pub_function_linking = false;
        }

        // old module's structs are a subset of the new module's structs
        for (name, old_struct) in &old_module.structs {
            let new_struct = match new_module.structs.get(name) {
                Some(new_struct) => new_struct,
                None => {
                    // Struct not present in new . Existing modules that depend on this struct will fail to link with the new version of the module.
                    // Also, struct layout cannot be guaranteed transitively, because after
                    // removing the struct, it could be re-added later with a different layout.
                    struct_and_pub_function_linking = false;
                    struct_layout = false;
                    break;
                },
            };

            if !struct_abilities_compatibile(old_struct.abilities, new_struct.abilities)
                || !struct_type_parameters_compatibile(
                    &old_struct.type_parameters,
                    &new_struct.type_parameters,
                )
            {
                struct_and_pub_function_linking = false;
            }
            if new_struct.fields != old_struct.fields {
                // Fields changed. Code in this module will fail at runtime if it tries to
                // read a previously published struct value
                // TODO: this is a stricter definition than required. We could in principle
                // choose that changing the name (but not position or type) of a field is
                // compatible. The VM does not care about the name of a field
                // (it's purely informational), but clients presumably do.
                struct_layout = false
            }
        }

        // The modules are considered as compatible function-wise when all the conditions are met:
        //
        // - old module's public functions are a subset of the new module's public functions
        //   (i.e. we cannot remove or change public functions)
        // - old module's script functions are a subset of the new module's script functions
        //   (i.e. we cannot remove or change script functions)
        // - for any friend function that is removed or changed in the old module
        //   - if the function visibility is upgraded to public, it is OK
        //   - otherwise, it is considered as incompatible.
        //
        // NOTE: it is possible to relax the compatibility checking for a friend function, i.e.,
        // we can remove/change a friend function if the function is not used by any module in the
        // friend list. But for simplicity, we decided to go to the more restrictive form now and
        // we may revisit this in the future.
        for (name, old_func) in &old_module.exposed_functions {
            let new_func = match new_module.exposed_functions.get(name) {
                Some(new_func) => new_func,
                None => {
                    if matches!(old_func.visibility, Visibility::Friend) {
                        friend_linking = false;
                    } else {
                        struct_and_pub_function_linking = false;
                    }
                    continue;
                },
            };
            let is_vis_compatible = match (old_func.visibility, new_func.visibility) {
                // public must remain public
                (Visibility::Public, Visibility::Public) => true,
                (Visibility::Public, _) => false,
                // friend can become public or remain friend
                (Visibility::Friend, Visibility::Public)
                | (Visibility::Friend, Visibility::Friend) => true,
                (Visibility::Friend, _) => false,
                // private can become public or friend, or stay private
                (Visibility::Private, _) => true,
            };
            let is_entry_compatible = if old_module.file_format_version < VERSION_5
                && new_module.file_format_version < VERSION_5
            {
                // if it was public(script), it must remain pubic(script)
                // if it was not public(script), it _cannot_ become public(script)
                old_func.is_entry == new_func.is_entry
            } else {
                // If it was an entry function, it must remain one.
                // If it was not an entry function, it is allowed to become one.
                !old_func.is_entry || new_func.is_entry
            };
            if !is_vis_compatible
                || !is_entry_compatible
                || old_func.parameters != new_func.parameters
                || old_func.return_ != new_func.return_
                || !fun_type_parameters_compatibile(
                    &old_func.type_parameters,
                    &new_func.type_parameters,
                )
            {
                if matches!(old_func.visibility, Visibility::Friend) {
                    friend_linking = false;
                } else {
                    struct_and_pub_function_linking = false;
                }
            }
        }

        // check friend declarations compatibility
        //
        // - additions to the list are allowed
        // - removals are not allowed
        //
        let old_friend_module_ids: BTreeSet<_> = old_module.friends.iter().cloned().collect();
        let new_friend_module_ids: BTreeSet<_> = new_module.friends.iter().cloned().collect();
        if !old_friend_module_ids.is_subset(&new_friend_module_ids) {
            friend_linking = false;
        }

        if self.check_struct_and_pub_function_linking && !struct_and_pub_function_linking {
            return Err(PartialVMError::new(
                StatusCode::BACKWARD_INCOMPATIBLE_MODULE_UPDATE,
            ));
        }
        if self.check_struct_layout && !struct_layout {
            return Err(PartialVMError::new(
                StatusCode::BACKWARD_INCOMPATIBLE_MODULE_UPDATE,
            ));
        }
        if self.check_friend_linking && !friend_linking {
            return Err(PartialVMError::new(
                StatusCode::BACKWARD_INCOMPATIBLE_MODULE_UPDATE,
            ));
        }

        Ok(())
    }
}

// When upgrading, the new abilities must be a superset of the old abilities.
// Adding an ability is fine, but removing an ability could cause existing usages to fail.
fn struct_abilities_compatibile(old_abilities: AbilitySet, new_abilities: AbilitySet) -> bool {
    old_abilities.is_subset(new_abilities)
}

// When upgrading, the new type parameters must be the same length, and the new type parameter
// constraints must be compatible
fn fun_type_parameters_compatibile(
    old_type_parameters: &[AbilitySet],
    new_type_parameters: &[AbilitySet],
) -> bool {
    old_type_parameters.len() == new_type_parameters.len()
        && old_type_parameters.iter().zip(new_type_parameters).all(
            |(old_type_parameter_constraint, new_type_parameter_constraint)| {
                type_parameter_constraints_compatibile(
                    *old_type_parameter_constraint,
                    *new_type_parameter_constraint,
                )
            },
        )
}

fn struct_type_parameters_compatibile(
    old_type_parameters: &[StructTypeParameter],
    new_type_parameters: &[StructTypeParameter],
) -> bool {
    old_type_parameters.len() == new_type_parameters.len()
        && old_type_parameters.iter().zip(new_type_parameters).all(
            |(old_type_parameter, new_type_parameter)| {
                type_parameter_phantom_decl_compatibile(old_type_parameter, new_type_parameter)
                    && type_parameter_constraints_compatibile(
                        old_type_parameter.constraints,
                        new_type_parameter.constraints,
                    )
            },
        )
}

// When upgrading, the new constraints must be a subset of (or equal to) the old constraints.
// Removing an ability is fine, but adding an ability could cause existing callsites to fail
fn type_parameter_constraints_compatibile(
    old_type_constraints: AbilitySet,
    new_type_constraints: AbilitySet,
) -> bool {
    new_type_constraints.is_subset(old_type_constraints)
}

// Adding a phantom annotation to a parameter won't break clients because that can only increase the
// the set of abilities in struct instantiations. Put it differently, adding phantom declarations
// relaxes the requirements for clients.
fn type_parameter_phantom_decl_compatibile(
    old_type_parameter: &StructTypeParameter,
    new_type_parameter: &StructTypeParameter,
) -> bool {
    // old_type_paramter.is_phantom => new_type_parameter.is_phantom
    !old_type_parameter.is_phantom || new_type_parameter.is_phantom
}
