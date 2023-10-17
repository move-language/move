use alloc::borrow::ToOwned;
use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use move_binary_format::access::ModuleAccess;
use move_binary_format::file_format::{
    Ability, AbilitySet, Signature, SignatureToken, StructFieldInformation, StructHandleIndex,
    Visibility,
};
use move_binary_format::CompiledModule;
use move_core_types::account_address::AccountAddress;
use move_core_types::identifier::Identifier;
use move_core_types::language_storage::ModuleId;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Move module ABI.
pub struct ModuleAbi {
    /// Module ID.
    pub id: ModuleId,
    /// Friends.
    pub friends: Vec<Friend>,
    /// Structs.
    pub structs: Vec<Struct>,
    /// Functions.
    pub funcs: Vec<Function>,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Move struct definition.
pub struct Struct {
    /// Name identifier.
    pub name: Identifier,
    /// Generic type abilities.
    pub type_parameters: Vec<TypeAbilities>,
    /// Struct abilities.
    pub abilities: TypeAbilities,
    /// Struct elements.
    pub fields: Vec<Field>,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Represntation for a struct element.
pub struct Field {
    /// Name.
    pub name: Identifier,
    /// Type.
    pub tp: Type,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Generic type abilities.
pub struct TypeAbilities {
    /// Abilities.
    pub abilities: Vec<TypeAbility>,
}

impl From<&AbilitySet> for TypeAbilities {
    fn from(val: &AbilitySet) -> Self {
        TypeAbilities {
            abilities: val
                .into_iter()
                .map(|a| match a {
                    Ability::Copy => TypeAbility::Copy,
                    Ability::Drop => TypeAbility::Drop,
                    Ability::Store => TypeAbility::Store,
                    Ability::Key => TypeAbility::Key,
                })
                .collect(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Move type abilities.
pub enum TypeAbility {
    Copy,
    Drop,
    Store,
    Key,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Move type.
pub enum Type {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Address,
    Signer,
    Vector(Box<Type>),
    Struct(StructDef),
    Reference(Box<Type>),
    MutableReference(Box<Type>),
    TypeParameter(u16),
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
// TODO: Can we merge this one with the main struct definition.
/// Simple struct definition for type.
pub struct StructDef {
    /// Module ID.
    pub id: ModuleId,
    /// Name.
    pub name: Identifier,
    /// Struct fields.
    pub fields: Vec<Type>,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Move function.
pub struct Function {
    /// Name.
    pub name: Identifier,
    /// Visibility.
    pub visibility: FunctionVisibility,
    /// Generic type abilities.
    pub type_parameters: Vec<TypeAbilities>,
    /// Function arguments.
    pub parameters: Vec<Type>,
    /// Return types.
    pub returns: Vec<Type>,
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Function visibility.
// Private not needed since it's not accessible to outer modules.
pub enum FunctionVisibility {
    Public,
    Friend,
}

impl From<&Visibility> for FunctionVisibility {
    fn from(val: &Visibility) -> Self {
        match val {
            Visibility::Private => {
                // not possible
                FunctionVisibility::Public
            }
            Visibility::Public => FunctionVisibility::Public,
            Visibility::Friend => FunctionVisibility::Friend,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Ord, PartialOrd, Eq, PartialEq)]
/// Friend module.
pub struct Friend {
    /// Address of the module.
    pub address: AccountAddress,
    /// Name of the module.
    pub name: Identifier,
}

impl From<CompiledModule> for ModuleAbi {
    fn from(module: CompiledModule) -> Self {
        ModuleAbi {
            id: module.self_id(),
            friends: make_friend_abi(&module),
            structs: make_structs_abi(&module),
            funcs: make_func_abi(&module),
        }
    }
}

fn make_structs_abi(module: &CompiledModule) -> Vec<Struct> {
    module
        .struct_defs()
        .iter()
        .map(|sdef| {
            let handle = module.struct_handle_at(sdef.struct_handle);
            let type_parameters = handle
                .type_parameters
                .iter()
                .map(|tp| TypeAbilities::from(&tp.constraints))
                .collect();

            let fields = match &sdef.field_information {
                StructFieldInformation::Native => vec![],
                StructFieldInformation::Declared(defs) => defs
                    .iter()
                    .map(|field| Field {
                        name: module.identifier_at(field.name).to_owned(),
                        tp: make_type(&field.signature.0, module),
                    })
                    .collect(),
            };

            Struct {
                name: module.identifier_at(handle.name).to_owned(),
                type_parameters,
                abilities: TypeAbilities::from(&handle.abilities),
                fields,
            }
        })
        .collect()
}

fn make_type(tok: &SignatureToken, module: &CompiledModule) -> Type {
    match tok {
        SignatureToken::Bool => Type::Bool,
        SignatureToken::U8 => Type::U8,
        SignatureToken::U16 => Type::U16,
        SignatureToken::U32 => Type::U32,
        SignatureToken::U64 => Type::U64,
        SignatureToken::U128 => Type::U128,
        SignatureToken::U256 => Type::U256,
        SignatureToken::Address => Type::Address,
        SignatureToken::Signer => Type::Signer,
        SignatureToken::Vector(tp) => Type::Vector(Box::new(make_type(tp, module))),
        SignatureToken::Struct(idx) => Type::Struct(make_struct_def(*idx, &[], module)),
        SignatureToken::StructInstantiation(idx, tps) => {
            Type::Struct(make_struct_def(*idx, tps, module))
        }
        SignatureToken::Reference(rf) => Type::Reference(Box::new(make_type(rf, module))),
        SignatureToken::MutableReference(tp) => {
            Type::MutableReference(Box::new(make_type(tp, module)))
        }
        SignatureToken::TypeParameter(val) => Type::TypeParameter(*val),
    }
}

fn make_struct_def(
    idx: StructHandleIndex,
    tps: &[SignatureToken],
    module: &CompiledModule,
) -> StructDef {
    let struct_handle = module.struct_handle_at(idx);
    let struct_module_handle = module.module_handle_at(struct_handle.module);
    let id = module.module_id_for_handle(struct_module_handle);

    StructDef {
        id,
        name: module.identifier_at(struct_handle.name).to_owned(),
        fields: tps.iter().map(|tok| make_type(tok, module)).collect(),
    }
}

fn make_func_abi(module: &CompiledModule) -> Vec<Function> {
    module
        .function_defs()
        .iter()
        .filter(|def| match def.visibility {
            Visibility::Public | Visibility::Friend => true,
            Visibility::Private => false,
        })
        .map(|def| {
            let handle = module.function_handle_at(def.function);
            let Signature(parameters) = module.signature_at(handle.parameters);
            let Signature(return_) = module.signature_at(handle.return_);
            Function {
                name: module.identifier_at(handle.name).to_owned(),
                visibility: FunctionVisibility::from(&def.visibility),
                type_parameters: handle
                    .type_parameters
                    .iter()
                    .map(TypeAbilities::from)
                    .collect(),
                parameters: parameters.iter().map(|st| make_type(st, module)).collect(),
                returns: return_.iter().map(|st| make_type(st, module)).collect(),
            }
        })
        .collect()
}

fn make_friend_abi(module: &CompiledModule) -> Vec<Friend> {
    module
        .friend_decls()
        .iter()
        .map(|decl| Friend {
            address: *module.address_identifier_at(decl.address),
            name: module.identifier_at(decl.name).to_owned(),
        })
        .collect()
}
