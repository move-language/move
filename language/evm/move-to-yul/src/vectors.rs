// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// This file defines vector functionalities.

use crate::{context::Context, yul_functions::YulFunction, Generator};
use move_model::{
    emitln,
    model::{FunId, QualifiedInstId},
};

/// The size (in bytes) of the vector metadata, which is stored in front of the actual vector data.
pub const VECTOR_METADATA_SIZE: usize = 32;
/// The number of slots allocated initially for an empty vector.
pub const VECTOR_INITIAL_CAPACITY: usize = 2;

pub(crate) fn define_empty_fun(
    gen: &mut Generator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
) {
    assert_eq!(
        fun_id.inst.len(),
        1,
        "vector instantiated with non-one type parameter"
    );
    emitln!(ctx.writer, "() -> vector {");
    ctx.writer.indent();
    let type_size = ctx.type_size(fun_id.inst.get(0).unwrap());
    emitln!(
        ctx.writer,
        "vector := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::Malloc,
            std::iter::once(
                (VECTOR_METADATA_SIZE + type_size * VECTOR_INITIAL_CAPACITY).to_string()
            ),
        )
    );
    emitln!(
        ctx.writer,
        "{}",
        gen.call_builtin_str(
            ctx,
            YulFunction::MemoryStoreU64,
            vec![
                "add(vector, 8)".to_string(),
                VECTOR_INITIAL_CAPACITY.to_string()
            ]
            .into_iter()
        )
    );
    emitln!(
        ctx.writer,
        "vector := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::MakePtr,
            vec!["false".to_string(), "vector".to_string()].into_iter()
        )
    );
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_length_fun(
    gen: &mut Generator,
    ctx: &Context,
    _fun_id: &QualifiedInstId<FunId>,
) {
    emitln!(ctx.writer, "(v_ptr) -> len {");
    ctx.writer.indent();
    emitln!(
        ctx.writer,
        "let v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU256,
            std::iter::once("v_ptr".to_string())
        )
    );
    emitln!(
        ctx.writer,
        "len := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_borrow_fun(
    gen: &mut Generator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
) {
    assert_eq!(
        fun_id.inst.len(),
        1,
        "vector instantiated with non-one type parameter"
    );
    let elem_type = fun_id.inst.get(0).unwrap();
    let elem_type_size = ctx.type_size(elem_type);

    emitln!(ctx.writer, "(v_ptr, i) -> e_ptr {");
    ctx.writer.indent();

    emitln!(
        ctx.writer,
        "let v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU256,
            std::iter::once("v_ptr".to_string())
        )
    );

    emitln!(
        ctx.writer,
        "let size := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );

    emitln!(
        ctx.writer,
        "if {} {{ {} }}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::GtEq,
            vec!["i".to_string(), "size".to_string()].into_iter()
        ),
        &gen.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty())
    );

    // calculate byte offset at which the new element should be stored
    emitln!(
        ctx.writer,
        "e_ptr := {}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![
                "v".to_string(),
                format!("add({}, mul(i, {}))", VECTOR_METADATA_SIZE, elem_type_size)
            ]
            .into_iter()
        )
    );
    if ctx.type_is_struct(elem_type) {
        emitln!(
            ctx.writer,
            "let e := {}",
            gen.call_builtin_str(
                ctx,
                YulFunction::LoadU256,
                std::iter::once("e_ptr".to_string())
            )
        );
        emitln!(
            ctx.writer,
            "e_ptr := {}",
            gen.call_builtin_str(
                ctx,
                YulFunction::MakePtr,
                vec![false.to_string(), "e".to_string()].into_iter()
            )
        );
    }
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_pop_back_fun(
    gen: &mut Generator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
) {
    assert_eq!(
        fun_id.inst.len(),
        1,
        "vector instantiated with non-one type parameter"
    );
    let elem_type = fun_id.inst.get(0).unwrap();
    let elem_type_size = ctx.type_size(elem_type);

    emitln!(ctx.writer, "(v_ptr) -> e {");
    ctx.writer.indent();

    emitln!(
        ctx.writer,
        "let v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU256,
            std::iter::once("v_ptr".to_string())
        )
    );

    emitln!(
        ctx.writer,
        "let size := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );

    emitln!(
        ctx.writer,
        "if iszero(size) {{ {} }}",
        gen.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty())
    );

    emitln!(
        ctx.writer,
        "let e_ptr := {}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![
                "v".to_string(),
                format!(
                    "add({}, mul(sub(size, 1), {}))",
                    VECTOR_METADATA_SIZE, elem_type_size
                )
            ]
            .into_iter()
        )
    );
    emitln!(
        ctx.writer,
        "e := {}",
        gen.call_builtin_str(
            ctx,
            ctx.load_builtin_fun(elem_type),
            std::iter::once("e_ptr".to_string())
        )
    );

    // TODO: implement element's move to memory if vector is in global storage
    // and element is a struct or vector

    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            YulFunction::StoreU64,
            vec!["v".to_string(), "sub(size, 1)".to_string()].into_iter()
        )
    );

    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_push_back_fun(
    gen: &mut Generator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
) {
    assert_eq!(
        fun_id.inst.len(),
        1,
        "vector instantiated with non-one type parameter"
    );
    let elem_type = fun_id.inst.get(0).unwrap();
    let elem_type_size = ctx.type_size(elem_type);

    emitln!(ctx.writer, "(v_ptr, e) {");
    ctx.writer.indent();
    emitln!(ctx.writer, "let size, byte_offs, capacity");
    emitln!(
        ctx.writer,
        "let v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU256,
            std::iter::once("v_ptr".to_string())
        )
    );

    emitln!(
        ctx.writer,
        "size := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );

    // calculate byte offset at which the new element should be stored
    emitln!(
        ctx.writer,
        "let e_ptr := {}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![
                "v".to_string(),
                format!(
                    "add({}, mul(size, {}))",
                    VECTOR_METADATA_SIZE, elem_type_size
                )
            ]
            .into_iter()
        )
    );

    // store the new element there
    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            ctx.store_builtin_fun(elem_type),
            vec!["e_ptr".to_string(), "e".to_string()].into_iter()
        )
    );

    // TODO: implement element's move to storage if vector is in global storage
    // and element is a struct or vector

    // increment size
    emitln!(ctx.writer, "size := add(size, 1)");

    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            YulFunction::StoreU64,
            vec!["v".to_string(), "size".to_string()].into_iter()
        )
    );

    // load capacity
    emitln!(
        ctx.writer,
        "capacity := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU64,
            std::iter::once("$IndexPtr(v, 8)".to_string())
        )
    );

    // if in memory and size == capacity, resize
    emitln!(
        ctx.writer,
        "if and(iszero({}), eq(size, capacity)) {{",
        gen.call_builtin_str(
            ctx,
            YulFunction::IsStoragePtr,
            std::iter::once("v".to_string())
        ),
    );

    ctx.writer.indent();

    emitln!(
        ctx.writer,
        "let new_v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::ResizeVector,
            vec![
                "v".to_string(),
                "capacity".to_string(),
                elem_type_size.to_string()
            ]
            .into_iter()
        )
    );
    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            YulFunction::StoreU256,
            vec!["v_ptr".to_string(), "new_v".to_string()].into_iter()
        )
    );
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_swap_fun(gen: &mut Generator, ctx: &Context, fun_id: &QualifiedInstId<FunId>) {
    let elem_type = fun_id.inst.get(0).unwrap();
    let elem_type_size = ctx.type_size(elem_type);
    emitln!(ctx.writer, "(v_ptr, i, j) {");
    ctx.writer.indent();
    emitln!(
        ctx.writer,
        "let v := {}",
        gen.call_builtin_str(
            ctx,
            YulFunction::LoadU256,
            std::iter::once("v_ptr".to_string())
        )
    );
    emitln!(
        ctx.writer,
        "let size := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );

    emitln!(
        ctx.writer,
        "if or({}, {}) {{ {} }}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::GtEq,
            vec!["i".to_string(), "size".to_string()].into_iter()
        ),
        &gen.call_builtin_str(
            ctx,
            YulFunction::GtEq,
            vec!["j".to_string(), "size".to_string()].into_iter()
        ),
        &gen.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty())
    );

    emitln!(
        ctx.writer,
        "let i_ptr := {}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![
                "v".to_string(),
                format!("add({}, mul(i, {}))", VECTOR_METADATA_SIZE, elem_type_size)
            ]
            .into_iter()
        )
    );
    emitln!(
        ctx.writer,
        "let j_ptr := {}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::IndexPtr,
            vec![
                "v".to_string(),
                format!("add({}, mul(j, {}))", VECTOR_METADATA_SIZE, elem_type_size)
            ]
            .into_iter()
        )
    );
    emitln!(
        ctx.writer,
        "let i_val := {}",
        &gen.call_builtin_str(
            ctx,
            ctx.load_builtin_fun(elem_type),
            std::iter::once("i_ptr".to_string())
        )
    );
    emitln!(
        ctx.writer,
        "let j_val := {}",
        &gen.call_builtin_str(
            ctx,
            ctx.load_builtin_fun(elem_type),
            std::iter::once("j_ptr".to_string())
        )
    );
    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            ctx.store_builtin_fun(elem_type),
            vec!["i_ptr".to_string(), "j_val".to_string()].into_iter()
        )
    );
    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            ctx.store_builtin_fun(elem_type),
            vec!["j_ptr".to_string(), "i_val".to_string()].into_iter()
        )
    );
    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}

pub(crate) fn define_destroy_empty_fun(
    gen: &mut Generator,
    ctx: &Context,
    fun_id: &QualifiedInstId<FunId>,
) {
    assert_eq!(
        fun_id.inst.len(),
        1,
        "vector instantiated with non-one type parameter"
    );
    emitln!(ctx.writer, "(v) {");
    ctx.writer.indent();
    let type_size = ctx.type_size(fun_id.inst.get(0).unwrap());
    emitln!(
        ctx.writer,
        "let size := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once("v".to_string()))
    );

    // check that the vector is indeed empty

    emitln!(
        ctx.writer,
        "if {} {{ {} }}",
        &gen.call_builtin_str(
            ctx,
            YulFunction::LogicalNot,
            std::iter::once("iszero(size)".to_string())
        ),
        &gen.call_builtin_str(ctx, YulFunction::AbortBuiltin, std::iter::empty())
    );

    let capacity_ptr = gen.call_builtin_str(
        ctx,
        YulFunction::IndexPtr,
        vec!["v".to_string(), "8".to_string()].into_iter(),
    );
    emitln!(
        ctx.writer,
        "let capacity := {}",
        gen.call_builtin_str(ctx, YulFunction::LoadU64, std::iter::once(capacity_ptr))
    );

    emitln!(
        ctx.writer,
        &gen.call_builtin_str(
            ctx,
            YulFunction::Free,
            vec![
                "v".to_string(),
                format!(
                    "add({}, mul(capacity, {}))",
                    VECTOR_METADATA_SIZE, type_size
                )
            ]
            .into_iter()
        )
    );

    ctx.writer.unindent();
    emitln!(ctx.writer, "}");
}
