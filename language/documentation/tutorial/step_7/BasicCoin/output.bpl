
// ** Expanded prelude

// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// Basic theory for vectors using arrays. This version of vectors is not extensional.

type {:datatype} Vec _;

function {:constructor} Vec<T>(v: [int]T, l: int): Vec T;

function {:builtin "MapConst"} MapConstVec<T>(T): [int]T;
function DefaultVecElem<T>(): T;
function {:inline} DefaultVecMap<T>(): [int]T { MapConstVec(DefaultVecElem()) }

function {:inline} EmptyVec<T>(): Vec T {
    Vec(DefaultVecMap(), 0)
}

function {:inline} MakeVec1<T>(v: T): Vec T {
    Vec(DefaultVecMap()[0 := v], 1)
}

function {:inline} MakeVec2<T>(v1: T, v2: T): Vec T {
    Vec(DefaultVecMap()[0 := v1][1 := v2], 2)
}

function {:inline} MakeVec3<T>(v1: T, v2: T, v3: T): Vec T {
    Vec(DefaultVecMap()[0 := v1][1 := v2][2 := v3], 3)
}

function {:inline} MakeVec4<T>(v1: T, v2: T, v3: T, v4: T): Vec T {
    Vec(DefaultVecMap()[0 := v1][1 := v2][2 := v3][3 := v4], 4)
}

function {:inline} ExtendVec<T>(v: Vec T, elem: T): Vec T {
    (var l := l#Vec(v);
    Vec(v#Vec(v)[l := elem], l + 1))
}

function {:inline} ReadVec<T>(v: Vec T, i: int): T {
    v#Vec(v)[i]
}

function {:inline} LenVec<T>(v: Vec T): int {
    l#Vec(v)
}

function {:inline} IsEmptyVec<T>(v: Vec T): bool {
    l#Vec(v) == 0
}

function {:inline} RemoveVec<T>(v: Vec T): Vec T {
    (var l := l#Vec(v) - 1;
    Vec(v#Vec(v)[l := DefaultVecElem()], l))
}

function {:inline} RemoveAtVec<T>(v: Vec T, i: int): Vec T {
    (var l := l#Vec(v) - 1;
    Vec(
        (lambda j: int ::
           if j >= 0 && j < l then
               if j < i then v#Vec(v)[j] else v#Vec(v)[j+1]
           else DefaultVecElem()),
        l))
}

function {:inline} ConcatVec<T>(v1: Vec T, v2: Vec T): Vec T {
    (var l1, m1, l2, m2 := l#Vec(v1), v#Vec(v1), l#Vec(v2), v#Vec(v2);
    Vec(
        (lambda i: int ::
          if i >= 0 && i < l1 + l2 then
            if i < l1 then m1[i] else m2[i - l1]
          else DefaultVecElem()),
        l1 + l2))
}

function {:inline} ReverseVec<T>(v: Vec T): Vec T {
    (var l := l#Vec(v);
    Vec(
        (lambda i: int :: if 0 <= i && i < l then v#Vec(v)[l - i - 1] else DefaultVecElem()),
        l))
}

function {:inline} SliceVec<T>(v: Vec T, i: int, j: int): Vec T {
    (var m := v#Vec(v);
    Vec(
        (lambda k:int ::
          if 0 <= k && k < j - i then
            m[i + k]
          else
            DefaultVecElem()),
        (if j - i < 0 then 0 else j - i)))
}


function {:inline} UpdateVec<T>(v: Vec T, i: int, elem: T): Vec T {
    Vec(v#Vec(v)[i := elem], l#Vec(v))
}

function {:inline} SwapVec<T>(v: Vec T, i: int, j: int): Vec T {
    (var m := v#Vec(v);
    Vec(m[i := m[j]][j := m[i]], l#Vec(v)))
}

function {:inline} ContainsVec<T>(v: Vec T, e: T): bool {
    (var l := l#Vec(v);
    (exists i: int :: InRangeVec(v, i) && v#Vec(v)[i] == e))
}

function IndexOfVec<T>(v: Vec T, e: T): int;
axiom {:ctor "Vec"} (forall<T> v: Vec T, e: T :: {IndexOfVec(v, e)}
    (var i := IndexOfVec(v,e);
     if (!ContainsVec(v, e)) then i == -1
     else InRangeVec(v, i) && ReadVec(v, i) == e &&
        (forall j: int :: j >= 0 && j < i ==> ReadVec(v, j) != e)));

// This function should stay non-inlined as it guards many quantifiers
// over vectors. It appears important to have this uninterpreted for
// quantifier triggering.
function InRangeVec<T>(v: Vec T, i: int): bool {
    i >= 0 && i < LenVec(v)
}

// Copyright (c) The Diem Core Contributors
// SPDX-License-Identifier: Apache-2.0

// Boogie model for multisets, based on Boogie arrays. This theory assumes extensional equality for element types.

type {:datatype} Multiset _;
function {:constructor} Multiset<T>(v: [T]int, l: int): Multiset T;

function {:builtin "MapConst"} MapConstMultiset<T>(l: int): [T]int;

function {:inline} EmptyMultiset<T>(): Multiset T {
    Multiset(MapConstMultiset(0), 0)
}

function {:inline} LenMultiset<T>(s: Multiset T): int {
    l#Multiset(s)
}

function {:inline} ExtendMultiset<T>(s: Multiset T, v: T): Multiset T {
    (var len := l#Multiset(s);
    (var cnt := v#Multiset(s)[v];
    Multiset(v#Multiset(s)[v := (cnt + 1)], len + 1)))
}

// This function returns (s1 - s2). This function assumes that s2 is a subset of s1.
function {:inline} SubtractMultiset<T>(s1: Multiset T, s2: Multiset T): Multiset T {
    (var len1 := l#Multiset(s1);
    (var len2 := l#Multiset(s2);
    Multiset((lambda v:T :: v#Multiset(s1)[v]-v#Multiset(s2)[v]), len1-len2)))
}

function {:inline} IsEmptyMultiset<T>(s: Multiset T): bool {
    (l#Multiset(s) == 0) &&
    (forall v: T :: v#Multiset(s)[v] == 0)
}

function {:inline} IsSubsetMultiset<T>(s1: Multiset T, s2: Multiset T): bool {
    (l#Multiset(s1) <= l#Multiset(s2)) &&
    (forall v: T :: v#Multiset(s1)[v] <= v#Multiset(s2)[v])
}

function {:inline} ContainsMultiset<T>(s: Multiset T, v: T): bool {
    v#Multiset(s)[v] > 0
}



// ============================================================================================
// Primitive Types

const $MAX_U8: int;
axiom $MAX_U8 == 255;
const $MAX_U64: int;
axiom $MAX_U64 == 18446744073709551615;
const $MAX_U128: int;
axiom $MAX_U128 == 340282366920938463463374607431768211455;

type {:datatype} $Range;
function {:constructor} $Range(lb: int, ub: int): $Range;

function {:inline} $IsValid'bool'(v: bool): bool {
  true
}

function $IsValid'u8'(v: int): bool {
  v >= 0 && v <= $MAX_U8
}

function $IsValid'u64'(v: int): bool {
  v >= 0 && v <= $MAX_U64
}

function $IsValid'u128'(v: int): bool {
  v >= 0 && v <= $MAX_U128
}

function $IsValid'num'(v: int): bool {
  true
}

function $IsValid'address'(v: int): bool {
  // TODO: restrict max to representable addresses?
  v >= 0
}

function {:inline} $IsValidRange(r: $Range): bool {
   $IsValid'u64'(lb#$Range(r)) &&  $IsValid'u64'(ub#$Range(r))
}

// Intentionally not inlined so it serves as a trigger in quantifiers.
function $InRange(r: $Range, i: int): bool {
   lb#$Range(r) <= i && i < ub#$Range(r)
}


function {:inline} $IsEqual'u8'(x: int, y: int): bool {
    x == y
}

function {:inline} $IsEqual'u64'(x: int, y: int): bool {
    x == y
}

function {:inline} $IsEqual'u128'(x: int, y: int): bool {
    x == y
}

function {:inline} $IsEqual'num'(x: int, y: int): bool {
    x == y
}

function {:inline} $IsEqual'address'(x: int, y: int): bool {
    x == y
}

function {:inline} $IsEqual'bool'(x: bool, y: bool): bool {
    x == y
}

// ============================================================================================
// Memory

type {:datatype} $Location;

// A global resource location within the statically known resource type's memory,
// where `a` is an address.
function {:constructor} $Global(a: int): $Location;

// A local location. `i` is the unique index of the local.
function {:constructor} $Local(i: int): $Location;

// The location of a reference outside of the verification scope, for example, a `&mut` parameter
// of the function being verified. References with these locations don't need to be written back
// when mutation ends.
function {:constructor} $Param(i: int): $Location;


// A mutable reference which also carries its current value. Since mutable references
// are single threaded in Move, we can keep them together and treat them as a value
// during mutation until the point they are stored back to their original location.
type {:datatype} $Mutation _;
function {:constructor} $Mutation<T>(l: $Location, p: Vec int, v: T): $Mutation T;

// Representation of memory for a given type.
type {:datatype} $Memory _;
function {:constructor} $Memory<T>(domain: [int]bool, contents: [int]T): $Memory T;

function {:builtin "MapConst"} $ConstMemoryDomain(v: bool): [int]bool;
function {:builtin "MapConst"} $ConstMemoryContent<T>(v: T): [int]T;
axiom $ConstMemoryDomain(false) == (lambda i: int :: false);
axiom $ConstMemoryDomain(true) == (lambda i: int :: true);


// Dereferences a mutation.
function {:inline} $Dereference<T>(ref: $Mutation T): T {
    v#$Mutation(ref)
}

// Update the value of a mutation.
function {:inline} $UpdateMutation<T>(m: $Mutation T, v: T): $Mutation T {
    $Mutation(l#$Mutation(m), p#$Mutation(m), v)
}

function {:inline} $ChildMutation<T1, T2>(m: $Mutation T1, offset: int, v: T2): $Mutation T2 {
    $Mutation(l#$Mutation(m), ExtendVec(p#$Mutation(m), offset), v)
}

// Return true of the mutation is a parent of a child which was derived with the given edge offset. This
// is used to implement write-back choices.
function {:inline} $IsParentMutation<T1, T2>(parent: $Mutation T1, edge: int, child: $Mutation T2 ): bool {
    l#$Mutation(parent) == l#$Mutation(child) &&
    (var pp := p#$Mutation(parent);
    (var cp := p#$Mutation(child);
    (var pl := LenVec(pp);
    (var cl := LenVec(cp);
     cl == pl + 1 &&
     (forall i: int:: i >= 0 && i < pl ==> ReadVec(pp, i) ==  ReadVec(cp, i)) &&
     $EdgeMatches(ReadVec(cp, pl), edge)
    ))))
}

// Return true of the mutation is a parent of a child, for hyper edge.
function {:inline} $IsParentMutationHyper<T1, T2>(parent: $Mutation T1, hyper_edge: Vec int, child: $Mutation T2 ): bool {
    l#$Mutation(parent) == l#$Mutation(child) &&
    (var pp := p#$Mutation(parent);
    (var cp := p#$Mutation(child);
    (var pl := LenVec(pp);
    (var cl := LenVec(cp);
    (var el := LenVec(hyper_edge);
     cl == pl + el &&
     (forall i: int:: i >= 0 && i < pl ==> ReadVec(pp, i) == ReadVec(cp, i)) &&
     (forall i: int:: i >= 0 && i < el ==> $EdgeMatches(ReadVec(cp, pl + i), ReadVec(hyper_edge, i)))
    )))))
}

function {:inline} $EdgeMatches(edge: int, edge_pattern: int): bool {
    edge_pattern == -1 // wildcard
    || edge_pattern == edge
}



function {:inline} $SameLocation<T1, T2>(m1: $Mutation T1, m2: $Mutation T2): bool {
    l#$Mutation(m1) == l#$Mutation(m2)
}

function {:inline} $HasGlobalLocation<T>(m: $Mutation T): bool {
    is#$Global(l#$Mutation(m))
}

function {:inline} $HasLocalLocation<T>(m: $Mutation T, idx: int): bool {
    l#$Mutation(m) == $Local(idx)
}

function {:inline} $GlobalLocationAddress<T>(m: $Mutation T): int {
    a#$Global(l#$Mutation(m))
}



// Tests whether resource exists.
function {:inline} $ResourceExists<T>(m: $Memory T, addr: int): bool {
    domain#$Memory(m)[addr]
}

// Obtains Value of given resource.
function {:inline} $ResourceValue<T>(m: $Memory T, addr: int): T {
    contents#$Memory(m)[addr]
}

// Update resource.
function {:inline} $ResourceUpdate<T>(m: $Memory T, a: int, v: T): $Memory T {
    $Memory(domain#$Memory(m)[a := true], contents#$Memory(m)[a := v])
}

// Remove resource.
function {:inline} $ResourceRemove<T>(m: $Memory T, a: int): $Memory T {
    $Memory(domain#$Memory(m)[a := false], contents#$Memory(m))
}

// Copies resource from memory s to m.
function {:inline} $ResourceCopy<T>(m: $Memory T, s: $Memory T, a: int): $Memory T {
    $Memory(domain#$Memory(m)[a := domain#$Memory(s)[a]],
            contents#$Memory(m)[a := contents#$Memory(s)[a]])
}



// ============================================================================================
// Abort Handling

var $abort_flag: bool;
var $abort_code: int;

function {:inline} $process_abort_code(code: int): int {
    code
}

const $EXEC_FAILURE_CODE: int;
axiom $EXEC_FAILURE_CODE == -1;

// TODO(wrwg): currently we map aborts of native functions like those for vectors also to
//   execution failure. This may need to be aligned with what the runtime actually does.

procedure {:inline 1} $ExecFailureAbort() {
    $abort_flag := true;
    $abort_code := $EXEC_FAILURE_CODE;
}

procedure {:inline 1} $InitVerification() {
    // Set abort_flag to false, and havoc abort_code
    $abort_flag := false;
    havoc $abort_code;
    // Initialize event store
    call $InitEventStore();
}

// ============================================================================================
// Instructions


procedure {:inline 1} $CastU8(src: int) returns (dst: int)
{
    if (src > $MAX_U8) {
        call $ExecFailureAbort();
        return;
    }
    dst := src;
}

procedure {:inline 1} $CastU64(src: int) returns (dst: int)
{
    if (src > $MAX_U64) {
        call $ExecFailureAbort();
        return;
    }
    dst := src;
}

procedure {:inline 1} $CastU128(src: int) returns (dst: int)
{
    if (src > $MAX_U128) {
        call $ExecFailureAbort();
        return;
    }
    dst := src;
}

procedure {:inline 1} $AddU8(src1: int, src2: int) returns (dst: int)
{
    if (src1 + src2 > $MAX_U8) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 + src2;
}

procedure {:inline 1} $AddU64(src1: int, src2: int) returns (dst: int)
{
    if (src1 + src2 > $MAX_U64) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 + src2;
}

procedure {:inline 1} $AddU64_unchecked(src1: int, src2: int) returns (dst: int)
{
    dst := src1 + src2;
}

procedure {:inline 1} $AddU128(src1: int, src2: int) returns (dst: int)
{
    if (src1 + src2 > $MAX_U128) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 + src2;
}

procedure {:inline 1} $AddU128_unchecked(src1: int, src2: int) returns (dst: int)
{
    dst := src1 + src2;
}

procedure {:inline 1} $Sub(src1: int, src2: int) returns (dst: int)
{
    if (src1 < src2) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 - src2;
}

// Note that *not* inlining the shl/shr functions avoids timeouts. It appears that Z3 can reason
// better about this if it is an axiomatized function.
function $shl(src1: int, p: int): int {
    if p == 8 then src1 * 256
    else if p == 16 then src1 * 65536
    else if p == 32 then src1 * 4294967296
    else if p == 64 then src1 * 18446744073709551616
    // Value is undefined, otherwise.
    else -1
}

function $shr(src1: int, p: int): int {
    if p == 8 then src1 div 256
    else if p == 16 then src1 div 65536
    else if p == 32 then src1 div 4294967296
    else if p == 64 then src1 div 18446744073709551616
    // Value is undefined, otherwise.
    else -1
}

// TODO: fix this and $Shr to drop bits on overflow. Requires $Shl8, $Shl64, and $Shl128
procedure {:inline 1} $Shl(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    res := $shl(src1, src2);
    assert res >= 0;   // restriction: shift argument must be 8, 16, 32, or 64
    dst := res;
}

procedure {:inline 1} $Shr(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    res := $shr(src1, src2);
    assert res >= 0;   // restriction: shift argument must be 8, 16, 32, or 64
    dst := res;
}

procedure {:inline 1} $MulU8(src1: int, src2: int) returns (dst: int)
{
    if (src1 * src2 > $MAX_U8) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 * src2;
}

procedure {:inline 1} $MulU64(src1: int, src2: int) returns (dst: int)
{
    if (src1 * src2 > $MAX_U64) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 * src2;
}

procedure {:inline 1} $MulU128(src1: int, src2: int) returns (dst: int)
{
    if (src1 * src2 > $MAX_U128) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 * src2;
}

procedure {:inline 1} $Div(src1: int, src2: int) returns (dst: int)
{
    if (src2 == 0) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 div src2;
}

procedure {:inline 1} $Mod(src1: int, src2: int) returns (dst: int)
{
    if (src2 == 0) {
        call $ExecFailureAbort();
        return;
    }
    dst := src1 mod src2;
}

procedure {:inline 1} $ArithBinaryUnimplemented(src1: int, src2: int) returns (dst: int);

procedure {:inline 1} $Lt(src1: int, src2: int) returns (dst: bool)
{
    dst := src1 < src2;
}

procedure {:inline 1} $Gt(src1: int, src2: int) returns (dst: bool)
{
    dst := src1 > src2;
}

procedure {:inline 1} $Le(src1: int, src2: int) returns (dst: bool)
{
    dst := src1 <= src2;
}

procedure {:inline 1} $Ge(src1: int, src2: int) returns (dst: bool)
{
    dst := src1 >= src2;
}

procedure {:inline 1} $And(src1: bool, src2: bool) returns (dst: bool)
{
    dst := src1 && src2;
}

procedure {:inline 1} $Or(src1: bool, src2: bool) returns (dst: bool)
{
    dst := src1 || src2;
}

procedure {:inline 1} $Not(src: bool) returns (dst: bool)
{
    dst := !src;
}

// Pack and Unpack are auto-generated for each type T


// ==================================================================================
// Native Vector

function {:inline} $SliceVecByRange<T>(v: Vec T, r: $Range): Vec T {
    SliceVec(v, lb#$Range(r), ub#$Range(r))
}

// ----------------------------------------------------------------------------------
// Native Vector implementation for element type `u8`

// Not inlined. It appears faster this way.
function $IsEqual'vec'u8''(v1: Vec (int), v2: Vec (int)): bool {
    LenVec(v1) == LenVec(v2) &&
    (forall i: int:: InRangeVec(v1, i) ==> $IsEqual'u8'(ReadVec(v1, i), ReadVec(v2, i)))
}

// Not inlined.
function $IsValid'vec'u8''(v: Vec (int)): bool {
    $IsValid'u64'(LenVec(v)) &&
    (forall i: int:: InRangeVec(v, i) ==> $IsValid'u8'(ReadVec(v, i)))
}


function {:inline} $ContainsVec'u8'(v: Vec (int), e: int): bool {
    (exists i: int :: $IsValid'u64'(i) && InRangeVec(v, i) && $IsEqual'u8'(ReadVec(v, i), e))
}

function $IndexOfVec'u8'(v: Vec (int), e: int): int;
axiom (forall v: Vec (int), e: int:: {$IndexOfVec'u8'(v, e)}
    (var i := $IndexOfVec'u8'(v, e);
     if (!$ContainsVec'u8'(v, e)) then i == -1
     else $IsValid'u64'(i) && InRangeVec(v, i) && $IsEqual'u8'(ReadVec(v, i), e) &&
        (forall j: int :: $IsValid'u64'(j) && j >= 0 && j < i ==> !$IsEqual'u8'(ReadVec(v, j), e))));


function {:inline} $RangeVec'u8'(v: Vec (int)): $Range {
    $Range(0, LenVec(v))
}


function {:inline} $EmptyVec'u8'(): Vec (int) {
    EmptyVec()
}

procedure {:inline 1} $1_Vector_empty'u8'() returns (v: Vec (int)) {
    v := EmptyVec();
}

function {:inline} $1_Vector_$empty'u8'(): Vec (int) {
    EmptyVec()
}

procedure {:inline 1} $1_Vector_is_empty'u8'(v: Vec (int)) returns (b: bool) {
    b := IsEmptyVec(v);
}

procedure {:inline 1} $1_Vector_push_back'u8'(m: $Mutation (Vec (int)), val: int) returns (m': $Mutation (Vec (int))) {
    m' := $UpdateMutation(m, ExtendVec($Dereference(m), val));
}

function {:inline} $1_Vector_$push_back'u8'(v: Vec (int), val: int): Vec (int) {
    ExtendVec(v, val)
}

procedure {:inline 1} $1_Vector_pop_back'u8'(m: $Mutation (Vec (int))) returns (e: int, m': $Mutation (Vec (int))) {
    var v: Vec (int);
    var len: int;
    v := $Dereference(m);
    len := LenVec(v);
    if (len == 0) {
        call $ExecFailureAbort();
        return;
    }
    e := ReadVec(v, len-1);
    m' := $UpdateMutation(m, RemoveVec(v));
}

procedure {:inline 1} $1_Vector_append'u8'(m: $Mutation (Vec (int)), other: Vec (int)) returns (m': $Mutation (Vec (int))) {
    m' := $UpdateMutation(m, ConcatVec($Dereference(m), other));
}

procedure {:inline 1} $1_Vector_reverse'u8'(m: $Mutation (Vec (int))) returns (m': $Mutation (Vec (int))) {
    m' := $UpdateMutation(m, ReverseVec($Dereference(m)));
}

procedure {:inline 1} $1_Vector_length'u8'(v: Vec (int)) returns (l: int) {
    l := LenVec(v);
}

function {:inline} $1_Vector_$length'u8'(v: Vec (int)): int {
    LenVec(v)
}

procedure {:inline 1} $1_Vector_borrow'u8'(v: Vec (int), i: int) returns (dst: int) {
    if (!InRangeVec(v, i)) {
        call $ExecFailureAbort();
        return;
    }
    dst := ReadVec(v, i);
}

function {:inline} $1_Vector_$borrow'u8'(v: Vec (int), i: int): int {
    ReadVec(v, i)
}

procedure {:inline 1} $1_Vector_borrow_mut'u8'(m: $Mutation (Vec (int)), index: int)
returns (dst: $Mutation (int), m': $Mutation (Vec (int)))
{
    var v: Vec (int);
    v := $Dereference(m);
    if (!InRangeVec(v, index)) {
        call $ExecFailureAbort();
        return;
    }
    dst := $Mutation(l#$Mutation(m), ExtendVec(p#$Mutation(m), index), ReadVec(v, index));
    m' := m;
}

function {:inline} $1_Vector_$borrow_mut'u8'(v: Vec (int), i: int): int {
    ReadVec(v, i)
}

procedure {:inline 1} $1_Vector_destroy_empty'u8'(v: Vec (int)) {
    if (!IsEmptyVec(v)) {
      call $ExecFailureAbort();
    }
}

procedure {:inline 1} $1_Vector_swap'u8'(m: $Mutation (Vec (int)), i: int, j: int) returns (m': $Mutation (Vec (int)))
{
    var v: Vec (int);
    v := $Dereference(m);
    if (!InRangeVec(v, i) || !InRangeVec(v, j)) {
        call $ExecFailureAbort();
        return;
    }
    m' := $UpdateMutation(m, SwapVec(v, i, j));
}

function {:inline} $1_Vector_$swap'u8'(v: Vec (int), i: int, j: int): Vec (int) {
    SwapVec(v, i, j)
}

procedure {:inline 1} $1_Vector_remove'u8'(m: $Mutation (Vec (int)), i: int) returns (e: int, m': $Mutation (Vec (int)))
{
    var v: Vec (int);

    v := $Dereference(m);

    if (!InRangeVec(v, i)) {
        call $ExecFailureAbort();
        return;
    }
    e := ReadVec(v, i);
    m' := $UpdateMutation(m, RemoveAtVec(v, i));
}

procedure {:inline 1} $1_Vector_swap_remove'u8'(m: $Mutation (Vec (int)), i: int) returns (e: int, m': $Mutation (Vec (int)))
{
    var len: int;
    var v: Vec (int);

    v := $Dereference(m);
    len := LenVec(v);
    if (!InRangeVec(v, i)) {
        call $ExecFailureAbort();
        return;
    }
    e := ReadVec(v, i);
    m' := $UpdateMutation(m, RemoveVec(SwapVec(v, i, len-1)));
}

procedure {:inline 1} $1_Vector_contains'u8'(v: Vec (int), e: int) returns (res: bool)  {
    res := $ContainsVec'u8'(v, e);
}

procedure {:inline 1}
$1_Vector_index_of'u8'(v: Vec (int), e: int) returns (res1: bool, res2: int) {
    res2 := $IndexOfVec'u8'(v, e);
    if (res2 >= 0) {
        res1 := true;
    } else {
        res1 := false;
        res2 := 0;
    }
}


// ==================================================================================
// Native Hash

// Hash is modeled as an otherwise uninterpreted injection.
// In truth, it is not an injection since the domain has greater cardinality
// (arbitrary length vectors) than the co-domain (vectors of length 32).  But it is
// common to assume in code there are no hash collisions in practice.  Fortunately,
// Boogie is not smart enough to recognized that there is an inconsistency.
// FIXME: If we were using a reliable extensional theory of arrays, and if we could use ==
// instead of $IsEqual, we might be able to avoid so many quantified formulas by
// using a sha2_inverse function in the ensures conditions of Hash_sha2_256 to
// assert that sha2/3 are injections without using global quantified axioms.


function $1_Hash_sha2(val: Vec int): Vec int;

// This says that Hash_sha2 is bijective.
axiom (forall v1,v2: Vec int :: {$1_Hash_sha2(v1), $1_Hash_sha2(v2)}
       $IsEqual'vec'u8''(v1, v2) <==> $IsEqual'vec'u8''($1_Hash_sha2(v1), $1_Hash_sha2(v2)));

procedure $1_Hash_sha2_256(val: Vec int) returns (res: Vec int);
ensures res == $1_Hash_sha2(val);     // returns Hash_sha2 Value
ensures $IsValid'vec'u8''(res);    // result is a legal vector of U8s.
ensures LenVec(res) == 32;               // result is 32 bytes.

// Spec version of Move native function.
function {:inline} $1_Hash_$sha2_256(val: Vec int): Vec int {
    $1_Hash_sha2(val)
}

// similarly for Hash_sha3
function $1_Hash_sha3(val: Vec int): Vec int;

axiom (forall v1,v2: Vec int :: {$1_Hash_sha3(v1), $1_Hash_sha3(v2)}
       $IsEqual'vec'u8''(v1, v2) <==> $IsEqual'vec'u8''($1_Hash_sha3(v1), $1_Hash_sha3(v2)));

procedure $1_Hash_sha3_256(val: Vec int) returns (res: Vec int);
ensures res == $1_Hash_sha3(val);     // returns Hash_sha3 Value
ensures $IsValid'vec'u8''(res);    // result is a legal vector of U8s.
ensures LenVec(res) == 32;               // result is 32 bytes.

// Spec version of Move native function.
function {:inline} $1_Hash_$sha3_256(val: Vec int): Vec int {
    $1_Hash_sha3(val)
}

// ==================================================================================
// Native diem_account

procedure {:inline 1} $1_DiemAccount_create_signer(
  addr: int
) returns (signer: $signer) {
    // A signer is currently identical to an address.
    signer := $signer(addr);
}

procedure {:inline 1} $1_DiemAccount_destroy_signer(
  signer: $signer
) {
  return;
}

// ==================================================================================
// Native account

procedure {:inline 1} $1_Account_create_signer(
  addr: int
) returns (signer: $signer) {
    // A signer is currently identical to an address.
    signer := $signer(addr);
}

// ==================================================================================
// Native Signer

type {:datatype} $signer;
function {:constructor} $signer($addr: int): $signer;
function {:inline} $IsValid'signer'(s: $signer): bool {
    $IsValid'address'($addr#$signer(s))
}
function {:inline} $IsEqual'signer'(s1: $signer, s2: $signer): bool {
    s1 == s2
}

procedure {:inline 1} $1_Signer_borrow_address(signer: $signer) returns (res: int) {
    res := $addr#$signer(signer);
}

function {:inline} $1_Signer_$borrow_address(signer: $signer): int
{
    $addr#$signer(signer)
}

function $1_Signer_is_txn_signer(s: $signer): bool;

function $1_Signer_is_txn_signer_addr(a: int): bool;


// ==================================================================================
// Native signature

// Signature related functionality is handled via uninterpreted functions. This is sound
// currently because we verify every code path based on signature verification with
// an arbitrary interpretation.

function $1_Signature_$ed25519_validate_pubkey(public_key: Vec int): bool;
function $1_Signature_$ed25519_verify(signature: Vec int, public_key: Vec int, message: Vec int): bool;

// Needed because we do not have extensional equality:
axiom (forall k1, k2: Vec int ::
    {$1_Signature_$ed25519_validate_pubkey(k1), $1_Signature_$ed25519_validate_pubkey(k2)}
    $IsEqual'vec'u8''(k1, k2) ==> $1_Signature_$ed25519_validate_pubkey(k1) == $1_Signature_$ed25519_validate_pubkey(k2));
axiom (forall s1, s2, k1, k2, m1, m2: Vec int ::
    {$1_Signature_$ed25519_verify(s1, k1, m1), $1_Signature_$ed25519_verify(s2, k2, m2)}
    $IsEqual'vec'u8''(s1, s2) && $IsEqual'vec'u8''(k1, k2) && $IsEqual'vec'u8''(m1, m2)
    ==> $1_Signature_$ed25519_verify(s1, k1, m1) == $1_Signature_$ed25519_verify(s2, k2, m2));


procedure {:inline 1} $1_Signature_ed25519_validate_pubkey(public_key: Vec int) returns (res: bool) {
    res := $1_Signature_$ed25519_validate_pubkey(public_key);
}

procedure {:inline 1} $1_Signature_ed25519_verify(
        signature: Vec int, public_key: Vec int, message: Vec int) returns (res: bool) {
    res := $1_Signature_$ed25519_verify(signature, public_key, message);
}


// ==================================================================================
// Native BCS::serialize


// ==================================================================================
// Native Event module



procedure {:inline 1} $InitEventStore() {
}



//==================================
// Begin Translation



// Given Types for Type Parameters

type #0;
function {:inline} $IsEqual'#0'(x1: #0, x2: #0): bool { x1 == x2 }
function {:inline} $IsValid'#0'(x: #0): bool { true }

// fun Signer::address_of [baseline] at ./../../../../move-stdlib/sources/Signer.move:12:5+77
procedure {:inline 1} $1_Signer_address_of(_$t0: $signer) returns ($ret0: int)
{
    // declare local variables
    var $t1: int;
    var $t2: int;
    var $t0: $signer;
    var $temp_0'address': int;
    var $temp_0'signer': $signer;
    $t0 := _$t0;

    // bytecode translation starts here
    // trace_local[s]($t0) at ./../../../../move-stdlib/sources/Signer.move:12:5+1
    assume {:print "$at(24,389,390)"} true;
    assume {:print "$track_local(0,0,0):", $t0} $t0 == $t0;

    // $t1 := Signer::borrow_address($t0) on_abort goto L2 with $t2 at ./../../../../move-stdlib/sources/Signer.move:13:10+17
    assume {:print "$at(24,443,460)"} true;
    call $t1 := $1_Signer_borrow_address($t0);
    if ($abort_flag) {
        assume {:print "$at(24,443,460)"} true;
        $t2 := $abort_code;
        assume {:print "$track_abort(0,0):", $t2} $t2 == $t2;
        goto L2;
    }

    // trace_return[0]($t1) at ./../../../../move-stdlib/sources/Signer.move:13:9+18
    assume {:print "$track_return(0,0,0):", $t1} $t1 == $t1;

    // label L1 at ./../../../../move-stdlib/sources/Signer.move:14:5+1
    assume {:print "$at(24,465,466)"} true;
L1:

    // return $t1 at ./../../../../move-stdlib/sources/Signer.move:14:5+1
    $ret0 := $t1;
    return;

    // label L2 at ./../../../../move-stdlib/sources/Signer.move:14:5+1
L2:

    // abort($t2) at ./../../../../move-stdlib/sources/Signer.move:14:5+1
    $abort_code := $t2;
    $abort_flag := true;
    return;

}

// struct BasicCoin::Balance<#0> at ./sources/BasicCoin.move:15:5+77
type {:datatype} $cafe_BasicCoin_Balance'#0';
function {:constructor} $cafe_BasicCoin_Balance'#0'($coin: $cafe_BasicCoin_Coin'#0'): $cafe_BasicCoin_Balance'#0';
function {:inline} $Update'$cafe_BasicCoin_Balance'#0''_coin(s: $cafe_BasicCoin_Balance'#0', x: $cafe_BasicCoin_Coin'#0'): $cafe_BasicCoin_Balance'#0' {
    $cafe_BasicCoin_Balance'#0'(x)
}
function $IsValid'$cafe_BasicCoin_Balance'#0''(s: $cafe_BasicCoin_Balance'#0'): bool {
    $IsValid'$cafe_BasicCoin_Coin'#0''($coin#$cafe_BasicCoin_Balance'#0'(s))
}
function {:inline} $IsEqual'$cafe_BasicCoin_Balance'#0''(s1: $cafe_BasicCoin_Balance'#0', s2: $cafe_BasicCoin_Balance'#0'): bool {
    s1 == s2
}
var $cafe_BasicCoin_Balance'#0'_$memory: $Memory $cafe_BasicCoin_Balance'#0';

// struct BasicCoin::Coin<#0> at ./sources/BasicCoin.move:11:5+66
type {:datatype} $cafe_BasicCoin_Coin'#0';
function {:constructor} $cafe_BasicCoin_Coin'#0'($value: int): $cafe_BasicCoin_Coin'#0';
function {:inline} $Update'$cafe_BasicCoin_Coin'#0''_value(s: $cafe_BasicCoin_Coin'#0', x: int): $cafe_BasicCoin_Coin'#0' {
    $cafe_BasicCoin_Coin'#0'(x)
}
function $IsValid'$cafe_BasicCoin_Coin'#0''(s: $cafe_BasicCoin_Coin'#0'): bool {
    $IsValid'u64'($value#$cafe_BasicCoin_Coin'#0'(s))
}
function {:inline} $IsEqual'$cafe_BasicCoin_Coin'#0''(s1: $cafe_BasicCoin_Coin'#0', s2: $cafe_BasicCoin_Coin'#0'): bool {
    s1 == s2
}

// fun BasicCoin::balance_of<#0> [baseline] at ./sources/BasicCoin.move:34:5+136
procedure {:inline 1} $cafe_BasicCoin_balance_of'#0'(_$t0: int) returns ($ret0: int)
{
    // declare local variables
    var $t1: $cafe_BasicCoin_Balance'#0';
    var $t2: int;
    var $t3: $cafe_BasicCoin_Coin'#0';
    var $t4: int;
    var $t0: int;
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;

    // bytecode translation starts here
    // trace_local[owner]($t0) at ./sources/BasicCoin.move:34:5+1
    assume {:print "$at(23,1324,1325)"} true;
    assume {:print "$track_local(2,0,0):", $t0} $t0 == $t0;

    // $t1 := get_global<BasicCoin::Balance<#0>>($t0) on_abort goto L2 with $t2 at ./sources/BasicCoin.move:35:9+13
    assume {:print "$at(23,1404,1417)"} true;
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t1 := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0);
    }
    if ($abort_flag) {
        assume {:print "$at(23,1404,1417)"} true;
        $t2 := $abort_code;
        assume {:print "$track_abort(2,0):", $t2} $t2 == $t2;
        goto L2;
    }

    // $t3 := get_field<BasicCoin::Balance<#0>>.coin($t1) at ./sources/BasicCoin.move:35:9+44
    $t3 := $coin#$cafe_BasicCoin_Balance'#0'($t1);

    // $t4 := get_field<BasicCoin::Coin<#0>>.value($t3) at ./sources/BasicCoin.move:35:9+50
    $t4 := $value#$cafe_BasicCoin_Coin'#0'($t3);

    // trace_return[0]($t4) at ./sources/BasicCoin.move:35:9+50
    assume {:print "$track_return(2,0,0):", $t4} $t4 == $t4;

    // label L1 at ./sources/BasicCoin.move:36:5+1
    assume {:print "$at(23,1459,1460)"} true;
L1:

    // return $t4 at ./sources/BasicCoin.move:36:5+1
    $ret0 := $t4;
    return;

    // label L2 at ./sources/BasicCoin.move:36:5+1
L2:

    // abort($t2) at ./sources/BasicCoin.move:36:5+1
    $abort_code := $t2;
    $abort_flag := true;
    return;

}

// fun BasicCoin::balance_of [verification] at ./sources/BasicCoin.move:34:5+136
procedure {:timeLimit 40} $cafe_BasicCoin_balance_of$verify(_$t0: int) returns ($ret0: int)
{
    // declare local variables
    var $t1: $cafe_BasicCoin_Balance'#0';
    var $t2: int;
    var $t3: $cafe_BasicCoin_Coin'#0';
    var $t4: int;
    var $t0: int;
    var $temp_0'address': int;
    var $temp_0'u64': int;
    var $cafe_BasicCoin_Balance'#0'_$memory#0: $Memory $cafe_BasicCoin_Balance'#0';
    $t0 := _$t0;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:34:5+1
    assume {:print "$at(23,1324,1325)"} true;
    assume $IsValid'address'($t0);

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:34:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // @0 := save_mem(BasicCoin::Balance<#0>) at ./sources/BasicCoin.move:34:5+1
    $cafe_BasicCoin_Balance'#0'_$memory#0 := $cafe_BasicCoin_Balance'#0'_$memory;

    // trace_local[owner]($t0) at ./sources/BasicCoin.move:34:5+1
    assume {:print "$track_local(2,0,0):", $t0} $t0 == $t0;

    // $t1 := get_global<BasicCoin::Balance<#0>>($t0) on_abort goto L2 with $t2 at ./sources/BasicCoin.move:35:9+13
    assume {:print "$at(23,1404,1417)"} true;
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t1 := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0);
    }
    if ($abort_flag) {
        assume {:print "$at(23,1404,1417)"} true;
        $t2 := $abort_code;
        assume {:print "$track_abort(2,0):", $t2} $t2 == $t2;
        goto L2;
    }

    // $t3 := get_field<BasicCoin::Balance<#0>>.coin($t1) at ./sources/BasicCoin.move:35:9+44
    $t3 := $coin#$cafe_BasicCoin_Balance'#0'($t1);

    // $t4 := get_field<BasicCoin::Coin<#0>>.value($t3) at ./sources/BasicCoin.move:35:9+50
    $t4 := $value#$cafe_BasicCoin_Coin'#0'($t3);

    // trace_return[0]($t4) at ./sources/BasicCoin.move:35:9+50
    assume {:print "$track_return(2,0,0):", $t4} $t4 == $t4;

    // label L1 at ./sources/BasicCoin.move:36:5+1
    assume {:print "$at(23,1459,1460)"} true;
L1:

    // assert Not(Not(exists[@0]<BasicCoin::Balance<#0>>($t0))) at ./sources/BasicCoin.move:40:9+44
    assume {:print "$at(23,1528,1572)"} true;
    assert {:msg "assert_failed(23,1528,1572): function does not abort under this condition"}
      !!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory#0, $t0);

    // return $t4 at ./sources/BasicCoin.move:40:9+44
    $ret0 := $t4;
    return;

    // label L2 at ./sources/BasicCoin.move:36:5+1
    assume {:print "$at(23,1459,1460)"} true;
L2:

    // assert Not(exists[@0]<BasicCoin::Balance<#0>>($t0)) at ./sources/BasicCoin.move:38:5+112
    assume {:print "$at(23,1466,1578)"} true;
    assert {:msg "assert_failed(23,1466,1578): abort not covered by any of the `aborts_if` clauses"}
      !$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory#0, $t0);

    // abort($t2) at ./sources/BasicCoin.move:38:5+112
    $abort_code := $t2;
    $abort_flag := true;
    return;

}

// fun BasicCoin::deposit<#0> [baseline] at ./sources/BasicCoin.move:58:5+295
procedure {:inline 1} $cafe_BasicCoin_deposit'#0'(_$t0: int, _$t1: $cafe_BasicCoin_Coin'#0') returns ()
{
    // declare local variables
    var $t2: int;
    var $t3: $Mutation (int);
    var $t4: int;
    var $t5: int;
    var $t6: int;
    var $t7: $Mutation ($cafe_BasicCoin_Balance'#0');
    var $t8: $Mutation ($cafe_BasicCoin_Coin'#0');
    var $t9: $Mutation (int);
    var $t10: int;
    var $t11: int;
    var $t0: int;
    var $t1: $cafe_BasicCoin_Coin'#0';
    var $temp_0'$cafe_BasicCoin_Coin'#0'': $cafe_BasicCoin_Coin'#0';
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    assume IsEmptyVec(p#$Mutation($t3));
    assume IsEmptyVec(p#$Mutation($t7));
    assume IsEmptyVec(p#$Mutation($t8));
    assume IsEmptyVec(p#$Mutation($t9));

    // bytecode translation starts here
    // trace_local[addr]($t0) at ./sources/BasicCoin.move:58:5+1
    assume {:print "$at(23,2388,2389)"} true;
    assume {:print "$track_local(2,1,0):", $t0} $t0 == $t0;

    // trace_local[check]($t1) at ./sources/BasicCoin.move:58:5+1
    assume {:print "$track_local(2,1,1):", $t1} $t1 == $t1;

    // $t5 := BasicCoin::balance_of<#0>($t0) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:59:23+26
    assume {:print "$at(23,2488,2514)"} true;
    call $t5 := $cafe_BasicCoin_balance_of'#0'($t0);
    if ($abort_flag) {
        assume {:print "$at(23,2488,2514)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // trace_local[balance]($t5) at ./sources/BasicCoin.move:59:13+7
    assume {:print "$track_local(2,1,2):", $t5} $t5 == $t5;

    // $t7 := borrow_global<BasicCoin::Balance<#0>>($t0) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:60:32+17
    assume {:print "$at(23,2547,2564)"} true;
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t7 := $Mutation($Global($t0), EmptyVec(), $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0));
    }
    if ($abort_flag) {
        assume {:print "$at(23,2547,2564)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // $t8 := borrow_field<BasicCoin::Balance<#0>>.coin($t7) at ./sources/BasicCoin.move:60:32+47
    $t8 := $ChildMutation($t7, 0, $coin#$cafe_BasicCoin_Balance'#0'($Dereference($t7)));

    // $t9 := borrow_field<BasicCoin::Coin<#0>>.value($t8) at ./sources/BasicCoin.move:60:27+58
    $t9 := $ChildMutation($t8, 0, $value#$cafe_BasicCoin_Coin'#0'($Dereference($t8)));

    // trace_local[balance_ref]($t9) at ./sources/BasicCoin.move:60:13+11
    $temp_0'u64' := $Dereference($t9);
    assume {:print "$track_local(2,1,3):", $temp_0'u64'} $temp_0'u64' == $temp_0'u64';

    // $t10 := unpack BasicCoin::Coin<#0>($t1) at ./sources/BasicCoin.move:61:13+14
    assume {:print "$at(23,2614,2628)"} true;
    $t10 := $value#$cafe_BasicCoin_Coin'#0'($t1);

    // trace_local[value]($t10) at ./sources/BasicCoin.move:61:20+5
    assume {:print "$track_local(2,1,4):", $t10} $t10 == $t10;

    // $t11 := +($t5, $t10) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:62:32+1
    assume {:print "$at(23,2669,2670)"} true;
    call $t11 := $AddU64($t5, $t10);
    if ($abort_flag) {
        assume {:print "$at(23,2669,2670)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // write_ref($t9, $t11) at ./sources/BasicCoin.move:62:9+30
    $t9 := $UpdateMutation($t9, $t11);

    // write_back[Reference($t8).value (u64)]($t9) at ./sources/BasicCoin.move:62:9+30
    $t8 := $UpdateMutation($t8, $Update'$cafe_BasicCoin_Coin'#0''_value($Dereference($t8), $Dereference($t9)));

    // write_back[Reference($t7).coin (BasicCoin::Coin<#0>)]($t8) at ./sources/BasicCoin.move:62:9+30
    $t7 := $UpdateMutation($t7, $Update'$cafe_BasicCoin_Balance'#0''_coin($Dereference($t7), $Dereference($t8)));

    // write_back[BasicCoin::Balance<#0>@]($t7) at ./sources/BasicCoin.move:62:9+30
    $cafe_BasicCoin_Balance'#0'_$memory := $ResourceUpdate($cafe_BasicCoin_Balance'#0'_$memory, $GlobalLocationAddress($t7),
        $Dereference($t7));

    // label L1 at ./sources/BasicCoin.move:63:5+1
    assume {:print "$at(23,2682,2683)"} true;
L1:

    // return () at ./sources/BasicCoin.move:63:5+1
    return;

    // label L2 at ./sources/BasicCoin.move:63:5+1
L2:

    // abort($t6) at ./sources/BasicCoin.move:63:5+1
    $abort_code := $t6;
    $abort_flag := true;
    return;

}

// fun BasicCoin::deposit [verification] at ./sources/BasicCoin.move:58:5+295
procedure {:timeLimit 40} $cafe_BasicCoin_deposit$verify(_$t0: int, _$t1: $cafe_BasicCoin_Coin'#0') returns ()
{
    // declare local variables
    var $t2: int;
    var $t3: $Mutation (int);
    var $t4: int;
    var $t5: int;
    var $t6: int;
    var $t7: $Mutation ($cafe_BasicCoin_Balance'#0');
    var $t8: $Mutation ($cafe_BasicCoin_Coin'#0');
    var $t9: $Mutation (int);
    var $t10: int;
    var $t11: int;
    var $t0: int;
    var $t1: $cafe_BasicCoin_Coin'#0';
    var $temp_0'$cafe_BasicCoin_Coin'#0'': $cafe_BasicCoin_Coin'#0';
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    assume IsEmptyVec(p#$Mutation($t3));
    assume IsEmptyVec(p#$Mutation($t7));
    assume IsEmptyVec(p#$Mutation($t8));
    assume IsEmptyVec(p#$Mutation($t9));

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:58:5+1
    assume {:print "$at(23,2388,2389)"} true;
    assume $IsValid'address'($t0);

    // assume WellFormed($t1) at ./sources/BasicCoin.move:58:5+1
    assume $IsValid'$cafe_BasicCoin_Coin'#0''($t1);

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:58:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // trace_local[addr]($t0) at ./sources/BasicCoin.move:58:5+1
    assume {:print "$track_local(2,1,0):", $t0} $t0 == $t0;

    // trace_local[check]($t1) at ./sources/BasicCoin.move:58:5+1
    assume {:print "$track_local(2,1,1):", $t1} $t1 == $t1;

    // $t5 := BasicCoin::balance_of<#0>($t0) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:59:23+26
    assume {:print "$at(23,2488,2514)"} true;
    call $t5 := $cafe_BasicCoin_balance_of'#0'($t0);
    if ($abort_flag) {
        assume {:print "$at(23,2488,2514)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // trace_local[balance]($t5) at ./sources/BasicCoin.move:59:13+7
    assume {:print "$track_local(2,1,2):", $t5} $t5 == $t5;

    // $t7 := borrow_global<BasicCoin::Balance<#0>>($t0) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:60:32+17
    assume {:print "$at(23,2547,2564)"} true;
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t7 := $Mutation($Global($t0), EmptyVec(), $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0));
    }
    if ($abort_flag) {
        assume {:print "$at(23,2547,2564)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // $t8 := borrow_field<BasicCoin::Balance<#0>>.coin($t7) at ./sources/BasicCoin.move:60:32+47
    $t8 := $ChildMutation($t7, 0, $coin#$cafe_BasicCoin_Balance'#0'($Dereference($t7)));

    // $t9 := borrow_field<BasicCoin::Coin<#0>>.value($t8) at ./sources/BasicCoin.move:60:27+58
    $t9 := $ChildMutation($t8, 0, $value#$cafe_BasicCoin_Coin'#0'($Dereference($t8)));

    // trace_local[balance_ref]($t9) at ./sources/BasicCoin.move:60:13+11
    $temp_0'u64' := $Dereference($t9);
    assume {:print "$track_local(2,1,3):", $temp_0'u64'} $temp_0'u64' == $temp_0'u64';

    // $t10 := unpack BasicCoin::Coin<#0>($t1) at ./sources/BasicCoin.move:61:13+14
    assume {:print "$at(23,2614,2628)"} true;
    $t10 := $value#$cafe_BasicCoin_Coin'#0'($t1);

    // trace_local[value]($t10) at ./sources/BasicCoin.move:61:20+5
    assume {:print "$track_local(2,1,4):", $t10} $t10 == $t10;

    // $t11 := +($t5, $t10) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:62:32+1
    assume {:print "$at(23,2669,2670)"} true;
    call $t11 := $AddU64($t5, $t10);
    if ($abort_flag) {
        assume {:print "$at(23,2669,2670)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,1):", $t6} $t6 == $t6;
        goto L2;
    }

    // write_ref($t9, $t11) at ./sources/BasicCoin.move:62:9+30
    $t9 := $UpdateMutation($t9, $t11);

    // write_back[Reference($t8).value (u64)]($t9) at ./sources/BasicCoin.move:62:9+30
    $t8 := $UpdateMutation($t8, $Update'$cafe_BasicCoin_Coin'#0''_value($Dereference($t8), $Dereference($t9)));

    // write_back[Reference($t7).coin (BasicCoin::Coin<#0>)]($t8) at ./sources/BasicCoin.move:62:9+30
    $t7 := $UpdateMutation($t7, $Update'$cafe_BasicCoin_Balance'#0''_coin($Dereference($t7), $Dereference($t8)));

    // write_back[BasicCoin::Balance<#0>@]($t7) at ./sources/BasicCoin.move:62:9+30
    $cafe_BasicCoin_Balance'#0'_$memory := $ResourceUpdate($cafe_BasicCoin_Balance'#0'_$memory, $GlobalLocationAddress($t7),
        $Dereference($t7));

    // label L1 at ./sources/BasicCoin.move:63:5+1
    assume {:print "$at(23,2682,2683)"} true;
L1:

    // return () at ./sources/BasicCoin.move:63:5+1
    return;

    // label L2 at ./sources/BasicCoin.move:63:5+1
L2:

    // abort($t6) at ./sources/BasicCoin.move:63:5+1
    $abort_code := $t6;
    $abort_flag := true;
    return;

}

// fun BasicCoin::mint [verification] at ./sources/BasicCoin.move:29:5+244
procedure {:timeLimit 40} $cafe_BasicCoin_mint$verify(_$t0: int, _$t1: int, _$t2: #0) returns ()
{
    // declare local variables
    var $t3: $cafe_BasicCoin_Coin'#0';
    var $t4: int;
    var $t0: int;
    var $t1: int;
    var $t2: #0;
    var $temp_0'#0': #0;
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    $t2 := _$t2;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:29:5+1
    assume {:print "$at(23,1074,1075)"} true;
    assume $IsValid'address'($t0);

    // assume WellFormed($t1) at ./sources/BasicCoin.move:29:5+1
    assume $IsValid'u64'($t1);

    // assume WellFormed($t2) at ./sources/BasicCoin.move:29:5+1
    assume $IsValid'#0'($t2);

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:29:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // trace_local[mint_addr]($t0) at ./sources/BasicCoin.move:29:5+1
    assume {:print "$track_local(2,2,0):", $t0} $t0 == $t0;

    // trace_local[amount]($t1) at ./sources/BasicCoin.move:29:5+1
    assume {:print "$track_local(2,2,1):", $t1} $t1 == $t1;

    // trace_local[_witness]($t2) at ./sources/BasicCoin.move:29:5+1
    assume {:print "$track_local(2,2,2):", $t2} $t2 == $t2;

    // $t3 := pack BasicCoin::Coin<#0>($t1) at ./sources/BasicCoin.move:31:28+32
    assume {:print "$at(23,1278,1310)"} true;
    $t3 := $cafe_BasicCoin_Coin'#0'($t1);

    // BasicCoin::deposit<#0>($t0, $t3) on_abort goto L2 with $t4 at ./sources/BasicCoin.move:31:9+52
    call $cafe_BasicCoin_deposit'#0'($t0, $t3);
    if ($abort_flag) {
        assume {:print "$at(23,1259,1311)"} true;
        $t4 := $abort_code;
        assume {:print "$track_abort(2,2):", $t4} $t4 == $t4;
        goto L2;
    }

    // label L1 at ./sources/BasicCoin.move:32:5+1
    assume {:print "$at(23,1317,1318)"} true;
L1:

    // return () at ./sources/BasicCoin.move:32:5+1
    return;

    // label L2 at ./sources/BasicCoin.move:32:5+1
L2:

    // abort($t4) at ./sources/BasicCoin.move:32:5+1
    $abort_code := $t4;
    $abort_flag := true;
    return;

}

// fun BasicCoin::publish_balance [verification] at ./sources/BasicCoin.move:21:5+306
procedure {:timeLimit 40} $cafe_BasicCoin_publish_balance$verify(_$t0: $signer) returns ()
{
    // declare local variables
    var $t1: $cafe_BasicCoin_Coin'#0';
    var $t2: int;
    var $t3: int;
    var $t4: bool;
    var $t5: bool;
    var $t6: int;
    var $t7: int;
    var $t8: int;
    var $t9: $cafe_BasicCoin_Coin'#0';
    var $t10: $cafe_BasicCoin_Balance'#0';
    var $t0: $signer;
    var $temp_0'signer': $signer;
    $t0 := _$t0;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:21:5+1
    assume {:print "$at(23,591,592)"} true;
    assume $IsValid'signer'($t0) && $1_Signer_is_txn_signer($t0) && $1_Signer_is_txn_signer_addr($addr#$signer($t0));

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:21:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // trace_local[account]($t0) at ./sources/BasicCoin.move:21:5+1
    assume {:print "$track_local(2,3,0):", $t0} $t0 == $t0;

    // $t2 := Signer::address_of($t0) on_abort goto L3 with $t3 at ./sources/BasicCoin.move:23:44+27
    assume {:print "$at(23,745,772)"} true;
    call $t2 := $1_Signer_address_of($t0);
    if ($abort_flag) {
        assume {:print "$at(23,745,772)"} true;
        $t3 := $abort_code;
        assume {:print "$track_abort(2,3):", $t3} $t3 == $t3;
        goto L3;
    }

    // $t4 := exists<BasicCoin::Balance<#0>>($t2) at ./sources/BasicCoin.move:23:18+6
    $t4 := $ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t2);

    // $t5 := !($t4) at ./sources/BasicCoin.move:23:17+1
    call $t5 := $Not($t4);

    // if ($t5) goto L0 else goto L1 at ./sources/BasicCoin.move:23:9+113
    if ($t5) { goto L0; } else { goto L1; }

    // label L1 at ./sources/BasicCoin.move:23:9+113
L1:

    // destroy($t0) at ./sources/BasicCoin.move:23:9+113

    // $t6 := 2 at ./sources/BasicCoin.move:23:100+20
    $t6 := 2;
    assume $IsValid'u64'($t6);

    // $t7 := opaque begin: Errors::already_published($t6) at ./sources/BasicCoin.move:23:74+47

    // assume WellFormed($t7) at ./sources/BasicCoin.move:23:74+47
    assume $IsValid'u64'($t7);

    // assume Eq<u64>($t7, 6) at ./sources/BasicCoin.move:23:74+47
    assume $IsEqual'u64'($t7, 6);

    // $t7 := opaque end: Errors::already_published($t6) at ./sources/BasicCoin.move:23:74+47

    // trace_abort($t7) at ./sources/BasicCoin.move:23:9+113
    assume {:print "$at(23,710,823)"} true;
    assume {:print "$track_abort(2,3):", $t7} $t7 == $t7;

    // $t3 := move($t7) at ./sources/BasicCoin.move:23:9+113
    $t3 := $t7;

    // goto L3 at ./sources/BasicCoin.move:23:9+113
    goto L3;

    // label L0 at ./sources/BasicCoin.move:24:17+7
    assume {:print "$at(23,841,848)"} true;
L0:

    // $t8 := 0 at ./sources/BasicCoin.move:22:50+1
    assume {:print "$at(23,697,698)"} true;
    $t8 := 0;
    assume $IsValid'u64'($t8);

    // $t9 := pack BasicCoin::Coin<#0>($t8) at ./sources/BasicCoin.move:22:26+27
    $t9 := $cafe_BasicCoin_Coin'#0'($t8);

    // $t10 := pack BasicCoin::Balance<#0>($t9) at ./sources/BasicCoin.move:24:26+39
    assume {:print "$at(23,850,889)"} true;
    $t10 := $cafe_BasicCoin_Balance'#0'($t9);

    // move_to<BasicCoin::Balance<#0>>($t10, $t0) on_abort goto L3 with $t3 at ./sources/BasicCoin.move:24:9+7
    if ($ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $addr#$signer($t0))) {
        call $ExecFailureAbort();
    } else {
        $cafe_BasicCoin_Balance'#0'_$memory := $ResourceUpdate($cafe_BasicCoin_Balance'#0'_$memory, $addr#$signer($t0), $t10);
    }
    if ($abort_flag) {
        assume {:print "$at(23,833,840)"} true;
        $t3 := $abort_code;
        assume {:print "$track_abort(2,3):", $t3} $t3 == $t3;
        goto L3;
    }

    // label L2 at ./sources/BasicCoin.move:25:5+1
    assume {:print "$at(23,896,897)"} true;
L2:

    // return () at ./sources/BasicCoin.move:25:5+1
    return;

    // label L3 at ./sources/BasicCoin.move:25:5+1
L3:

    // abort($t3) at ./sources/BasicCoin.move:25:5+1
    $abort_code := $t3;
    $abort_flag := true;
    return;

}

// fun BasicCoin::transfer [verification] at ./sources/BasicCoin.move:45:5+233
procedure {:timeLimit 40} $cafe_BasicCoin_transfer$verify(_$t0: $signer, _$t1: int, _$t2: int, _$t3: #0) returns ()
{
    // declare local variables
    var $t4: $cafe_BasicCoin_Coin'#0';
    var $t5: int;
    var $t6: int;
    var $t7: $cafe_BasicCoin_Coin'#0';
    var $t0: $signer;
    var $t1: int;
    var $t2: int;
    var $t3: #0;
    var $temp_0'#0': #0;
    var $temp_0'$cafe_BasicCoin_Coin'#0'': $cafe_BasicCoin_Coin'#0';
    var $temp_0'address': int;
    var $temp_0'signer': $signer;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    $t2 := _$t2;
    $t3 := _$t3;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:45:5+1
    assume {:print "$at(23,1774,1775)"} true;
    assume $IsValid'signer'($t0) && $1_Signer_is_txn_signer($t0) && $1_Signer_is_txn_signer_addr($addr#$signer($t0));

    // assume WellFormed($t1) at ./sources/BasicCoin.move:45:5+1
    assume $IsValid'address'($t1);

    // assume WellFormed($t2) at ./sources/BasicCoin.move:45:5+1
    assume $IsValid'u64'($t2);

    // assume WellFormed($t3) at ./sources/BasicCoin.move:45:5+1
    assume $IsValid'#0'($t3);

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:45:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // trace_local[from]($t0) at ./sources/BasicCoin.move:45:5+1
    assume {:print "$track_local(2,4,0):", $t0} $t0 == $t0;

    // trace_local[to]($t1) at ./sources/BasicCoin.move:45:5+1
    assume {:print "$track_local(2,4,1):", $t1} $t1 == $t1;

    // trace_local[amount]($t2) at ./sources/BasicCoin.move:45:5+1
    assume {:print "$track_local(2,4,2):", $t2} $t2 == $t2;

    // trace_local[_witness]($t3) at ./sources/BasicCoin.move:45:5+1
    assume {:print "$track_local(2,4,3):", $t3} $t3 == $t3;

    // $t5 := Signer::address_of($t0) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:46:40+24
    assume {:print "$at(23,1929,1953)"} true;
    call $t5 := $1_Signer_address_of($t0);
    if ($abort_flag) {
        assume {:print "$at(23,1929,1953)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,4):", $t6} $t6 == $t6;
        goto L2;
    }

    // $t7 := BasicCoin::withdraw<#0>($t5, $t2) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:46:21+52
    call $t7 := $cafe_BasicCoin_withdraw'#0'($t5, $t2);
    if ($abort_flag) {
        assume {:print "$at(23,1910,1962)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,4):", $t6} $t6 == $t6;
        goto L2;
    }

    // trace_local[check]($t7) at ./sources/BasicCoin.move:46:13+5
    assume {:print "$track_local(2,4,4):", $t7} $t7 == $t7;

    // BasicCoin::deposit<#0>($t1, $t7) on_abort goto L2 with $t6 at ./sources/BasicCoin.move:47:9+28
    assume {:print "$at(23,1972,2000)"} true;
    call $cafe_BasicCoin_deposit'#0'($t1, $t7);
    if ($abort_flag) {
        assume {:print "$at(23,1972,2000)"} true;
        $t6 := $abort_code;
        assume {:print "$track_abort(2,4):", $t6} $t6 == $t6;
        goto L2;
    }

    // label L1 at ./sources/BasicCoin.move:48:5+1
    assume {:print "$at(23,2006,2007)"} true;
L1:

    // return () at ./sources/BasicCoin.move:48:5+1
    return;

    // label L2 at ./sources/BasicCoin.move:48:5+1
L2:

    // abort($t6) at ./sources/BasicCoin.move:48:5+1
    $abort_code := $t6;
    $abort_flag := true;
    return;

}

// fun BasicCoin::withdraw<#0> [baseline] at ./sources/BasicCoin.move:50:5+369
procedure {:inline 1} $cafe_BasicCoin_withdraw'#0'(_$t0: int, _$t1: int) returns ($ret0: $cafe_BasicCoin_Coin'#0')
{
    // declare local variables
    var $t2: int;
    var $t3: $Mutation (int);
    var $t4: int;
    var $t5: int;
    var $t6: bool;
    var $t7: int;
    var $t8: $Mutation ($cafe_BasicCoin_Balance'#0');
    var $t9: $Mutation ($cafe_BasicCoin_Coin'#0');
    var $t10: $Mutation (int);
    var $t11: int;
    var $t12: $cafe_BasicCoin_Coin'#0';
    var $t0: int;
    var $t1: int;
    var $temp_0'$cafe_BasicCoin_Coin'#0'': $cafe_BasicCoin_Coin'#0';
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    assume IsEmptyVec(p#$Mutation($t3));
    assume IsEmptyVec(p#$Mutation($t8));
    assume IsEmptyVec(p#$Mutation($t9));
    assume IsEmptyVec(p#$Mutation($t10));

    // bytecode translation starts here
    // trace_local[addr]($t0) at ./sources/BasicCoin.move:50:5+1
    assume {:print "$at(23,2013,2014)"} true;
    assume {:print "$track_local(2,5,0):", $t0} $t0 == $t0;

    // trace_local[amount]($t1) at ./sources/BasicCoin.move:50:5+1
    assume {:print "$track_local(2,5,1):", $t1} $t1 == $t1;

    // $t4 := BasicCoin::balance_of<#0>($t0) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:51:23+26
    assume {:print "$at(23,2122,2148)"} true;
    call $t4 := $cafe_BasicCoin_balance_of'#0'($t0);
    if ($abort_flag) {
        assume {:print "$at(23,2122,2148)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // trace_local[balance]($t4) at ./sources/BasicCoin.move:51:13+7
    assume {:print "$track_local(2,5,2):", $t4} $t4 == $t4;

    // $t6 := >=($t4, $t1) at ./sources/BasicCoin.move:52:25+2
    assume {:print "$at(23,2174,2176)"} true;
    call $t6 := $Ge($t4, $t1);

    // if ($t6) goto L0 else goto L1 at ./sources/BasicCoin.move:52:9+49
    if ($t6) { goto L0; } else { goto L1; }

    // label L1 at ./sources/BasicCoin.move:52:36+21
L1:

    // $t7 := 1 at ./sources/BasicCoin.move:52:36+21
    $t7 := 1;
    assume $IsValid'u64'($t7);

    // trace_abort($t7) at ./sources/BasicCoin.move:52:9+49
    assume {:print "$at(23,2158,2207)"} true;
    assume {:print "$track_abort(2,5):", $t7} $t7 == $t7;

    // $t5 := move($t7) at ./sources/BasicCoin.move:52:9+49
    $t5 := $t7;

    // goto L3 at ./sources/BasicCoin.move:52:9+49
    goto L3;

    // label L0 at ./sources/BasicCoin.move:53:69+4
    assume {:print "$at(23,2277,2281)"} true;
L0:

    // $t8 := borrow_global<BasicCoin::Balance<#0>>($t0) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:53:32+17
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t8 := $Mutation($Global($t0), EmptyVec(), $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0));
    }
    if ($abort_flag) {
        assume {:print "$at(23,2240,2257)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // $t9 := borrow_field<BasicCoin::Balance<#0>>.coin($t8) at ./sources/BasicCoin.move:53:32+47
    $t9 := $ChildMutation($t8, 0, $coin#$cafe_BasicCoin_Balance'#0'($Dereference($t8)));

    // $t10 := borrow_field<BasicCoin::Coin<#0>>.value($t9) at ./sources/BasicCoin.move:53:27+58
    $t10 := $ChildMutation($t9, 0, $value#$cafe_BasicCoin_Coin'#0'($Dereference($t9)));

    // trace_local[balance_ref]($t10) at ./sources/BasicCoin.move:53:13+11
    $temp_0'u64' := $Dereference($t10);
    assume {:print "$track_local(2,5,3):", $temp_0'u64'} $temp_0'u64' == $temp_0'u64';

    // $t11 := -($t4, $t1) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:54:32+1
    assume {:print "$at(23,2326,2327)"} true;
    call $t11 := $Sub($t4, $t1);
    if ($abort_flag) {
        assume {:print "$at(23,2326,2327)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // write_ref($t10, $t11) at ./sources/BasicCoin.move:54:9+31
    $t10 := $UpdateMutation($t10, $t11);

    // write_back[Reference($t9).value (u64)]($t10) at ./sources/BasicCoin.move:54:9+31
    $t9 := $UpdateMutation($t9, $Update'$cafe_BasicCoin_Coin'#0''_value($Dereference($t9), $Dereference($t10)));

    // write_back[Reference($t8).coin (BasicCoin::Coin<#0>)]($t9) at ./sources/BasicCoin.move:54:9+31
    $t8 := $UpdateMutation($t8, $Update'$cafe_BasicCoin_Balance'#0''_coin($Dereference($t8), $Dereference($t9)));

    // write_back[BasicCoin::Balance<#0>@]($t8) at ./sources/BasicCoin.move:54:9+31
    $cafe_BasicCoin_Balance'#0'_$memory := $ResourceUpdate($cafe_BasicCoin_Balance'#0'_$memory, $GlobalLocationAddress($t8),
        $Dereference($t8));

    // $t12 := pack BasicCoin::Coin<#0>($t1) at ./sources/BasicCoin.move:55:9+32
    assume {:print "$at(23,2344,2376)"} true;
    $t12 := $cafe_BasicCoin_Coin'#0'($t1);

    // trace_return[0]($t12) at ./sources/BasicCoin.move:55:9+32
    assume {:print "$track_return(2,5,0):", $t12} $t12 == $t12;

    // label L2 at ./sources/BasicCoin.move:56:5+1
    assume {:print "$at(23,2381,2382)"} true;
L2:

    // return $t12 at ./sources/BasicCoin.move:56:5+1
    $ret0 := $t12;
    return;

    // label L3 at ./sources/BasicCoin.move:56:5+1
L3:

    // abort($t5) at ./sources/BasicCoin.move:56:5+1
    $abort_code := $t5;
    $abort_flag := true;
    return;

}

// fun BasicCoin::withdraw [verification] at ./sources/BasicCoin.move:50:5+369
procedure {:timeLimit 40} $cafe_BasicCoin_withdraw$verify(_$t0: int, _$t1: int) returns ($ret0: $cafe_BasicCoin_Coin'#0')
{
    // declare local variables
    var $t2: int;
    var $t3: $Mutation (int);
    var $t4: int;
    var $t5: int;
    var $t6: bool;
    var $t7: int;
    var $t8: $Mutation ($cafe_BasicCoin_Balance'#0');
    var $t9: $Mutation ($cafe_BasicCoin_Coin'#0');
    var $t10: $Mutation (int);
    var $t11: int;
    var $t12: $cafe_BasicCoin_Coin'#0';
    var $t0: int;
    var $t1: int;
    var $temp_0'$cafe_BasicCoin_Coin'#0'': $cafe_BasicCoin_Coin'#0';
    var $temp_0'address': int;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    assume IsEmptyVec(p#$Mutation($t3));
    assume IsEmptyVec(p#$Mutation($t8));
    assume IsEmptyVec(p#$Mutation($t9));
    assume IsEmptyVec(p#$Mutation($t10));

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/BasicCoin.move:50:5+1
    assume {:print "$at(23,2013,2014)"} true;
    assume $IsValid'address'($t0);

    // assume WellFormed($t1) at ./sources/BasicCoin.move:50:5+1
    assume $IsValid'u64'($t1);

    // assume forall $rsc: ResourceDomain<BasicCoin::Balance<#0>>(): WellFormed($rsc) at ./sources/BasicCoin.move:50:5+1
    assume (forall $a_0: int :: {$ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0)}(var $rsc := $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $a_0);
    ($IsValid'$cafe_BasicCoin_Balance'#0''($rsc))));

    // trace_local[addr]($t0) at ./sources/BasicCoin.move:50:5+1
    assume {:print "$track_local(2,5,0):", $t0} $t0 == $t0;

    // trace_local[amount]($t1) at ./sources/BasicCoin.move:50:5+1
    assume {:print "$track_local(2,5,1):", $t1} $t1 == $t1;

    // $t4 := BasicCoin::balance_of<#0>($t0) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:51:23+26
    assume {:print "$at(23,2122,2148)"} true;
    call $t4 := $cafe_BasicCoin_balance_of'#0'($t0);
    if ($abort_flag) {
        assume {:print "$at(23,2122,2148)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // trace_local[balance]($t4) at ./sources/BasicCoin.move:51:13+7
    assume {:print "$track_local(2,5,2):", $t4} $t4 == $t4;

    // $t6 := >=($t4, $t1) at ./sources/BasicCoin.move:52:25+2
    assume {:print "$at(23,2174,2176)"} true;
    call $t6 := $Ge($t4, $t1);

    // if ($t6) goto L0 else goto L1 at ./sources/BasicCoin.move:52:9+49
    if ($t6) { goto L0; } else { goto L1; }

    // label L1 at ./sources/BasicCoin.move:52:36+21
L1:

    // $t7 := 1 at ./sources/BasicCoin.move:52:36+21
    $t7 := 1;
    assume $IsValid'u64'($t7);

    // trace_abort($t7) at ./sources/BasicCoin.move:52:9+49
    assume {:print "$at(23,2158,2207)"} true;
    assume {:print "$track_abort(2,5):", $t7} $t7 == $t7;

    // $t5 := move($t7) at ./sources/BasicCoin.move:52:9+49
    $t5 := $t7;

    // goto L3 at ./sources/BasicCoin.move:52:9+49
    goto L3;

    // label L0 at ./sources/BasicCoin.move:53:69+4
    assume {:print "$at(23,2277,2281)"} true;
L0:

    // $t8 := borrow_global<BasicCoin::Balance<#0>>($t0) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:53:32+17
    if (!$ResourceExists($cafe_BasicCoin_Balance'#0'_$memory, $t0)) {
        call $ExecFailureAbort();
    } else {
        $t8 := $Mutation($Global($t0), EmptyVec(), $ResourceValue($cafe_BasicCoin_Balance'#0'_$memory, $t0));
    }
    if ($abort_flag) {
        assume {:print "$at(23,2240,2257)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // $t9 := borrow_field<BasicCoin::Balance<#0>>.coin($t8) at ./sources/BasicCoin.move:53:32+47
    $t9 := $ChildMutation($t8, 0, $coin#$cafe_BasicCoin_Balance'#0'($Dereference($t8)));

    // $t10 := borrow_field<BasicCoin::Coin<#0>>.value($t9) at ./sources/BasicCoin.move:53:27+58
    $t10 := $ChildMutation($t9, 0, $value#$cafe_BasicCoin_Coin'#0'($Dereference($t9)));

    // trace_local[balance_ref]($t10) at ./sources/BasicCoin.move:53:13+11
    $temp_0'u64' := $Dereference($t10);
    assume {:print "$track_local(2,5,3):", $temp_0'u64'} $temp_0'u64' == $temp_0'u64';

    // $t11 := -($t4, $t1) on_abort goto L3 with $t5 at ./sources/BasicCoin.move:54:32+1
    assume {:print "$at(23,2326,2327)"} true;
    call $t11 := $Sub($t4, $t1);
    if ($abort_flag) {
        assume {:print "$at(23,2326,2327)"} true;
        $t5 := $abort_code;
        assume {:print "$track_abort(2,5):", $t5} $t5 == $t5;
        goto L3;
    }

    // write_ref($t10, $t11) at ./sources/BasicCoin.move:54:9+31
    $t10 := $UpdateMutation($t10, $t11);

    // write_back[Reference($t9).value (u64)]($t10) at ./sources/BasicCoin.move:54:9+31
    $t9 := $UpdateMutation($t9, $Update'$cafe_BasicCoin_Coin'#0''_value($Dereference($t9), $Dereference($t10)));

    // write_back[Reference($t8).coin (BasicCoin::Coin<#0>)]($t9) at ./sources/BasicCoin.move:54:9+31
    $t8 := $UpdateMutation($t8, $Update'$cafe_BasicCoin_Balance'#0''_coin($Dereference($t8), $Dereference($t9)));

    // write_back[BasicCoin::Balance<#0>@]($t8) at ./sources/BasicCoin.move:54:9+31
    $cafe_BasicCoin_Balance'#0'_$memory := $ResourceUpdate($cafe_BasicCoin_Balance'#0'_$memory, $GlobalLocationAddress($t8),
        $Dereference($t8));

    // $t12 := pack BasicCoin::Coin<#0>($t1) at ./sources/BasicCoin.move:55:9+32
    assume {:print "$at(23,2344,2376)"} true;
    $t12 := $cafe_BasicCoin_Coin'#0'($t1);

    // trace_return[0]($t12) at ./sources/BasicCoin.move:55:9+32
    assume {:print "$track_return(2,5,0):", $t12} $t12 == $t12;

    // label L2 at ./sources/BasicCoin.move:56:5+1
    assume {:print "$at(23,2381,2382)"} true;
L2:

    // return $t12 at ./sources/BasicCoin.move:56:5+1
    $ret0 := $t12;
    return;

    // label L3 at ./sources/BasicCoin.move:56:5+1
L3:

    // abort($t5) at ./sources/BasicCoin.move:56:5+1
    $abort_code := $t5;
    $abort_flag := true;
    return;

}
