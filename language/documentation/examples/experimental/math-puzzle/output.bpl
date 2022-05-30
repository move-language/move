
// ** Expanded prelude

// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
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
// Copyright (c) The Move Contributors
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

// uninterpreted function to return an undefined value.
function $undefined_int(): int;

// Recursive exponentiation function
// Undefined unless e >=0.  $pow(0,0) is also undefined.
function $pow(n: int, e: int): int {
    if n != 0 && e == 0 then 1
    else if e > 0 then n * $pow(n, e - 1)
    else $undefined_int()
}

function $shl(src1: int, p: int): int {
    src1 * $pow(2, p)
}

function $shr(src1: int, p: int): int {
    src1 div $pow(2, p)
}

// We need to know the size of the destination in order to drop bits
// that have been shifted left more than that, so we have $ShlU8/64/128
procedure {:inline 1} $ShlU8(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    // src2 is a u8
    assume src2 >= 0 && src2 < 256;
    dst := $shl(src1, src2) mod 256;
}

procedure {:inline 1} $ShlU64(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    // src2 is a u8
    assume src2 >= 0 && src2 < 256;
    dst := $shl(src1, src2) mod 18446744073709551616;
}

procedure {:inline 1} $ShlU128(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    // src2 is a u8
    assume src2 >= 0 && src2 < 256;
    dst := $shl(src1, src2) mod 340282366920938463463374607431768211456;
}

// We don't need to know the size of destination, so no $ShrU8, etc.
procedure {:inline 1} $Shr(src1: int, src2: int) returns (dst: int)
{
    var res: int;
    // src2 is a u8
    assume src2 >= 0 && src2 < 256;
    dst := $shr(src1, src2);
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


// fun Puzzle::assert0 [baseline] at ./sources/puzzle.move:2:5+51
procedure {:inline 1} $42_Puzzle_assert0(_$t0: bool) returns ()
{
    // declare local variables
    var $t1: int;
    var $t0: bool;
    var $temp_0'bool': bool;
    $t0 := _$t0;

    // bytecode translation starts here
    // trace_local[b]($t0) at ./sources/puzzle.move:2:5+1
    assume {:print "$at(2,26,27)"} true;
    assume {:print "$track_local(0,0,0):", $t0} $t0 == $t0;

    // if ($t0) goto L0 else goto L1 at ./sources/puzzle.move:3:9+13
    assume {:print "$at(2,57,70)"} true;
    if ($t0) { goto L0; } else { goto L1; }

    // label L1 at ./sources/puzzle.move:3:20+1
L1:

    // $t1 := 0 at ./sources/puzzle.move:3:20+1
    $t1 := 0;
    assume $IsValid'u64'($t1);

    // trace_abort($t1) at ./sources/puzzle.move:3:9+13
    assume {:print "$at(2,57,70)"} true;
    assume {:print "$track_abort(0,0):", $t1} $t1 == $t1;

    // goto L3 at ./sources/puzzle.move:3:9+13
    goto L3;

    // label L0 at ./sources/puzzle.move:3:22+1
L0:

    // label L2 at ./sources/puzzle.move:4:5+1
    assume {:print "$at(2,76,77)"} true;
L2:

    // return () at ./sources/puzzle.move:4:5+1
    return;

    // label L3 at ./sources/puzzle.move:4:5+1
L3:

    // abort($t1) at ./sources/puzzle.move:4:5+1
    $abort_code := $t1;
    $abort_flag := true;
    return;

}

// fun Puzzle::assert0 [verification] at ./sources/puzzle.move:2:5+51
procedure {:timeLimit 40} $42_Puzzle_assert0$verify(_$t0: bool) returns ()
{
    // declare local variables
    var $t1: int;
    var $t0: bool;
    var $temp_0'bool': bool;
    $t0 := _$t0;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/puzzle.move:2:5+1
    assume {:print "$at(2,26,27)"} true;
    assume $IsValid'bool'($t0);

    // trace_local[b]($t0) at ./sources/puzzle.move:2:5+1
    assume {:print "$track_local(0,0,0):", $t0} $t0 == $t0;

    // if ($t0) goto L0 else goto L1 at ./sources/puzzle.move:3:9+13
    assume {:print "$at(2,57,70)"} true;
    if ($t0) { goto L0; } else { goto L1; }

    // label L1 at ./sources/puzzle.move:3:20+1
L1:

    // $t1 := 0 at ./sources/puzzle.move:3:20+1
    $t1 := 0;
    assume $IsValid'u64'($t1);

    // trace_abort($t1) at ./sources/puzzle.move:3:9+13
    assume {:print "$at(2,57,70)"} true;
    assume {:print "$track_abort(0,0):", $t1} $t1 == $t1;

    // goto L3 at ./sources/puzzle.move:3:9+13
    goto L3;

    // label L0 at ./sources/puzzle.move:3:22+1
L0:

    // label L2 at ./sources/puzzle.move:4:5+1
    assume {:print "$at(2,76,77)"} true;
L2:

    // return () at ./sources/puzzle.move:4:5+1
    return;

    // label L3 at ./sources/puzzle.move:4:5+1
L3:

    // abort($t1) at ./sources/puzzle.move:4:5+1
    $abort_code := $t1;
    $abort_flag := true;
    return;

}

// fun Puzzle::puzzle [verification] at ./sources/puzzle.move:6:5+1294
procedure {:timeLimit 40} $42_Puzzle_puzzle$verify(_$t0: int, _$t1: int, _$t2: int, _$t3: int, _$t4: int, _$t5: int, _$t6: int, _$t7: int) returns ()
{
    // declare local variables
    var $t8: bool;
    var $t9: bool;
    var $t10: bool;
    var $t11: bool;
    var $t12: bool;
    var $t13: bool;
    var $t14: bool;
    var $t15: bool;
    var $t16: int;
    var $t17: int;
    var $t18: bool;
    var $t19: int;
    var $t20: bool;
    var $t21: bool;
    var $t22: int;
    var $t23: int;
    var $t24: bool;
    var $t25: int;
    var $t26: bool;
    var $t27: bool;
    var $t28: int;
    var $t29: bool;
    var $t30: int;
    var $t31: bool;
    var $t32: bool;
    var $t33: int;
    var $t34: bool;
    var $t35: int;
    var $t36: bool;
    var $t37: bool;
    var $t38: int;
    var $t39: bool;
    var $t40: int;
    var $t41: bool;
    var $t42: bool;
    var $t43: int;
    var $t44: bool;
    var $t45: int;
    var $t46: bool;
    var $t47: bool;
    var $t48: int;
    var $t49: bool;
    var $t50: int;
    var $t51: bool;
    var $t52: bool;
    var $t53: int;
    var $t54: bool;
    var $t55: int;
    var $t56: bool;
    var $t57: bool;
    var $t58: int;
    var $t59: int;
    var $t60: bool;
    var $t61: bool;
    var $t62: bool;
    var $t63: bool;
    var $t64: int;
    var $t65: bool;
    var $t66: int;
    var $t67: int;
    var $t68: int;
    var $t69: bool;
    var $t70: int;
    var $t71: int;
    var $t72: int;
    var $t73: bool;
    var $t74: int;
    var $t75: bool;
    var $t76: int;
    var $t77: int;
    var $t78: int;
    var $t79: bool;
    var $t80: int;
    var $t81: int;
    var $t82: int;
    var $t83: int;
    var $t84: int;
    var $t85: int;
    var $t86: int;
    var $t87: bool;
    var $t88: int;
    var $t89: int;
    var $t90: int;
    var $t91: int;
    var $t92: int;
    var $t93: bool;
    var $t0: int;
    var $t1: int;
    var $t2: int;
    var $t3: int;
    var $t4: int;
    var $t5: int;
    var $t6: int;
    var $t7: int;
    var $temp_0'bool': bool;
    var $temp_0'u64': int;
    $t0 := _$t0;
    $t1 := _$t1;
    $t2 := _$t2;
    $t3 := _$t3;
    $t4 := _$t4;
    $t5 := _$t5;
    $t6 := _$t6;
    $t7 := _$t7;

    // verification entrypoint assumptions
    call $InitVerification();

    // bytecode translation starts here
    // assume WellFormed($t0) at ./sources/puzzle.move:6:5+1
    assume {:print "$at(2,83,84)"} true;
    assume $IsValid'u64'($t0);

    // assume WellFormed($t1) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t1);

    // assume WellFormed($t2) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t2);

    // assume WellFormed($t3) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t3);

    // assume WellFormed($t4) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t4);

    // assume WellFormed($t5) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t5);

    // assume WellFormed($t6) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t6);

    // assume WellFormed($t7) at ./sources/puzzle.move:6:5+1
    assume $IsValid'u64'($t7);

    // trace_local[a]($t0) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,0):", $t0} $t0 == $t0;

    // trace_local[b]($t1) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,1):", $t1} $t1 == $t1;

    // trace_local[c]($t2) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,2):", $t2} $t2 == $t2;

    // trace_local[d]($t3) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,3):", $t3} $t3 == $t3;

    // trace_local[e]($t4) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,4):", $t4} $t4 == $t4;

    // trace_local[f]($t5) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,5):", $t5} $t5 == $t5;

    // trace_local[g]($t6) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,6):", $t6} $t6 == $t6;

    // trace_local[h]($t7) at ./sources/puzzle.move:6:5+1
    assume {:print "$track_local(0,1,7):", $t7} $t7 == $t7;

    // $t17 := 1 at ./sources/puzzle.move:7:17+1
    assume {:print "$at(2,176,177)"} true;
    $t17 := 1;
    assume $IsValid'u64'($t17);

    // $t18 := <=($t17, $t0) at ./sources/puzzle.move:7:19+2
    call $t18 := $Le($t17, $t0);

    // if ($t18) goto L0 else goto L2 at ./sources/puzzle.move:7:17+16
    if ($t18) { goto L0; } else { goto L2; }

    // label L0 at ./sources/puzzle.move:7:27+1
L0:

    // $t19 := 9 at ./sources/puzzle.move:7:32+1
    $t19 := 9;
    assume $IsValid'u64'($t19);

    // $t20 := <=($t0, $t19) at ./sources/puzzle.move:7:29+2
    call $t20 := $Le($t0, $t19);

    // $t8 := $t20 at ./sources/puzzle.move:7:17+16
    $t8 := $t20;

    // trace_local[tmp#$8]($t20) at ./sources/puzzle.move:7:17+16
    assume {:print "$track_local(0,1,8):", $t20} $t20 == $t20;

    // goto L3 at ./sources/puzzle.move:7:17+16
    goto L3;

    // label L2 at ./sources/puzzle.move:7:17+16
L2:

    // $t21 := false at ./sources/puzzle.move:7:17+16
    $t21 := false;
    assume $IsValid'bool'($t21);

    // $t8 := $t21 at ./sources/puzzle.move:7:17+16
    $t8 := $t21;

    // trace_local[tmp#$8]($t21) at ./sources/puzzle.move:7:17+16
    assume {:print "$track_local(0,1,8):", $t21} $t21 == $t21;

    // label L3 at ./sources/puzzle.move:7:17+16
L3:

    // Puzzle::assert0($t8) on_abort goto L33 with $t22 at ./sources/puzzle.move:7:9+25
    call $42_Puzzle_assert0($t8);
    if ($abort_flag) {
        assume {:print "$at(2,168,193)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t23 := 1 at ./sources/puzzle.move:8:17+1
    assume {:print "$at(2,234,235)"} true;
    $t23 := 1;
    assume $IsValid'u64'($t23);

    // $t24 := <=($t23, $t1) at ./sources/puzzle.move:8:19+2
    call $t24 := $Le($t23, $t1);

    // if ($t24) goto L4 else goto L6 at ./sources/puzzle.move:8:17+16
    if ($t24) { goto L4; } else { goto L6; }

    // label L4 at ./sources/puzzle.move:8:27+1
L4:

    // $t25 := 9 at ./sources/puzzle.move:8:32+1
    $t25 := 9;
    assume $IsValid'u64'($t25);

    // $t26 := <=($t1, $t25) at ./sources/puzzle.move:8:29+2
    call $t26 := $Le($t1, $t25);

    // $t9 := $t26 at ./sources/puzzle.move:8:17+16
    $t9 := $t26;

    // trace_local[tmp#$9]($t26) at ./sources/puzzle.move:8:17+16
    assume {:print "$track_local(0,1,9):", $t26} $t26 == $t26;

    // goto L7 at ./sources/puzzle.move:8:17+16
    goto L7;

    // label L6 at ./sources/puzzle.move:8:17+16
L6:

    // $t27 := false at ./sources/puzzle.move:8:17+16
    $t27 := false;
    assume $IsValid'bool'($t27);

    // $t9 := $t27 at ./sources/puzzle.move:8:17+16
    $t9 := $t27;

    // trace_local[tmp#$9]($t27) at ./sources/puzzle.move:8:17+16
    assume {:print "$track_local(0,1,9):", $t27} $t27 == $t27;

    // label L7 at ./sources/puzzle.move:8:17+16
L7:

    // Puzzle::assert0($t9) on_abort goto L33 with $t22 at ./sources/puzzle.move:8:9+25
    call $42_Puzzle_assert0($t9);
    if ($abort_flag) {
        assume {:print "$at(2,226,251)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t28 := 1 at ./sources/puzzle.move:9:17+1
    assume {:print "$at(2,292,293)"} true;
    $t28 := 1;
    assume $IsValid'u64'($t28);

    // $t29 := <=($t28, $t2) at ./sources/puzzle.move:9:19+2
    call $t29 := $Le($t28, $t2);

    // if ($t29) goto L8 else goto L10 at ./sources/puzzle.move:9:17+16
    if ($t29) { goto L8; } else { goto L10; }

    // label L8 at ./sources/puzzle.move:9:27+1
L8:

    // $t30 := 9 at ./sources/puzzle.move:9:32+1
    $t30 := 9;
    assume $IsValid'u64'($t30);

    // $t31 := <=($t2, $t30) at ./sources/puzzle.move:9:29+2
    call $t31 := $Le($t2, $t30);

    // $t10 := $t31 at ./sources/puzzle.move:9:17+16
    $t10 := $t31;

    // trace_local[tmp#$10]($t31) at ./sources/puzzle.move:9:17+16
    assume {:print "$track_local(0,1,10):", $t31} $t31 == $t31;

    // goto L11 at ./sources/puzzle.move:9:17+16
    goto L11;

    // label L10 at ./sources/puzzle.move:9:17+16
L10:

    // $t32 := false at ./sources/puzzle.move:9:17+16
    $t32 := false;
    assume $IsValid'bool'($t32);

    // $t10 := $t32 at ./sources/puzzle.move:9:17+16
    $t10 := $t32;

    // trace_local[tmp#$10]($t32) at ./sources/puzzle.move:9:17+16
    assume {:print "$track_local(0,1,10):", $t32} $t32 == $t32;

    // label L11 at ./sources/puzzle.move:9:17+16
L11:

    // Puzzle::assert0($t10) on_abort goto L33 with $t22 at ./sources/puzzle.move:9:9+25
    call $42_Puzzle_assert0($t10);
    if ($abort_flag) {
        assume {:print "$at(2,284,309)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t33 := 1 at ./sources/puzzle.move:10:17+1
    assume {:print "$at(2,350,351)"} true;
    $t33 := 1;
    assume $IsValid'u64'($t33);

    // $t34 := <=($t33, $t3) at ./sources/puzzle.move:10:19+2
    call $t34 := $Le($t33, $t3);

    // if ($t34) goto L12 else goto L14 at ./sources/puzzle.move:10:17+16
    if ($t34) { goto L12; } else { goto L14; }

    // label L12 at ./sources/puzzle.move:10:27+1
L12:

    // $t35 := 9 at ./sources/puzzle.move:10:32+1
    $t35 := 9;
    assume $IsValid'u64'($t35);

    // $t36 := <=($t3, $t35) at ./sources/puzzle.move:10:29+2
    call $t36 := $Le($t3, $t35);

    // $t11 := $t36 at ./sources/puzzle.move:10:17+16
    $t11 := $t36;

    // trace_local[tmp#$11]($t36) at ./sources/puzzle.move:10:17+16
    assume {:print "$track_local(0,1,11):", $t36} $t36 == $t36;

    // goto L15 at ./sources/puzzle.move:10:17+16
    goto L15;

    // label L14 at ./sources/puzzle.move:10:17+16
L14:

    // $t37 := false at ./sources/puzzle.move:10:17+16
    $t37 := false;
    assume $IsValid'bool'($t37);

    // $t11 := $t37 at ./sources/puzzle.move:10:17+16
    $t11 := $t37;

    // trace_local[tmp#$11]($t37) at ./sources/puzzle.move:10:17+16
    assume {:print "$track_local(0,1,11):", $t37} $t37 == $t37;

    // label L15 at ./sources/puzzle.move:10:17+16
L15:

    // Puzzle::assert0($t11) on_abort goto L33 with $t22 at ./sources/puzzle.move:10:9+25
    call $42_Puzzle_assert0($t11);
    if ($abort_flag) {
        assume {:print "$at(2,342,367)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t38 := 1 at ./sources/puzzle.move:11:17+1
    assume {:print "$at(2,408,409)"} true;
    $t38 := 1;
    assume $IsValid'u64'($t38);

    // $t39 := <=($t38, $t4) at ./sources/puzzle.move:11:19+2
    call $t39 := $Le($t38, $t4);

    // if ($t39) goto L16 else goto L18 at ./sources/puzzle.move:11:17+16
    if ($t39) { goto L16; } else { goto L18; }

    // label L16 at ./sources/puzzle.move:11:27+1
L16:

    // $t40 := 9 at ./sources/puzzle.move:11:32+1
    $t40 := 9;
    assume $IsValid'u64'($t40);

    // $t41 := <=($t4, $t40) at ./sources/puzzle.move:11:29+2
    call $t41 := $Le($t4, $t40);

    // $t12 := $t41 at ./sources/puzzle.move:11:17+16
    $t12 := $t41;

    // trace_local[tmp#$12]($t41) at ./sources/puzzle.move:11:17+16
    assume {:print "$track_local(0,1,12):", $t41} $t41 == $t41;

    // goto L19 at ./sources/puzzle.move:11:17+16
    goto L19;

    // label L18 at ./sources/puzzle.move:11:17+16
L18:

    // $t42 := false at ./sources/puzzle.move:11:17+16
    $t42 := false;
    assume $IsValid'bool'($t42);

    // $t12 := $t42 at ./sources/puzzle.move:11:17+16
    $t12 := $t42;

    // trace_local[tmp#$12]($t42) at ./sources/puzzle.move:11:17+16
    assume {:print "$track_local(0,1,12):", $t42} $t42 == $t42;

    // label L19 at ./sources/puzzle.move:11:17+16
L19:

    // Puzzle::assert0($t12) on_abort goto L33 with $t22 at ./sources/puzzle.move:11:9+25
    call $42_Puzzle_assert0($t12);
    if ($abort_flag) {
        assume {:print "$at(2,400,425)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t43 := 1 at ./sources/puzzle.move:12:17+1
    assume {:print "$at(2,466,467)"} true;
    $t43 := 1;
    assume $IsValid'u64'($t43);

    // $t44 := <=($t43, $t5) at ./sources/puzzle.move:12:19+2
    call $t44 := $Le($t43, $t5);

    // if ($t44) goto L20 else goto L22 at ./sources/puzzle.move:12:17+16
    if ($t44) { goto L20; } else { goto L22; }

    // label L20 at ./sources/puzzle.move:12:27+1
L20:

    // $t45 := 9 at ./sources/puzzle.move:12:32+1
    $t45 := 9;
    assume $IsValid'u64'($t45);

    // $t46 := <=($t5, $t45) at ./sources/puzzle.move:12:29+2
    call $t46 := $Le($t5, $t45);

    // $t13 := $t46 at ./sources/puzzle.move:12:17+16
    $t13 := $t46;

    // trace_local[tmp#$13]($t46) at ./sources/puzzle.move:12:17+16
    assume {:print "$track_local(0,1,13):", $t46} $t46 == $t46;

    // goto L23 at ./sources/puzzle.move:12:17+16
    goto L23;

    // label L22 at ./sources/puzzle.move:12:17+16
L22:

    // $t47 := false at ./sources/puzzle.move:12:17+16
    $t47 := false;
    assume $IsValid'bool'($t47);

    // $t13 := $t47 at ./sources/puzzle.move:12:17+16
    $t13 := $t47;

    // trace_local[tmp#$13]($t47) at ./sources/puzzle.move:12:17+16
    assume {:print "$track_local(0,1,13):", $t47} $t47 == $t47;

    // label L23 at ./sources/puzzle.move:12:17+16
L23:

    // Puzzle::assert0($t13) on_abort goto L33 with $t22 at ./sources/puzzle.move:12:9+25
    call $42_Puzzle_assert0($t13);
    if ($abort_flag) {
        assume {:print "$at(2,458,483)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t48 := 1 at ./sources/puzzle.move:13:17+1
    assume {:print "$at(2,524,525)"} true;
    $t48 := 1;
    assume $IsValid'u64'($t48);

    // $t49 := <=($t48, $t6) at ./sources/puzzle.move:13:19+2
    call $t49 := $Le($t48, $t6);

    // if ($t49) goto L24 else goto L26 at ./sources/puzzle.move:13:17+16
    if ($t49) { goto L24; } else { goto L26; }

    // label L24 at ./sources/puzzle.move:13:27+1
L24:

    // $t50 := 9 at ./sources/puzzle.move:13:32+1
    $t50 := 9;
    assume $IsValid'u64'($t50);

    // $t51 := <=($t6, $t50) at ./sources/puzzle.move:13:29+2
    call $t51 := $Le($t6, $t50);

    // $t14 := $t51 at ./sources/puzzle.move:13:17+16
    $t14 := $t51;

    // trace_local[tmp#$14]($t51) at ./sources/puzzle.move:13:17+16
    assume {:print "$track_local(0,1,14):", $t51} $t51 == $t51;

    // goto L27 at ./sources/puzzle.move:13:17+16
    goto L27;

    // label L26 at ./sources/puzzle.move:13:17+16
L26:

    // $t52 := false at ./sources/puzzle.move:13:17+16
    $t52 := false;
    assume $IsValid'bool'($t52);

    // $t14 := $t52 at ./sources/puzzle.move:13:17+16
    $t14 := $t52;

    // trace_local[tmp#$14]($t52) at ./sources/puzzle.move:13:17+16
    assume {:print "$track_local(0,1,14):", $t52} $t52 == $t52;

    // label L27 at ./sources/puzzle.move:13:17+16
L27:

    // Puzzle::assert0($t14) on_abort goto L33 with $t22 at ./sources/puzzle.move:13:9+25
    call $42_Puzzle_assert0($t14);
    if ($abort_flag) {
        assume {:print "$at(2,516,541)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t53 := 1 at ./sources/puzzle.move:14:17+1
    assume {:print "$at(2,582,583)"} true;
    $t53 := 1;
    assume $IsValid'u64'($t53);

    // $t54 := <=($t53, $t7) at ./sources/puzzle.move:14:19+2
    call $t54 := $Le($t53, $t7);

    // if ($t54) goto L28 else goto L30 at ./sources/puzzle.move:14:17+16
    if ($t54) { goto L28; } else { goto L30; }

    // label L28 at ./sources/puzzle.move:14:27+1
L28:

    // $t55 := 9 at ./sources/puzzle.move:14:32+1
    $t55 := 9;
    assume $IsValid'u64'($t55);

    // $t56 := <=($t7, $t55) at ./sources/puzzle.move:14:29+2
    call $t56 := $Le($t7, $t55);

    // $t15 := $t56 at ./sources/puzzle.move:14:17+16
    $t15 := $t56;

    // trace_local[tmp#$15]($t56) at ./sources/puzzle.move:14:17+16
    assume {:print "$track_local(0,1,15):", $t56} $t56 == $t56;

    // goto L31 at ./sources/puzzle.move:14:17+16
    goto L31;

    // label L30 at ./sources/puzzle.move:14:17+16
L30:

    // $t57 := false at ./sources/puzzle.move:14:17+16
    $t57 := false;
    assume $IsValid'bool'($t57);

    // $t15 := $t57 at ./sources/puzzle.move:14:17+16
    $t15 := $t57;

    // trace_local[tmp#$15]($t57) at ./sources/puzzle.move:14:17+16
    assume {:print "$track_local(0,1,15):", $t57} $t57 == $t57;

    // label L31 at ./sources/puzzle.move:14:17+16
L31:

    // Puzzle::assert0($t15) on_abort goto L33 with $t22 at ./sources/puzzle.move:14:9+25
    call $42_Puzzle_assert0($t15);
    if ($abort_flag) {
        assume {:print "$at(2,574,599)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t58 := 2 at ./sources/puzzle.move:16:24+1
    assume {:print "$at(2,648,649)"} true;
    $t58 := 2;
    assume $IsValid'u64'($t58);

    // $t59 := *($t2, $t58) on_abort goto L33 with $t22 at ./sources/puzzle.move:16:23+1
    call $t59 := $MulU64($t2, $t58);
    if ($abort_flag) {
        assume {:print "$at(2,647,648)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t60 := ==($t0, $t59) at ./sources/puzzle.move:16:19+2
    $t60 := $IsEqual'u64'($t0, $t59);

    // Puzzle::assert0($t60) on_abort goto L33 with $t22 at ./sources/puzzle.move:16:9+17
    call $42_Puzzle_assert0($t60);
    if ($abort_flag) {
        assume {:print "$at(2,633,650)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t61 := <($t1, $t7) at ./sources/puzzle.move:17:19+1
    assume {:print "$at(2,710,711)"} true;
    call $t61 := $Lt($t1, $t7);

    // Puzzle::assert0($t61) on_abort goto L33 with $t22 at ./sources/puzzle.move:17:9+14
    call $42_Puzzle_assert0($t61);
    if ($abort_flag) {
        assume {:print "$at(2,700,714)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t62 := ==($t2, $t4) at ./sources/puzzle.move:18:19+2
    assume {:print "$at(2,773,775)"} true;
    $t62 := $IsEqual'u64'($t2, $t4);

    // Puzzle::assert0($t62) on_abort goto L33 with $t22 at ./sources/puzzle.move:18:9+15
    call $42_Puzzle_assert0($t62);
    if ($abort_flag) {
        assume {:print "$at(2,763,778)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t63 := ==($t3, $t5) at ./sources/puzzle.move:19:19+2
    assume {:print "$at(2,835,837)"} true;
    $t63 := $IsEqual'u64'($t3, $t5);

    // Puzzle::assert0($t63) on_abort goto L33 with $t22 at ./sources/puzzle.move:19:9+15
    call $42_Puzzle_assert0($t63);
    if ($abort_flag) {
        assume {:print "$at(2,825,840)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t64 := 3 at ./sources/puzzle.move:20:22+1
    assume {:print "$at(2,900,901)"} true;
    $t64 := 3;
    assume $IsValid'u64'($t64);

    // $t65 := <=($t4, $t64) at ./sources/puzzle.move:20:19+2
    call $t65 := $Le($t4, $t64);

    // Puzzle::assert0($t65) on_abort goto L33 with $t22 at ./sources/puzzle.move:20:9+15
    call $42_Puzzle_assert0($t65);
    if ($abort_flag) {
        assume {:print "$at(2,887,902)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t66 := 2 at ./sources/puzzle.move:21:21+1
    assume {:print "$at(2,974,975)"} true;
    $t66 := 2;
    assume $IsValid'u64'($t66);

    // $t67 := %($t5, $t66) on_abort goto L33 with $t22 at ./sources/puzzle.move:21:19+1
    call $t67 := $Mod($t5, $t66);
    if ($abort_flag) {
        assume {:print "$at(2,972,973)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t68 := 1 at ./sources/puzzle.move:21:26+1
    $t68 := 1;
    assume $IsValid'u64'($t68);

    // $t69 := ==($t67, $t68) at ./sources/puzzle.move:21:23+2
    $t69 := $IsEqual'u64'($t67, $t68);

    // Puzzle::assert0($t69) on_abort goto L33 with $t22 at ./sources/puzzle.move:21:9+19
    call $42_Puzzle_assert0($t69);
    if ($abort_flag) {
        assume {:print "$at(2,962,981)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t70 := 2 at ./sources/puzzle.move:22:21+1
    assume {:print "$at(2,1029,1030)"} true;
    $t70 := 2;
    assume $IsValid'u64'($t70);

    // $t71 := %($t6, $t70) on_abort goto L33 with $t22 at ./sources/puzzle.move:22:19+1
    call $t71 := $Mod($t6, $t70);
    if ($abort_flag) {
        assume {:print "$at(2,1027,1028)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t72 := 0 at ./sources/puzzle.move:22:26+1
    $t72 := 0;
    assume $IsValid'u64'($t72);

    // $t73 := ==($t71, $t72) at ./sources/puzzle.move:22:23+2
    $t73 := $IsEqual'u64'($t71, $t72);

    // Puzzle::assert0($t73) on_abort goto L33 with $t22 at ./sources/puzzle.move:22:9+19
    call $42_Puzzle_assert0($t73);
    if ($abort_flag) {
        assume {:print "$at(2,1017,1036)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t74 := 5 at ./sources/puzzle.move:23:22+1
    assume {:print "$at(2,1086,1087)"} true;
    $t74 := 5;
    assume $IsValid'u64'($t74);

    // $t75 := >=($t7, $t74) at ./sources/puzzle.move:23:19+2
    call $t75 := $Ge($t7, $t74);

    // Puzzle::assert0($t75) on_abort goto L33 with $t22 at ./sources/puzzle.move:23:9+15
    call $42_Puzzle_assert0($t75);
    if ($abort_flag) {
        assume {:print "$at(2,1073,1088)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t76 := +($t2, $t4) on_abort goto L33 with $t22 at ./sources/puzzle.move:25:19+1
    assume {:print "$at(2,1162,1163)"} true;
    call $t76 := $AddU64($t2, $t4);
    if ($abort_flag) {
        assume {:print "$at(2,1162,1163)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t77 := 10 at ./sources/puzzle.move:25:23+2
    $t77 := 10;
    assume $IsValid'u64'($t77);

    // $t78 := %($t76, $t77) on_abort goto L33 with $t22 at ./sources/puzzle.move:25:22+1
    call $t78 := $Mod($t76, $t77);
    if ($abort_flag) {
        assume {:print "$at(2,1165,1166)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t79 := ==($t78, $t7) at ./sources/puzzle.move:25:26+2
    $t79 := $IsEqual'u64'($t78, $t7);

    // Puzzle::assert0($t79) on_abort goto L33 with $t22 at ./sources/puzzle.move:25:9+22
    call $42_Puzzle_assert0($t79);
    if ($abort_flag) {
        assume {:print "$at(2,1152,1174)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t80 := +($t2, $t4) on_abort goto L33 with $t22 at ./sources/puzzle.move:26:23+1
    assume {:print "$at(2,1223,1224)"} true;
    call $t80 := $AddU64($t2, $t4);
    if ($abort_flag) {
        assume {:print "$at(2,1223,1224)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t81 := 10 at ./sources/puzzle.move:26:27+2
    $t81 := 10;
    assume $IsValid'u64'($t81);

    // $t82 := /($t80, $t81) on_abort goto L33 with $t22 at ./sources/puzzle.move:26:26+1
    call $t82 := $Div($t80, $t81);
    if ($abort_flag) {
        assume {:print "$at(2,1226,1227)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // trace_local[carry]($t82) at ./sources/puzzle.move:26:13+5
    assume {:print "$track_local(0,1,16):", $t82} $t82 == $t82;

    // $t83 := +($t1, $t3) on_abort goto L33 with $t22 at ./sources/puzzle.move:27:19+1
    assume {:print "$at(2,1276,1277)"} true;
    call $t83 := $AddU64($t1, $t3);
    if ($abort_flag) {
        assume {:print "$at(2,1276,1277)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t84 := +($t83, $t82) on_abort goto L33 with $t22 at ./sources/puzzle.move:27:21+1
    call $t84 := $AddU64($t83, $t82);
    if ($abort_flag) {
        assume {:print "$at(2,1278,1279)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t85 := 10 at ./sources/puzzle.move:27:29+2
    $t85 := 10;
    assume $IsValid'u64'($t85);

    // $t86 := %($t84, $t85) on_abort goto L33 with $t22 at ./sources/puzzle.move:27:28+1
    call $t86 := $Mod($t84, $t85);
    if ($abort_flag) {
        assume {:print "$at(2,1285,1286)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t87 := ==($t86, $t6) at ./sources/puzzle.move:27:32+2
    $t87 := $IsEqual'u64'($t86, $t6);

    // Puzzle::assert0($t87) on_abort goto L33 with $t22 at ./sources/puzzle.move:27:9+28
    call $42_Puzzle_assert0($t87);
    if ($abort_flag) {
        assume {:print "$at(2,1266,1294)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t88 := +($t1, $t3) on_abort goto L33 with $t22 at ./sources/puzzle.move:28:21+1
    assume {:print "$at(2,1335,1336)"} true;
    call $t88 := $AddU64($t1, $t3);
    if ($abort_flag) {
        assume {:print "$at(2,1335,1336)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t89 := +($t88, $t82) on_abort goto L33 with $t22 at ./sources/puzzle.move:28:23+1
    call $t89 := $AddU64($t88, $t82);
    if ($abort_flag) {
        assume {:print "$at(2,1337,1338)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t90 := 10 at ./sources/puzzle.move:28:31+2
    $t90 := 10;
    assume $IsValid'u64'($t90);

    // $t91 := /($t89, $t90) on_abort goto L33 with $t22 at ./sources/puzzle.move:28:30+1
    call $t91 := $Div($t89, $t90);
    if ($abort_flag) {
        assume {:print "$at(2,1344,1345)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t92 := +($t0, $t91) on_abort goto L33 with $t22 at ./sources/puzzle.move:28:18+1
    call $t92 := $AddU64($t0, $t91);
    if ($abort_flag) {
        assume {:print "$at(2,1332,1333)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // $t93 := ==($t92, $t5) at ./sources/puzzle.move:28:34+2
    $t93 := $IsEqual'u64'($t92, $t5);

    // Puzzle::assert0($t93) on_abort goto L33 with $t22 at ./sources/puzzle.move:28:9+30
    call $42_Puzzle_assert0($t93);
    if ($abort_flag) {
        assume {:print "$at(2,1323,1353)"} true;
        $t22 := $abort_code;
        assume {:print "$track_abort(0,1):", $t22} $t22 == $t22;
        goto L33;
    }

    // label L32 at ./sources/puzzle.move:29:5+1
    assume {:print "$at(2,1376,1377)"} true;
L32:

    // assert Not(true) at ./sources/puzzle.move:31:9+15
    assume {:print "$at(2,1404,1419)"} true;
    assert {:msg "assert_failed(2,1404,1419): function does not abort under this condition"}
      !true;

    // return () at ./sources/puzzle.move:31:9+15
    return;

    // label L33 at ./sources/puzzle.move:29:5+1
    assume {:print "$at(2,1376,1377)"} true;
L33:

    // assert true at ./sources/puzzle.move:30:5+104
    assume {:print "$at(2,1382,1486)"} true;
    assert {:msg "assert_failed(2,1382,1486): abort not covered by any of the `aborts_if` clauses"}
      true;

    // abort($t22) at ./sources/puzzle.move:30:5+104
    $abort_code := $t22;
    $abort_flag := true;
    return;

}
