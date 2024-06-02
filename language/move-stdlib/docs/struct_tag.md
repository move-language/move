
<a name="0x1_struct_tag"></a>

# Module `0x1::struct_tag`

Module to decompose a move struct into it's components.


-  [Struct `StructTag`](#0x1_struct_tag_StructTag)
-  [Function `get`](#0x1_struct_tag_get)
-  [Function `into`](#0x1_struct_tag_into)
-  [Function `module_authority`](#0x1_struct_tag_module_authority)


<pre><code><b>use</b> <a href="ascii.md#0x1_ascii">0x1::ascii</a>;
</code></pre>



<a name="0x1_struct_tag_StructTag"></a>

## Struct `StructTag`



<pre><code><b>struct</b> <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a> <b>has</b> <b>copy</b>, drop, store
</code></pre>



<details>
<summary>Fields</summary>


<dl>
<dt>
<code>address_: <b>address</b></code>
</dt>
<dd>
 Address of the entity that the struct belongs to.
 taking <code>00000000000000000000000000000001::option::Option&lt;u64&gt;</code> for example,
 the address will be <code>00000000000000000000000000000001</code>
</dd>
<dt>
<code>module_name: <a href="ascii.md#0x1_ascii_String">ascii::String</a></code>
</dt>
<dd>
 the name of the module where the struct is defined.
 using the example struct above the module name should be <code><a href="option.md#0x1_option">option</a></code>
</dd>
<dt>
<code>struct_name: <a href="ascii.md#0x1_ascii_String">ascii::String</a></code>
</dt>
<dd>
 the name of the struct itself.
 using the example struct above the module name should be <code>Option</code>
</dd>
<dt>
<code>generics: <a href="vector.md#0x1_vector">vector</a>&lt;<a href="ascii.md#0x1_ascii_String">ascii::String</a>&gt;</code>
</dt>
<dd>
 the generics or tyepe params of the struct.
 using the example struct above the module name should be <code><a href="vector.md#0x1_vector">vector</a>["u64"]</code>
</dd>
</dl>


</details>

<a name="0x1_struct_tag_get"></a>

## Function `get`

Returns the tag of the struct of type <code>T</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_get">get</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>native</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_get">get</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>;
</code></pre>



</details>

<a name="0x1_struct_tag_into"></a>

## Function `into`



<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_into">into</a>(self: &<a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>): (<b>address</b>, <a href="ascii.md#0x1_ascii_String">ascii::String</a>, <a href="ascii.md#0x1_ascii_String">ascii::String</a>, <a href="vector.md#0x1_vector">vector</a>&lt;<a href="ascii.md#0x1_ascii_String">ascii::String</a>&gt;)
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_into">into</a>(self: &<a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>): (<b>address</b>, String, String, <a href="vector.md#0x1_vector">vector</a>&lt;String&gt;) {
    (self.address_, self.module_name, self.struct_name, self.generics)
}
</code></pre>



</details>

<a name="0x1_struct_tag_module_authority"></a>

## Function `module_authority`

Returns the module authority for the struct of type <code>T</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_module_authority">module_authority</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_module_authority">module_authority</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a> {
    <b>let</b> <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a> {
        address_,
        module_name,
        struct_name: _,
        generics: _
    } = <a href="struct_tag.md#0x1_struct_tag_get">get</a>&lt;T&gt;();

    <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a> {
        address_,
        module_name,
        struct_name: <a href="ascii.md#0x1_ascii_string">ascii::string</a>(b"Witness"),
        generics: <a href="vector.md#0x1_vector">vector</a>[]
    }
}
</code></pre>



</details>


[//]: # ("File containing references which can be used from documentation")
