
<a name="0x1_struct_tag"></a>

# Module `0x1::struct_tag`

Module to decompose a move struct into it's components.


-  [Struct `StructTag`](#0x1_struct_tag_StructTag)
-  [Function `get`](#0x1_struct_tag_get)
-  [Function `package_address`](#0x1_struct_tag_package_address)
-  [Function `module_name`](#0x1_struct_tag_module_name)
-  [Function `struct_name`](#0x1_struct_tag_struct_name)
-  [Function `generics`](#0x1_struct_tag_generics)


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
<code>package_address: <b>address</b></code>
</dt>
<dd>
 Address of the package that the struct belongs to.
 taking <code>00000000000000000000000000000001::option::Option&lt;u64&gt;</code> for example,
 it's package address will be <code>00000000000000000000000000000001</code>
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
 using the example struct above the module name should be <code><a href="vector.md#0x1_vector">vector</a>[u64]</code>
</dd>
</dl>


</details>

<a name="0x1_struct_tag_get"></a>

## Function `get`

Return the tag of the struct of type <code>T</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_get">get</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>native</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_get">get</a>&lt;T&gt;(): <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>;
</code></pre>



</details>

<a name="0x1_struct_tag_package_address"></a>

## Function `package_address`

Returns the package address of <code>self</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_package_address">package_address</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>): <b>address</b>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_package_address">package_address</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>): <b>address</b> {
    self.package_address
}
</code></pre>



</details>

<a name="0x1_struct_tag_module_name"></a>

## Function `module_name`

Returns the module name of <code>self</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_module_name">module_name</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>): <a href="ascii.md#0x1_ascii_String">ascii::String</a>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_module_name">module_name</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>): String {
    self.module_name
}
</code></pre>



</details>

<a name="0x1_struct_tag_struct_name"></a>

## Function `struct_name`

Returns the struct name of <code>self</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_struct_name">struct_name</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>): <a href="ascii.md#0x1_ascii_String">ascii::String</a>
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_struct_name">struct_name</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>): String {
    self.struct_name
}
</code></pre>



</details>

<a name="0x1_struct_tag_generics"></a>

## Function `generics`

Returns the generics of <code>self</code>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_generics">generics</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">struct_tag::StructTag</a>): <a href="vector.md#0x1_vector">vector</a>&lt;<a href="ascii.md#0x1_ascii_String">ascii::String</a>&gt;
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>public</b> <b>fun</b> <a href="struct_tag.md#0x1_struct_tag_generics">generics</a>(self: <a href="struct_tag.md#0x1_struct_tag_StructTag">StructTag</a>): <a href="vector.md#0x1_vector">vector</a>&lt;String&gt; {
    self.generics
}
</code></pre>



</details>


[//]: # ("File containing references which can be used from documentation")
