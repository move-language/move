
<a name="0x1_debug"></a>

# Module `0x1::debug`

Module providing debug functionality.


-  [Function `print`](#0x1_debug_print)
-  [Function `print_stack_trace`](#0x1_debug_print_stack_trace)
-  [Module Specification](#@Module_Specification_0)


<pre><code></code></pre>



<a name="0x1_debug_print"></a>

## Function `print`



<pre><code><b>public</b> <b>fun</b> <a href="debug.md#0x1_debug_print">print</a>&lt;T&gt;(x: &T)
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>native</b> <b>public</b> <b>fun</b> <a href="debug.md#0x1_debug_print">print</a>&lt;T&gt;(x: &T);
</code></pre>



</details>

<details>
<summary>Specification</summary>



<pre><code><b>pragma</b> intrinsic = intrinsic_trace_value;
</code></pre>



</details>

<a name="0x1_debug_print_stack_trace"></a>

## Function `print_stack_trace`



<pre><code><b>public</b> <b>fun</b> <a href="debug.md#0x1_debug_print_stack_trace">print_stack_trace</a>()
</code></pre>



<details>
<summary>Implementation</summary>


<pre><code><b>native</b> <b>public</b> <b>fun</b> <a href="debug.md#0x1_debug_print_stack_trace">print_stack_trace</a>();
</code></pre>



</details>

<details>
<summary>Specification</summary>



<pre><code><b>pragma</b> intrinsic = intrinsic_trace_stack;
</code></pre>



</details>

<a name="@Module_Specification_0"></a>

## Module Specification



<a name="0x1_debug_intrinsic_trace_value"></a>


<pre><code><b>native</b> <b>fun</b> <a href="debug.md#0x1_debug_intrinsic_trace_value">intrinsic_trace_value</a>&lt;T&gt;(x: &T);
<a name="0x1_debug_intrinsic_trace_stack"></a>
<b>native</b> <b>fun</b> <a href="debug.md#0x1_debug_intrinsic_trace_stack">intrinsic_trace_stack</a>();
</code></pre>
