axiom spec_fun_non_zero() > 0;
invariant exists x in 1..10, y in 8..12 : x == y;
apply ModuleInvariant<X, Y> to *foo*<Y, X>;
