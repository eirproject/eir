-module(basic_module).
-export([woo/1]).

woo(A) -> foo:foo(A).
