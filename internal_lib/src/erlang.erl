-module(erlang).

-export(['+'/2, '-'/2]).
-export([is_float/1, is_integer/1, is_number/1]).
-export([error/1]).

'+'(A, B) ->
    eir_intrinsics:add(A, B).
'-'(A, B) ->
    eir_intrinsics:sub(A, B).

error(A) ->
    eir_intrinsics:throw_error(A).

is_float(T) ->
    eir_intrinsics:is_float(T).
is_integer(T) ->
    eir_intrinsics:is_smallint(T)
        or eir_intrinsics:is_bigint(T).
is_number(T) ->
    erlang:is_float(T) or erlang:is_integer(T).
