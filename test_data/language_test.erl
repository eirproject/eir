-module(language_test).

-export([]).

module_function_capture() ->
    fun woohoo:hoo/2.

do_throw() ->
    throw(something).
do_error() ->
    erlang:error(something).
do_exit() ->
    erlang:exit(something).

try_catch(A) ->
    try A() of
        _ -> woo
    catch
        throw:hoo -> a_throw
    end.

try_catch_finally(A) ->
    try A() of
        _ -> woo
    catch
        throw:hoo -> a_throw
    after
        some_after_clause
    end.

self_tail_call(A) ->
    case A of
        0 ->
            reached_end;
        _ ->
            self_tail_call(A - 1)
    end.

binary_construct_1(A) ->
    <<A>>.

binary_pattern_match_1(<<_A:4/integer-signed-little-unit:8, _B/binary>>) ->
    matched.
binary_pattern_match_2(<<_A:4/integer-signed-little-unit:8, _B/integer>>) ->
    matched.
binary_pattern_match_3(<<_A:2/integer-signed-little-unit:2, _B/binary>>) ->
    matched.
binary_pattern_match_4(<<_A:4/float-unit:8, _B/binary>>) ->
    matched.
