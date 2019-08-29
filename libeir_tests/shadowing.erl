-module(shadowing).

-export([run/0]).

run() ->
    1 = case_matching({1}, 1),
    {_, _} = (catch case_matching({1}, 2)),

    1 = fun_shadowing({1}, 1),
    2 = fun_shadowing({1}, 2).

case_matching(A, B) ->
    case A of
        {B} -> B
    end.

fun_shadowing(A, B) ->
    C = fun({B}) -> B end,
    C(A).

