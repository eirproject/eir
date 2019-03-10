-module(testing).
-export([woo/1]).

woo(A) ->
    case A of
        5 -> woo;
        6 -> foo;
        _ -> A + 1
    end.
