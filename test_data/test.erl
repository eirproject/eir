-module(test).
-export([add/2, add_two/3, return_closure/1]).

add(A, B) ->
    A + B.

add_two(A, B, C) ->
    I = add(A, B),
    add(I, C).

return_closure(I) ->
    fun(A) ->
            add(I, A)
    end.
