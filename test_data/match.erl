-module(match).
-export([woo/0, hoo/1]).

woo() ->
    receive
        {send, Woo, {_Something, _Else} = Woo} ->
            Woo;
        {woo, Hoo} ->
            Hoo
    end.

hoo(Hoo) ->
    receive
        {send, Hoo} ->
            Hoo
    end.

