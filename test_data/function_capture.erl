-module(function_capture).
-export([woo/0]).

woo() ->
    fun woohoo:hoo/2.
