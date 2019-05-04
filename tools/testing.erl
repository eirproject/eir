-module(testing).

-export([fib_server/0]).

fib_server() -> 
    receive
        _ ->
            fib_server();
        exit ->
            false
    end.
