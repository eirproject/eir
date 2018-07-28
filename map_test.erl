-module(maptest).
-export([test/0, test6/1, test7/1]).
%-export([test/0, test2/2, test3/2, test4/2]).

test() ->
    #{woo => "hoo", foo => bar}.

test6(Woo) ->
    case Woo of
        #{ {hoo} := true, foo := bar } -> lsdlla;
        _ -> false
    end.

test7(Woo) ->
    case Woo of
        #{ #{ foo => bar } := true } -> lpsdlas;
        _ -> test7(Woo)
    end.

foo() -> 12.

% test2(Woo, Hoo) ->
%     case Woo of
%         #{Hoo := true} -> true;
%         _ -> false
%     end.
% 
% test3(Woo, Hoo) ->
%     case Woo of
%         { Hoo } -> true;
%         _ -> false
%     end.
% 
% test4(Woo, Hoo) ->
%     case Woo of
%         { Hoo, Foo, Foo } -> true;
%         _ -> false
%     end.
% 
% test5(Woo, Hoo) ->
%     T = {Hoo},
%     case Woo of
%         #{T := true} -> true;
%         _ -> false
%     end.
