-module(test).
-import(hello, [start/0]).
-export([myfun/0]).

myfun() ->
    start().