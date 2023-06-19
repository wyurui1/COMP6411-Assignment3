-module(hello).
-export([start/0]).

start() ->
    Pid = spawn(fun print_output/0),
    Pid ! {self(), "~n~n** The financial market is opening for the day **~n~n"},
    init:stop().

print_output() ->
    receive
        {Sender, Msg} ->
            io:format(Msg),
            Sender ! ok
    end.

