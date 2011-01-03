-module(splitter).
-export([start/1]).

start(String) ->
    Value = split(String),
    io_lib:format("~p", [Value]).

numeric(Char) ->
    if
        Char < $0 ->
            false;
        Char > $9 ->
            false;
        true -> true
    end.

split(String) ->
    split(String, []).

split([], Tokens) ->
    Tokens;

split(String, Tokens) ->
    {Extracted, Remaining} = next_token(String),
    Int = (catch erlang:list_to_integer(Extracted)),
    case is_number(Int) of
        true -> split(Remaining, Tokens ++ [Int]);
        false -> split(Remaining, Tokens ++ [Extracted])
    end.

next_token([]) ->
    [];

next_token({initial, String}) ->
    [Head|Tail] = String,
    next_token({next, Tail, [Head]});

next_token({final, Remaining, Extracted}) ->
    {Extracted, Remaining};

next_token({next, [], Extracted}) ->
    next_token({final, [], Extracted});

next_token({next, String, Extracted}) ->
    [Head|Tail] = String,
    case numeric(Head) == numeric(hd(Extracted)) of
        true -> 
            next_token({next, Tail, Extracted ++ [Head]});
        false ->
            next_token({final, String, Extracted})
    end;

next_token(String) ->
    next_token({initial, String}).

