-module(irc).
-export([start/0]).
-import(lists, [reverse/1]).
-import(splitter).

-define(CRNL, "\r\n").

start() ->
    case gen_tcp:connect("irc.oftc.net", 6667, [binary, {packet, 0}]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "NICK MrSplitMan\n" ++
                            "USER mrsplitman mrsplitman tamentis.ath.cx :MrSplitMan\n"),
            loop(Socket),
            io:format("Launched!\n");
        _ ->
            error
    end.

handle_response(Socket, {lines, []}) ->
    done;

handle_response(Socket, {lines, Lines}) ->
    [FirstLine|OtherLines] = Lines,
    handle_line(Socket, FirstLine),
    handle_response(Socket, {lines, OtherLines});
    
handle_response(Socket, Response) ->
    handle_response(Socket, {lines, re:split(Response, "\r|\n")}).

handle_line(Socket, []) ->
    done;

handle_line(Socket, Line) ->
    Message = re:split(Line, " ", [{return, list}]),

    case Message of
        [_|["004"|_]] ->
            timer:sleep(1000),
            handle_event(Socket, {ready});

        [PvUser|["PRIVMSG"|[PvChannel|PvMessage]]] ->
            handle_event(Socket, {privmsg, PvUser, PvChannel, PvMessage});

        % Pass the Host without the leading colon.
        ["PING", PingHost] ->
            handle_event(Socket, {ping, tl(PingHost)});

        [[]] ->
            empty;

        _ ->
            io:format("Unknown Message: ~p\n", [Message]),
            unknown
    end.

join(Socket, Channel) ->
    io:format("JOIN ~p\n", [Channel]),
    gen_tcp:send(Socket, "JOIN " ++ Channel ++ "\r\n").

handle_event(Socket, {ready}) ->
    join(Socket, "#debsquad");

handle_event(Socket, {privmsg, User, Channel, Message}) ->
    io:format("CHAN MSG ~p -> ~p\n", [Channel, Message]),
    case Message of
        [":MrSplitMan:"|["split",SplitValue]] ->
            handle_event(Socket, {magic_split, SplitValue});
        _ ->
            nothing
    end;

handle_event(Socket, {magic_split, Value}) ->
    MagicList = splitter:start(Value),
    gen_tcp:send(Socket, "PRIVMSG #debsquad :" ++ MagicList ++ "\r\n");

handle_event(Socket, {ping, Host}) ->
    io:format("PONG ~p\n", [Host]),
    gen_tcp:send(Socket, ":" ++ Host ++ " PONG " ++ Host ++ " :MrSplitMan\r\n").

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            handle_response(Socket, Bin),
            loop(Socket);
        {tcp_closed, Socket} ->
            true
    end.
