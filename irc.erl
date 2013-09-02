-module(irc).
-export([start/0]).
-export([handle_command/2]).
-import(lists, [reverse/1]).
-import(splitter).

-define(CRNL, "\r\n").

% Create a new tcp client process, on successful return of a Socket, send the
% nickname and enter a loop to handle I/O.
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


% Receive an empty list of line from the irc server, nothing to do.
handle_response(_Socket, {lines, []}) ->
    done;

% Receive a list of lines from the server, handle them one by one.
handle_response(Socket, {lines, Lines}) ->
    [FirstLine|OtherLines] = Lines,
    handle_line(Socket, FirstLine),
    handle_response(Socket, {lines, OtherLines});

% Receive a string from the server, split 
handle_response(Socket, Response) ->
    handle_response(Socket, {lines, re:split(Response, "\r|\n")}).


% Take care of a line, with no words.
handle_line(_Socket, []) ->
    done;

% Take care of a line/string
handle_line(Socket, Line) ->
    Message = re:split(Line, " ", [{return, list}]),

    case Message of
        % Code 004 means we're ready!
        [_|["004"|_]] ->
            timer:sleep(1000),
            handle_event(Socket, {ready});

        % A user send a message to a given channel.
        [PvUser|["PRIVMSG"|[PvChannel|PvMessage]]] ->
            handle_event(Socket, {privmsg, PvUser, PvChannel, PvMessage});

        % Pass the Host without the leading colon.
        ["PING", PingHost] ->
            handle_event(Socket, {ping, tl(PingHost)});

        % No op
        [[]] ->
            empty;

        % Unknown op
        _ ->
            io:format("Unknown Message: ~p\n", [Message]),
            unknown
    end.


% Send a JOIN command to the server.
join(Socket, Channel) ->
    io:format("JOIN ~p\n", [Channel]),
    gen_tcp:send(Socket, "JOIN " ++ Channel ++ "\r\n").


% When we are ready to go, join the default chan.
handle_event(Socket, {ready}) ->
    join(Socket, "#debsquad");

% Message sent by a user on a chan we are watching.
handle_event(Socket, {privmsg, User, Channel, Message}) ->
    io:format("CHAN MSG ~p on ~p -> ~p\n", [User, Channel, Message]),

    case Message of
        % Addressed to us, this is a command!
        [":MrSplitMan:"|Parameters] ->
            ?MODULE:handle_command(Socket, Parameters);
        % Don't understand... ignore it.
        _ ->
            nothing
    end;

% Server requests a Ping/Pong exchange.
handle_event(Socket, {ping, Host}) ->
    io:format("PONG ~p\n", [Host]),
    gen_tcp:send(Socket, ":" ++ Host ++ " PONG " ++ Host ++ " :MrSplitMan\r\n").


% We received a command, redirect to the right handler.
handle_command(Socket, [Command|Parameters]) ->
    case Command of
        "split" ->
            handle_command(Socket, {magic_split, Parameters});

        "die" ->
            handle_command(Socket, {die, Parameters});

        % Don't understand... ignore it.
        _ ->
            handle_command(Socket, {wtf, Parameters})
    end;

% Splitter handler.
handle_command(Socket, {magic_split, Value}) ->
    MagicList = splitter:start(Value),
    gen_tcp:send(Socket, "PRIVMSG #debsquad :" ++ MagicList ++ "\r\n");

% Die
handle_command(Socket, {die, _Value}) ->
    gen_tcp:send(Socket, "QUIT :Monde cruel...\r\n"),
    gen_tcp:close(Socket);

% WTF handler.
handle_command(Socket, {wtf, _Value}) ->
    gen_tcp:send(Socket, "PRIVMSG #debsquad :Hmmm ?\r\n").


% Socket I/O loop
loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            handle_response(Socket, Bin),
            loop(Socket);
        {tcp_closed, Socket} ->
            true
    end.
