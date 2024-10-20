-module(test_client).
-include_lib("eunit/include/eunit.hrl").
-export([]).
-compile([nowarn_unused_function]).

-define(SERVER,"server"). % forces students not to hardcode "shire"
-define(SERVERATOM, list_to_atom(?SERVER)).
-define(MAX, 100000).
-define(PERF_1_USERS, 50).
-define(PERF_2_USERS, 10).
-define(PERF_2_CHANS, 100).
-define(PERF_2_MSGS, 5).

% --- Output -----------------------------------------------------------------

% Turn output off/on
% is_output_off() ->
%     get(output) == off.
output_off() ->
    put(output,off).
output_on() ->
    put(output,on).

putStrLn(S) ->
    case get(output) of
        off -> ok ;
        _   -> io:fwrite(user, <<"~s~n">>, [S])
    end.

putStrLn(S1, S2) ->
    putStrLn(io_lib:format(S1, S2)).
sprintf(S1, S2) ->
    lists:flatten(io_lib:format(S1, S2)).

% To turn off colours, just use this function:
% colour(Num,S) -> S.
colour(Num,S) ->
    "\033["++Num++"m"++S++"\033[0m".
red(S) ->
    colour("31",S).
green(S) ->
    colour("32",S).
blue(S) ->
    colour("34",S).
gray(S) ->
    colour("37",S).
% purple(S) ->
%     colour("35",S).

% --- Helpers: assertions ----------------------------------------------------

% Positive assertion, with error message
assert(Message, Condition) ->
    Pfx = Message++": ",
    case (catch(?assert(Condition))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Fail"),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.
assert(Message, X, Y) ->
    Pfx = Message++": ",
    case (catch(?assertEqual(Y, X))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Fail")++
                  sprintf("~nExpected: ~p~n     Got: ~p~n", [Y,X]),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.
% assert_ok(X) ->
%     ?assertEqual(ok, X).
assert_ok(Message, X) ->
    assert(Message, X, ok).

% Assert for particular error message
assert_error(Result, Atom) ->
    ?assertMatch({error, Atom, _}, Result).
assert_error(Message, Result, Atom) ->
    Pfx = Message++" fails: ",
    case (catch(assert_error(Result, Atom))) of
        {'EXIT', _Ex} ->
            Msg = Pfx++red("Passes")++
                  sprintf("~nExpected: {error,~p,_}~n     Got: ~p~n", [Atom,Result]),
            case get(output) of
                off -> throw(Msg) ;
                _   -> putStrLn(Msg), throw("Test failed")
            end ;
        _ -> putStrLn(Pfx++green("Ok"))
    end.

% --- Helpers ----------------------------------------------------------------

% Our own version of helper:request without a timeout
request(Pid, Data) ->
    Ref = make_ref(),
    Pid!{request, self(), Ref, Data},
    receive
    {result, Ref, Result} ->
        Result;
    {exit, Ref, Reason} ->
        exit(Reason)
    end.

% Generic to_string
to_string({_Atom,Node}) ->
    atom_to_list(Node);
to_string(X) ->
    io_lib:format("~p", [X]).

init_header(Name) ->
    putStrLn(blue("\n# Test: "++Name)).

% It is assumed that is called at the beginning of each test case (only)
init(Name) ->
    ?assert(compile:file(server) =:= {ok,server}),
    init_header(Name),
    Pid = server:start(?SERVERATOM),
    % putStrLn("server ~p", [Pid]),
    assert("server startup", is_pid(Pid)),
    Pid.

% Call stop function on server. This should stop channels too.
server_stop() ->
    server:stop(?SERVERATOM),
    assert("stopping server", whereis(?SERVERATOM) =:= undefined).

% Kill server. This should leave channels running.
server_kill() ->
    exit(whereis(?SERVERATOM), kill),
    timer:sleep(500), % wait for killing to finish
    assert("killing server", whereis(?SERVERATOM) =:= undefined).

% Start new GUI and register it as Name
new_gui(Name) ->
    new_gui(Name, self()).
new_gui(Name, Mirror) ->
    catch(unregister(list_to_atom(Name))),
    {ok, Pid} = dummy_gui:start_link(Name,Mirror),
    Pid.

find_unique_name(Prefix) ->
    MStr = integer_to_list(rand:uniform(?MAX)),
    Name = Prefix++MStr,
    case whereis(list_to_atom(Name)) of
        undefined -> Name ;
        _         -> find_unique_name(Prefix)
    end.

% Start a new client
new_client() ->
    Nick = find_unique_name("user_"),
    new_client(Nick).

% Start a new client with a given nick
new_client(Nick) ->
    GUIName = find_unique_name("gui_"),
    new_client(Nick, GUIName).

% Start a new client with a given nick and GUI name
new_client(Nick, GUIName) ->
    ClientName = find_unique_name("client_"),
    ClientAtom = list_to_atom(ClientName),
    Pid = genserver:start(ClientAtom, client:initial_state(Nick, list_to_atom(GUIName), ?SERVERATOM), fun client:handle/2),
    {Pid, Nick, ClientAtom}.

% Start new client with associated dummy GUI
new_client_gui() ->
    Nick = find_unique_name("user_"),
    GUIName = find_unique_name("gui_"),
    new_gui(GUIName),
    new_client(Nick, GUIName).

% Join a channel and assert it succeeded
join_channel(ClientAtom, Channel) ->
    Result = request(ClientAtom,{join,Channel}),
    assert_ok(to_string(ClientAtom)++" joins "++Channel, Result).

% Leave a channel and assert it succeeded
leave_channel(ClientAtom, Channel) ->
    Result = request(ClientAtom,{leave,Channel}),
    assert_ok(to_string(ClientAtom)++" leaves "++Channel, Result).

% Send a message from GUI to client and assert it succeeded
send_message(ClientAtom, Channel, Message) ->
    Result = request(ClientAtom, {message_send,Channel,Message}),
    assert_ok(to_string(ClientAtom)++" sends message on "++Channel, Result).

% Change nick and assert it succeeded
change_nick(ClientAtom, Nick) ->
    Result = request(ClientAtom,{nick,Nick}),
    assert_ok(to_string(ClientAtom)++" changes nick to "++Nick, Result).

% Receive a specific message from dummy GUI
receive_message(Channel, Nick, Message) ->
    receive
        {message_receive, From, Msg} ->
            assert("channel matches", From, Channel),
            assert("message matches", Msg, Nick++"> "++Message)
    after
        5000 ->
            putStrLn(red("nothing received")),
            ?assert(false)
    end.

% Make sure thare are no pending messages from dummy GUI
no_more_messages() ->
    receive
        {message_receive, _From, _Msg} ->
            putStrLn(red("there are unreceived messages")),
            ?assert(false)
    after
        1000 ->
            assert("no more messages", true)
    end.

% Get a new channel name
new_channel() ->
    find_unique_name("#channel_").

% --- Unit tests -------------------------------------------------------------

% One user joins, writes, leaves
normal_one_user_test() ->
    init("normal_one_user"),
    Channel = new_channel(),
    {_Pid1, _Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),
    send_message(ClientAtom1, Channel, "hello"),
    send_message(ClientAtom1, Channel, "goodbye"),
    leave_channel(ClientAtom1, Channel).

% One user writes, the other receives
write_receive_test() ->
    init("write_receive"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2 with dummy GUI
    {_Pid2, _Nick2, ClientAtom2} = new_client_gui(),
    join_channel(ClientAtom2, Channel),

    % Client 1 writes to to channel
    Message = find_unique_name("message_"),
    send_message(ClientAtom1, Channel, Message),

    % Client 2 receives
    receive_message(Channel, Nick1, Message),

    % Client 2 leaves, 1 resends, no receive
    leave_channel(ClientAtom2, Channel),
    send_message(ClientAtom1, Channel, Message),
    no_more_messages(),

    ok.

% Write/receive with multiple channels
%
%  User1    User2    User3
%   | \     / | \     / |
%   |  Chan1  |  Chan2  |
%    \        |        /
%     `---- Chan3 ----`
write_receive_2_test() ->
    init("write_receive_2"),
    Channel1 = new_channel(),
    Channel2 = new_channel(),
    Channel3 = new_channel(),

    % Client 1 -> Channel 1, 3
    {_Pid1, Nick1, ClientAtom1} = new_client_gui(),
    join_channel(ClientAtom1, Channel1),
    join_channel(ClientAtom1, Channel3),

    % Client 2 -> Channel 1, 2, 3
    {_Pid2, Nick2, ClientAtom2} = new_client_gui(),
    join_channel(ClientAtom2, Channel1),
    join_channel(ClientAtom2, Channel2),
    join_channel(ClientAtom2, Channel3),

    % Client 3 -> Channel 2, 3
    {_Pid3, Nick3, ClientAtom3} = new_client_gui(),
    join_channel(ClientAtom3, Channel2),
    join_channel(ClientAtom3, Channel3),

    % Client 1 writes to channel 1
    % Receive from Client 2
    Message1 = find_unique_name("message_"),
    send_message(ClientAtom1, Channel1, Message1),
    receive_message(Channel1, Nick1, Message1),

    % Client 3 writes to channel 2
    % Receive from Client 2
    Message2 = find_unique_name("message_"),
    send_message(ClientAtom3, Channel2, Message2),
    receive_message(Channel2, Nick3, Message2),

    % Client 2 writes to channel 3
    % Receive from Client 1 and 2
    Message3 = find_unique_name("message_"),
    send_message(ClientAtom2, Channel3, Message3),
    receive_message(Channel3, Nick2, Message3),
    receive_message(Channel3, Nick2, Message3),

    no_more_messages(),
    ok.

% Connecting to non-responding server
join_nonresponding_server_test() ->
    init_header("join_nonresponding_server"),
    Pid = genserver:start(?SERVERATOM, {}, fun (St, _Msg) -> timer:sleep(100000), {dead, St} end), % blocking server
    assert("server startup", is_pid(Pid)),

    putStrLn("Wait a few seconds for timeout..."),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Channel = "#channel",
    Result = request(ClientAtom, {join,Channel}),
    assert_error("joining channel with non-responsive server", Result, server_not_reached).

% Joining when no server
join_no_server_test() ->
    init("join_no_server"),
    server_stop(),

    putStrLn("Wait a few seconds for timeout..."),
    {_Pid, _Nick, ClientAtom} = new_client(),
    Channel = "#channel",
    Result2 = request(ClientAtom,{join,Channel}),
    assert_error(to_string(ClientAtom)++" joins "++Channel, Result2, server_not_reached).

% Joining already joined
join_already_joined_test() ->
    init("join_already_joined"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client(),
    join_channel(ClientAtom, Channel),

    Result2 = request(ClientAtom,{join,Channel}),
    assert_error(to_string(ClientAtom)++" joins "++Channel, Result2, user_already_joined).

% Writing when not joined
write_not_joined1_test() ->
    init("write_not_joined1"),
    Channel = new_channel(),

    % Client
    {_Pid, _Nick, ClientAtom} = new_client(),
    join_channel(ClientAtom, Channel),
    leave_channel(ClientAtom, Channel),
    Result = request(ClientAtom,{message_send,Channel,"hi"}),
    assert_error(to_string(ClientAtom)++" writing to "++Channel, Result, user_not_joined).

% Writing when not joined
write_not_joined2_test() ->
    init("write_not_joined2"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client(),
    Result = request(ClientAtom2,{message_send,Channel,"hi"}),
    assert_error(to_string(ClientAtom2)++" writing to "++Channel, Result, user_not_joined).

% Writing when not joined
write_not_joined3_test() ->
    init("write_not_joined3"),
    Channel = new_channel(),

    % Client
    {_Pid, _Nick, ClientAtom} = new_client(),
    Result = request(ClientAtom,{message_send,Channel,"hi"}),
    assert_error(to_string(ClientAtom)++" writing to "++Channel, Result, server_not_reached).

% Leaving when not joined
leave_not_joined_test() ->
    init("leave_not_joined"),
    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client(),
    Result2 = request(ClientAtom2,{leave,Channel}),
    assert_error(to_string(ClientAtom2)++" leaving "++Channel, Result2, user_not_joined).

% Leaving when no server
leave_no_server_test() ->
    init("leave_no_server"),

    {_Pid, _Nick, ClientAtom} = new_client(),
    Channel = "#channel",
    join_channel(ClientAtom, Channel),
    server_kill(),
    leave_channel(ClientAtom, Channel).

% Leaving already leaved
leave_already_leaved_test() ->
    init("leave_already_leaved"),
    Channel = new_channel(),
    {_Pid, _Nick, ClientAtom} = new_client(),
    join_channel(ClientAtom, Channel),

    leave_channel(ClientAtom, Channel),
    Result = request(ClientAtom,{leave,Channel}),
    assert_error(to_string(ClientAtom)++" leaving "++Channel, Result, user_not_joined).

% --- Nick tests -------------------------------------------------------------

nick_change_test() ->
    init("nick_change"++gray(" - Only relevant for the distinction assignment!")),

    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client_gui(),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1 to something unique
    NewNick = find_unique_name("user_"),
    change_nick(ClientAtom1, NewNick),

    % Client 1 writes to channel
    % Make sure prompt in 2 reflects correct name
    Message = find_unique_name("message_"),
    send_message(ClientAtom1, Channel, Message),
    receive_message(Channel, NewNick, Message),

    no_more_messages(),
    ok.

nick_collide_test() ->
    init("nick_collide"++gray(" - Only relevant for the distinction assignment!")),

    Channel = new_channel(),

    % Client 1
    {_Pid1, _Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client(),
    join_channel(ClientAtom2, Channel),

    % Change nick of 1 to 2
    Result = request(ClientAtom1,{nick,Nick2}),
    assert_error(to_string(ClientAtom1)++" changing nick to "++Nick2, Result, nick_taken).

nick_swap_test() ->
    init("nick_swap"++gray(" - Only relevant for the distinction assignment!")),

    Channel = new_channel(),

    % Client 1
    {_Pid1, Nick1, ClientAtom1} = new_client(),
    join_channel(ClientAtom1, Channel),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client(),
    join_channel(ClientAtom2, Channel),

    NewNick = find_unique_name("user_"),
    change_nick(ClientAtom1, NewNick),

    change_nick(ClientAtom2, Nick1),
    change_nick(ClientAtom1, Nick2).

nick_change_hack_test() ->
    init("nick_change_hack"++gray(" - Only relevant for the distinction assignment!")),

    Channel1 = new_channel(),
    Channel2 = new_channel(),

    % Client 1
    {_Pid1, Nick1, ClientAtom1} = new_client_gui(),
    join_channel(ClientAtom1, Channel1),

    % Client 2
    {_Pid2, _Nick2, ClientAtom2} = new_client(),
    join_channel(ClientAtom2, Channel2),

    % Change nick of 1 to something unique
    NewNick = find_unique_name("user_"),
    change_nick(ClientAtom1, NewNick),
    change_nick(ClientAtom2, Nick1),

    Result = request(ClientAtom2,{message_send,Channel1,"rrrrr"}),
    assert_error(to_string(ClientAtom1)++" sends message on "++Channel1, Result, user_not_joined).

% --- Concurrency tests ------------------------------------------------------

-define(CONC_3_CHANS, 3).
-define(CONC_3_USERS, 3). % per channel
-define(CONC_3_MSGS, 2). % per user

% Test flow of many messages going through many channels
%
%    ch1       ch2       ch3
%   / | \     / | \     / | \
% u1 u2 u3  u4 u5 u6  u7 u8 u9
message_throughput_test() ->
    NRecvs = ?CONC_3_CHANS * ?CONC_3_USERS * (?CONC_3_USERS - 1) * ?CONC_3_MSGS, % sent to clients

    _ServerPid = init("message_throughput"),
    ParentPid = self(),
    UsersSeq = lists:seq(1, ?CONC_3_USERS * ?CONC_3_CHANS),
    MsgsSeq  = lists:seq(1, ?CONC_3_MSGS),

    % Connect and join channel
    Fjoin = fun (I) ->
        try
            output_off(),
            Is = lists:flatten(integer_to_list(I)),
            Nick = "user_conc3_"++Is,
            ClientName = "client_conc3_"++Is,
            ClientAtom = list_to_atom(ClientName),
            GUIName = "gui_conc3_"++Is,
            new_gui(GUIName, ParentPid),
            genserver:start(ClientAtom, client:initial_state(Nick, list_to_atom(GUIName), ?SERVERATOM), fun client:handle/2),

            Ch_Ix = (I rem ?CONC_3_CHANS) + 1,
            Ch_Ixs = lists:flatten(io_lib:format("~p", [Ch_Ix])),
            Channel = "#channel_"++Ch_Ixs,
            join_channel(ClientAtom, Channel),
            {ClientAtom,Channel}
        catch Ex ->
            {failed, Ex, I}
        end
    end,

    % Send messages
    Fsend = fun ({ClientAtom,Channel}) ->
        try
            Send = fun (I2) ->
                Is2 = lists:flatten(io_lib:format("~p", [I2])),
                Msg = "message_"++Is2,
                request(ClientAtom, {message_send, Channel, Msg})
            end,
            spawn(fun () ->
                lists:foreach(Send, MsgsSeq)
            end)
        catch Ex ->
            {failed, Ex, {ClientAtom,Channel}} % ignored
        end
    end,
    ClientAtoms = lists:map(Fjoin, UsersSeq),
    MsgX = io_lib:format("spawning ~p channels with ~p clients each", [?CONC_3_CHANS, ?CONC_3_USERS]),
    output_on(),
    assert_no_failures(MsgX, ClientAtoms),

    putStrLn("sending ~p messages per client", [?CONC_3_MSGS]),
    lists:foreach(fun (I) -> spawn(fun() -> Fsend(I) end) end, ClientAtoms),

    % Receive all pending messages
    Recv = fun (Fn, N) ->
        receive
            {message_receive, _Channel, _Nick_Msg} -> Fn(Fn, N+1)
        after
            500 -> N
        end
    end,
    Oks = Recv(Recv, 0),
    Timeouts = NRecvs - Oks,
    putStrLn("messages: ~p successful, ~p timed out, ~p total", [Oks, Timeouts, NRecvs]),
    Cond = (Oks =:= NRecvs),
    Msg = "all messages successful",
    assert(Msg, Cond).

% Looks for {failed, ...} in list
% Expects thrown exception in position 2
assert_no_failures(Msg, List) ->
    case lists:keyfind(failed, 1, List) of
        false -> assert(Msg, true) ;
        Found -> io:fwrite("~s~n", [element(2, Found)]), assert(Msg, false)
    end.

messages_no_server1_test() ->
    init("messages_no_server1"),
    Channel1 = new_channel(),

    % Client 1
    {_Pid1, Nick1, ClientAtom1} = new_client_gui(),
    join_channel(ClientAtom1, Channel1),

    % Client 2
    {_Pid2, Nick2, ClientAtom2} = new_client_gui(),
    join_channel(ClientAtom2, Channel1),

    server_kill(),

    % Client 1 writes to channel
    % Receive from Client 2
    Message1 = find_unique_name("message_"),
    send_message(ClientAtom1, Channel1, Message1),
    receive_message(Channel1, Nick1, Message1),

    % Client 2 writes to channel
    % Receive from Client 1
    Message2 = find_unique_name("message_"),
    send_message(ClientAtom2, Channel1, Message2),
    receive_message(Channel1, Nick2, Message2),

    no_more_messages(),
    ok.

messages_no_server2_test() ->
    init("messages_no_server2"),
    Channel = new_channel(),

    {_Pid, _Nick, ClientAtom} = new_client_gui(),
    join_channel(ClientAtom, Channel),

    server_stop(),

    putStrLn("Wait a few seconds for timeout..."),
    Message = find_unique_name("message_"),
    Result = request(ClientAtom, {message_send,Channel,Message}),
    assert_error(to_string(ClientAtom)++" writing to "++Channel, Result, server_not_reached).
