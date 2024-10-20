-module(tui).
-export([start/1, init/1, terminate/2, handle_info/2, handle_call/3, handle_cast/2]).

-behavior(gen_server).

-define(VERSION, "2.0").
-define(MAX_CONNECTIONS, 100000).

-record(state, {
    tui_inbox,       % tui process
    client_name,    % repl process
    nick,           % client nick
    channels        % subcribed channels
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REPL

start(ChatServerName) ->

    % Create unique names
    {ClientName, _} = find_unique_name("client_", ?MAX_CONNECTIONS),
    {TUIInbox,    _} = find_unique_name("tui_", ?MAX_CONNECTIONS),

    io:format(
        "Welcome to CCHAT v~s\n" ++
        "* Server name is: ~s\n" ++
        "* Your nick is: ~s\n",
        [?VERSION, ChatServerName, ClientName]
    ),

    start_link(TUIInbox),
    register(list_to_atom(ClientName), self()),

    genserver:start(
        list_to_atom(ClientName),
        client:initial_state(ClientName, list_to_atom(TUIInbox), ChatServerName),
        fun client:handle/2
    ),

    loop(#state{ tui_inbox = TUIInbox, client_name = ClientName, nick = ClientName, channels = []}).


loop(St = #state{ channels = Channels }) ->

    Prompt = string:join(lists:reverse(Channels), "|"),
    Line = io:get_line(Prompt ++ "> "),
    LineWithoutNL = string:strip(string:strip(Line, both, 13), both, 10),

    Cmd = lexgrm:parse_cmd(LineWithoutNL),

    case catch(handle_cmd(St, Cmd)) of
        {'EXIT', user_quit} ->
            io:format("Bye!~n"),
            ok;
        NewSt ->
            loop(NewSt)
    end.


handle_cmd(St = #state{ tui_inbox = TUIInbox, client_name = ClientName, nick = Nick, channels = Channels}, Cmd) ->
    case Cmd of

        % Joining a new channel
        {join, Channel} ->
            Result = catch_fatal(fun () -> request(ClientName, {join, Channel}) end),
            case Result of
                ok ->
                    io:format("* Joined ~s~n", [Channel]),
                    NewChannels = [ Channel | Channels ],
                    St#state{ channels = NewChannels };
                error ->
                    St
            end ;

        % /leave
        leave ->
            case Channels of
                [] ->
                    St;
                [ Curr | _ ] ->
                    handle_cmd(St, {leave, Curr})
            end;

        % /leave #channel
        {leave, Channel} ->
            Result = catch_fatal(fun () -> request(ClientName, {leave, Channel}) end),
            case Result of
                ok ->
                    io:format("* Left ~s~n", [Channel]),
                    NewChannels = [ Ch || Ch <- Channels, Ch /= Channel ],
                    St#state{ channels = NewChannels };
                error ->
                    St
            end;

        % /quit
        quit ->
            % Client process
            request(ClientName, quit),
            list_to_atom(ClientName) ! stop,

            % TUI
            catch gen_server:call(list_to_atom(TUIInbox), shutdown, 0),

            exit(user_quit);

        % Sending a message
        {msg, String} when String == [] ->
            case Channels of
                [] ->
                    St;
                [ Curr | Rest ] ->
                    NewChannels = Rest ++ [Curr],
                    St#state{ channels = NewChannels }
            end;

        {msg, String} ->
            case Channels of
                [] ->
                    io:format("* You have no active channels~n"),
                    St;
                [ Curr | _ ] ->
                    Result = catch_fatal(fun () -> request(ClientName, {message_send, Curr, String}) end),
                    case Result of
                        ok ->
                            channel_msg(Curr, Nick ++ "> " ++ String),
                            St;
                        error ->
                            St
                    end
            end;

        % Who am I?
        whoami ->
            Result = catch_fatal(fun () -> request(ClientName, whoami) end),
            case Result of
                error ->
                    St;
                SomeNick ->
                    io:format("* You are ~p\n", [SomeNick]),
                    St#state{ nick = SomeNick }
            end;

        % Change nickname
        {nick, SomeNick} ->
            Result = catch_fatal(fun () -> request(ClientName, {nick, SomeNick}) end ),
            case Result of
                ok ->
                    io:format("* You are now known as ~p~n", [SomeNick]),
                    St#state { nick = SomeNick };
                error ->
                    St
            end;

        % The given command was wrong
        {ignore, BadLine} ->
            io:format("* Command not recognized: ~p~n", [BadLine]),
            St
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gen_server behavior

start_link(TUIInbox) ->
    gen_server:start_link({local, list_to_atom(TUIInbox)}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call(Request, _From, State) ->
    case Request of
        {message_receive, Channel, Msg} ->
            channel_msg(Channel, Msg),
            {reply, ok, State};
        shutdown ->
           {stop, normal, ok, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliary functions

channel_msg(Channel, String) ->
    io:format("[~s] ~s~n", [Channel, String]).

% Finding an unique name
find_unique_name(Prefix,N) ->
    Num = rand:uniform(N),
    MStr = integer_to_list(Num),
    Name = Prefix++MStr,
    case whereis(list_to_atom(Name)) of
        undefined -> {Name, Num} ;
        _         -> find_unique_name(Prefix,N)
    end.

% Requests
request(ClientName, Msg) ->
    genserver:request(list_to_atom(ClientName), Msg, 100000). % must be greater than default timeout

% Errors
catch_fatal(F) ->
    case catch( F() ) of
        {'EXIT', Reason} ->
            exit("Fatal error: " ++ Reason);
        {error, _, Msg} ->
            io:format("Error: ~p~n", [Msg]),
            error;
        Result ->
            Result
    end.

% % Debugging
% trace(Args) ->
%     io:format("~s"++lists:flatten(lists:duplicate(length(Args)-1,"~p~n")),Args).