-module(server).
-export([start/1, stop/1]).

-record(serverState, {
    users,
    channels
}).

initial_state() ->
    #serverState{
        users = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    catch genserver:start(ServerAtom, initial_state(), fun handle/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:request(ServerAtom, delete_all_channels),
    genserver:stop(ServerAtom).

handle(State, {join, Pid, NewUserName, Channel}) ->
        NewNicksList = case lists:member(NewUserName, State#serverState.users) of
            true -> State#serverState.users;
            false -> [NewUserName | State#serverState.users]
        end,
        NewChannelsList = case lists:member(Channel, State#serverState.channels) of
            true -> State#serverState.channels;
            false -> 
                channel:start(Channel), 
                [Channel | State#serverState.channels]
        end,
        try 
            ChannelResponse = genserver:request(list_to_atom(Channel), {join, Pid}),
            {reply, ChannelResponse, State#serverState{users= NewNicksList, channels=NewChannelsList}}
        catch
            _:_Error -> {reply, {error, join_failed, "Failed to join channel " ++ Channel}, State}
        end;    


handle(State, {quit, UserName}) ->
    TempList = lists:delete(UserName, State#serverState.users),
    {reply, ok, State#serverState{users = TempList}}; 

%function that handles changing NickName 
handle(State,{nick, OldUserName, NewUserName})-> 
    case lists:member(NewUserName, State#serverState.users) of
        true when OldUserName =:= NewUserName ->
            {reply, ok, State};
        true ->
            io:format("Hello, world!~n"),
            {reply, {error, nick_taken, "Username "++NewUserName++" has been taken!"}, State};
        false ->
            TempList = [NewUserName | lists:delete(OldUserName, State#serverState.users)],
            {reply, ok, State#serverState{users=TempList}}
        end;


handle(State, delete_all_channels) ->
lists:foreach(
fun(Ch) -> genserver:stop(list_to_atom(Ch)) end, State#serverState.channels
),

{reply,ok, State#serverState{users=[], channels = []}};

handle(State, {message_send, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(Channel, State#serverState.channels) of
        true ->
            try
                ChannelResponse = genserver:request(ChannelAtom, {message_send, self(), Msg}),
                {reply, ChannelResponse, State}
            catch
                _:_Error -> {reply, {error, server_not_reached, "Failed to send message to channel " ++ Channel}, State}
            end;
        false ->
            {reply, {error, channel_not_exists, "Channel " ++ Channel ++ " does not exist"}, State}
        end; 

%ToDo catch other commands with some type of exeption
handle(State, _ ) ->
    {reply, {error, not_implemented}, State}.