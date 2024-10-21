-module(server).
-export([start/1, stop/1]).

-record(serverState, {
    users,       % List of users registered on the server
    channels     % List of channels available on the server
}).

% Initializes the server state with empty user and channel lists
initial_state() ->
    #serverState{
        users = [],
        channels = []
    }.

% Start a new server process with the given name
start(ServerAtom) ->
    catch genserver:start(ServerAtom, initial_state(), fun handle/2).

% Stop the server process registered to the given name with any other associated processes (when we stop server)
stop(ServerAtom) ->
    genserver:request(ServerAtom, delete_all_channels),  % Request to delete all channels
    genserver:stop(ServerAtom).                          % Stop the server process

% Handle the user joining a channel
handle(State, {join, Pid, NewUserName, Channel}) ->
        % Update the user list if the new username isn't already present
        NewNicksList = case lists:member(NewUserName, State#serverState.users) of
            true -> State#serverState.users;  % Keep the list unchanged
            false -> [NewUserName | State#serverState.users]  % Add new username
        end,
        % Add the channel to the list if it's not already present
        NewChannelsList = case lists:member(Channel, State#serverState.channels) of
            true -> State#serverState.channels;  % Keep the list unchanged
            false -> 
                channel:start(Channel),  % Start the new channel
                [Channel | State#serverState.channels]  % Add the new channel
        end,
        % Try sending a join request to the channel
        try 
            ChannelResponse = genserver:request(list_to_atom(Channel), {join, Pid}),
            {reply, ChannelResponse, State#serverState{users= NewNicksList, channels=NewChannelsList}}
        catch
            _:_Error -> {reply, {error, join_failed, "Failed to join channel " ++ Channel}, State}  % Handle errors
        end;    

% Handle the user quitting the server
handle(State, {quit, UserName}) ->
    TempList = lists:delete(UserName, State#serverState.users),  % Remove the user from the list
    {reply, ok, State#serverState{users = TempList}};  % Reply with the updated state

% Handle changing a user's nickname
handle(State, {nick, OldUserName, NewUserName}) -> 
    case lists:member(NewUserName, State#serverState.users) of
        true when OldUserName =:= NewUserName ->  % No change in nickname
            {reply, ok, State};  % Reply with the current state
        true ->  % New username already taken
            io:format("Hello, world!~n"),  % Debugging message
            {reply, {error, nick_taken, "Username " ++ NewUserName ++ " has been taken!"}, State};  % Error reply
        false ->  % New username is available
            TempList = [NewUserName | lists:delete(OldUserName, State#serverState.users)],  % Update user list
            {reply, ok, State#serverState{users = TempList}}  % Reply with the updated state
    end;

% Handle the delete of all channels
handle(State, delete_all_channels) ->
    lists:foreach(
        fun(Ch) -> genserver:stop(list_to_atom(Ch)) end,  % Stop each channel process
        State#serverState.channels
    ),
    {reply, ok, State#serverState{users = [], channels = []}};  % Clear the state and reply

% Handle sending a message to a channel
handle(State, {message_send, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),  % Convert channel name to atom to be able to check if it exists or not
    case lists:member(Channel, State#serverState.channels) of
        true ->  % Channel exists
            try
                ChannelResponse = genserver:request(ChannelAtom, {message_send, self(), Msg}),  % Send the message
                {reply, ChannelResponse, State}  % Reply with the response
            catch
                _:_Error -> {reply, {error, server_not_reached, "Failed to send message to channel " ++ Channel}, State}  % Handle errors
            end;
        false ->  % Channel does not exist
            {reply, {error, channel_not_exists, "Channel " ++ Channel ++ " does not exist"}, State}  % Error reply
    end;

% Catch-all handler for unimplemented commands
handle(State, _) ->
    {reply, {error, not_implemented}, State}.  % Reply with an error message
