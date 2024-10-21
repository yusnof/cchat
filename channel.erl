-module(channel).
-export([start/1,stop/1]).

% Record definition for the channel state.
% Fields:
%   - name: The name of the channel.
%   - members: A list of PIDs representing the members of the channel.
-record(channel_st, {
    name,   
    members 
}).



start(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

% Stop a channel process by its name.
% Converts the channel name to an atom and stops the genserver.
stop(Name) ->
    genserver:stop(list_to_atom(Name)).


% Initialize the channel state with a given name.
% Arguments:
%   - Name: The name of the channel.
% Returns a new channel state record with an empty member list.
initial_state(Name) ->
    #channel_st{
        name = Name,
        members = []
    }.
% Handle client request to join the channel.
% Arguments:
%   - St: Current channel state.
%   - {join, Pid}: A tuple indicating a join request, where Pid is the client's process ID.
% If the client is already a member, returns an error. Otherwise, adds the client to the member list.
handle(St, {join, Pid}) ->
    case lists:member(Pid, St#channel_st.members) of
        true ->
            {reply, {error, user_already_joined, "Already joined "++St#channel_st.name}, St};
        false ->
            TempList = [Pid | St#channel_st.members],
            {reply, ok, St#channel_st{members=TempList}}
        end;

% Handle client request to leave the channel.
% Arguments:
%   - St: Current channel state.
%   - {leave, Pid}: A tuple indicating a leave request, where Pid is the client's process ID.
% If the client is a member, they are removed from the list. If not, returns an error.
handle(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.members) of
        true ->
            TempList = lists:delete(Pid, St#channel_st.members),
            {reply, ok, St#channel_st{members = TempList}};
        false ->
            {reply, {error, not_a_member, "Client not a member of " ++ St#channel_st.name}, St}
    end; 

% Handle message send request from a client.
% Arguments:
%   - St: Current channel state.
%   - {message_send, ClientPid, ClientNick, Msg}: A tuple representing the message send request.
%     ClientPid: The PID of the client sending the message.
%     ClientNick: The nickname of the client.
%     Msg: The message content.
% The message is sent to all other members of the channel except the sender.
handle(St, {message_send, ClientPid, ClientNick, Msg}) ->
    OtherMembers = lists:delete(ClientPid, St#channel_st.members),
    Data = {request, self(), make_ref(), {message_receive, St#channel_st.name, ClientNick, Msg}},
    lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
    {reply, ok, St};

handle(St, _) ->
    {reply, {error, not_implemented, "Channel cannot handle this request!"}, St} .

