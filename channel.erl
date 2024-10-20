-module(channel).
-export([start/1,stop/1]).

-record(channel_st, {
    name,   
    members 
}).

initial_state(Name) ->
    #channel_st{
        name = Name,
        members = []
    }.
% Client request to join
handle(St, {join, Pid}) ->
    case lists:member(Pid, St#channel_st.members) of
        true ->
            {reply, {error, user_already_joined, "Already joined "++St#channel_st.name}, St};
        false ->
            TempList = [Pid | St#channel_st.members],
            {reply, ok, St#channel_st{members=TempList}}
        end;

% Client request to leave
handle(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.members) of
        true ->
            TempList = lists:delete(Pid, St#channel_st.members),
            {reply, ok, St#channel_st{members = TempList}};
        false ->
            {reply, {error, not_a_member, "Client not a member of " ++ St#channel_st.name}, St}
    end; 

handle(St, {message_send, ClientPid, ClientNick, Msg}) ->
    OtherMembers = lists:delete(ClientPid, St#channel_st.members),
    Data = {request, self(), make_ref(), {message_receive, St#channel_st.name, ClientNick, Msg}},
    lists:foreach((fun(Member) -> Member ! Data end), OtherMembers),
    {reply, ok, St};

handle(St, _) ->
    {reply, {error, not_implemented, "Channel cannot handle this request!"}, St} .


start(Name) ->
    genserver:start(list_to_atom(Name), initial_state(Name), fun handle/2).

stop(Name) ->
    genserver:stop(list_to_atom(Name)).