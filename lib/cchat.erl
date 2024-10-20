-module(cchat).
-export([server/0,client/0,client_tui/0]).
-define(SERVERNAME,shire).

% Start a server
server() ->
    server:start(?SERVERNAME).

% Start a client GUI
client() ->
    gui:start(?SERVERNAME).

% Start a client TUI
client_tui() ->
    tui:start(?SERVERNAME).
