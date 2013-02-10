-module(pg_rec_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
    pg_rec_sup:start_link(StartArgs).

stop(_State) ->
    ok.
