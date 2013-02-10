
-module(pg_rec_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
  % Connections = proplists:get_value(pg_connections, Args),
  DbParams =  proplists:get_value(db, Args),
  PgConnection =
    {pg_connection, {pg_connection, start_link, [DbParams]},
      permanent, 5000, worker, [pg_connection]},
  {ok, { {one_for_one, 5, 10}, [PgConnection]} }.

