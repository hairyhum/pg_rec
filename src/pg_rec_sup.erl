
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
  DbParams =  proplists:get_value(db, Args),
  PoolParams = proplists:get_value(db_pool, Args), 
  PgPool = db_pool:get_pool(PoolParams, DbParams), 
  {ok, { {one_for_one, 5, 10}, [PgPool]} }.

