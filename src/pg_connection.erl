-module (pg_connection).
%% Copyright
-behaviour(gen_server).
-include_lib("epgsql/include/pgsql.hrl").

-define(SERVER, ?MODULE).
-define(PG_HOST, "localhost").
-define(PG_PORT, 5432).
-define(PG_USER, "postgres").
-define(PG_PASS, "danniill").
-define(PG_DB, "baton").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([safe_equery/1, safe_equery/2, with_transaction/1]).

-type equery_result() :: 
  {error, #error{}} | 
  {ok, integer()} | 
  {ok, [#column{}], [tuple()]} | 
  {ok, integer(), [#column{}], [tuple()]}.

-export_type([equery_result/0]).

-spec safe_equery(tuple()) -> equery_result().
safe_equery(Query) when is_tuple(Query) ->
  safe_equery(Query, []).

-spec safe_equery(tuple(), list()) -> equery_result().
safe_equery(Query, Params) when is_tuple(Query), is_list(Params) ->
  gen_server:call(?SERVER, {safe_equery, {Query, Params}}).

-spec safe_equery(tuple(), list(), pid()) -> equery_result().
safe_equery(Query, Params, Pid) when is_tuple(Query), is_list(Params) ->
  BinQuery = sqerl:sql(Query, true),
  pgsql:equery(Pid, BinQuery, Params).

with_transaction(Fun) ->
  gen_server:call(?SERVER, {with_transaction, Fun}). 

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

init(Args) ->
  Host = proplists:get_value(host, Args, "localhost"),
  Port = proplists:get_value(port, Args, 5432),
  Db = proplists:get_value(database, Args, "postgres"),
  User = proplists:get_value(user, Args, "postgres"),
  Pass = proplists:get_value(password, Args, "postgres"),     
  case pgsql:connect(Host, [User], [Pass], [{database, Db}, {port, Port}]) of
    {ok, Pid} ->
      {ok, Pid};
    {error, Error} ->
      {stop, "DB-server is shutdown with error: ~p~n", [Error]}
  end.

handle_call({safe_equery, {Query, Params}}, _, Pid) ->
  Res = safe_equery(Query,Params,Pid),
  {reply, Res, Pid};
handle_call({with_transaction, Fun}, _, Pid) ->
  Res = pgsql:with_transaction(Pid, Fun),
  {reply, Res, Pid}. 

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
