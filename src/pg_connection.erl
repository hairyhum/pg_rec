-module (pg_connection).
%% Copyright
-behaviour(gen_server).
-include_lib("epgsql/include/pgsql.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
-export([safe_equery/3, bin_equery/3, safe_equery/2, bin_equery/2]).

-type equery_result() :: 
  {error, #error{}} | 
  {ok, integer()} | 
  {ok, [#column{}], [tuple()]} | 
  {ok, integer(), [#column{}], [tuple()]}.

-export_type([equery_result/0]).

-record(state, { pid::pid() }).

-spec safe_equery(pid(), tuple()) -> equery_result().
safe_equery(Pid, Query) -> safe_equery(Pid, Query, []).

-spec bin_equery(pid(), binary()) -> equery_result().
bin_equery(Pid, Query) -> bin_equery(Pid, Query, []).

-spec safe_equery(pid(), tuple(), list()) -> equery_result().
safe_equery(Pid, Query, Params) when is_pid(Pid), is_tuple(Query), is_list(Params) ->
  BinQuery = sqerl:sql(Query, true),
  pgsql:equery(Pid, BinQuery, Params).

-spec bin_equery(pid(), binary(), list()) -> equery_result().
bin_equery(Pid, Query, Params) when is_pid(Pid), is_binary(Query), is_list(Params) ->
  pgsql:equery(Pid, Query, Params).   


start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  Host = proplists:get_value(host, Args, "localhost"),
  Port = proplists:get_value(port, Args, 5432),
  Db = proplists:get_value(database, Args, "postgres"),
  User = proplists:get_value(user, Args, "postgres"),
  Pass = proplists:get_value(password, Args, "postgres"),     
  case pgsql:connect(Host, [User], [Pass], [{database, Db}, {port, Port}]) of
    {ok, Pid} ->
      {ok, #state{ pid = Pid }};
    {error, Error} ->
      {stop, "DB-server is shutdown with error: ~p~n", [Error]}
  end.

handle_call({safe_equery, {Query, Params}}, _, #state{ pid = Pid } = State) ->
  Res = safe_equery(Pid, Query,Params),
  {reply, Res, State};
handle_call({bin_equery, {Query, Params}}, _, #state{ pid = Pid } = State) ->
  Res = bin_equery(Pid, Query,Params),
  {reply, Res, State};
handle_call({with_transaction, Fun}, _, #state{ pid = Pid } = State) ->
  Res = pgsql:with_transaction(Pid, Fun),
  {reply, Res, State}. 

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
