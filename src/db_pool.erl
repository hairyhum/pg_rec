-module (db_pool).

%% API Gen_server
-export([get_pool/2]).

%% API
-export([safe_equery/1, safe_equery/2, bin_equery/1, bin_equery/2, with_transaction/1]).

-define (POOL_NAME, pg_pool).

get_pool(SizeArgs, WorkerArgs) -> 
  PoolArgs = [{name, {local, ?POOL_NAME}},
              {worker_module, pg_connection}] ++ SizeArgs, 
  poolboy:child_spec(?POOL_NAME, PoolArgs, WorkerArgs).

call_in_pool(Args) ->
  poolboy:transaction(?POOL_NAME, 
    fun(Worker) ->
      gen_server:call(Worker, Args)
    end). 

-spec safe_equery(tuple()) -> pg_connection:equery_result().
safe_equery(Query) ->
  safe_equery(Query, []).
-spec safe_equery(tuple(), list()) -> pg_connection:equery_result().
safe_equery(Query, Params) ->
  call_in_pool({safe_equery, {Query, Params}}).

-spec bin_equery(tuple()) -> pg_connection:equery_result().
bin_equery(Query) ->
  bin_equery(Query, []).
-spec bin_equery(tuple(), list()) -> pg_connection:equery_result().
bin_equery(Query, Params) ->
  call_in_pool({bin_equery, {Query, Params}}).

-spec with_transaction(fun()) -> pg_connection:equery_result().
with_transaction(Fun) ->
  call_in_pool({with_transaction, Fun}).


