-module(pg_provider).
-behaviour(gen_db_provider).

-export([save/2, find/2, find_many/2, delete/2, delete_many/2]).

equery() ->
  fun db_pool:safe_equery/2.
equery(undefined) -> equery();
equery(Pid) ->
  fun(Query, Params) -> 
    pg_connection:safe_equery(Pid, Query, Params) 
  end.

save(undefined, {Table, Id, Data, Index}) ->
  db_pool:with_transaction(
    fun(Pid) -> 
      save(Pid, {Table, Id, Data, Index})
    end);
save(Pid, {Table, Id, Data, Index}) ->
  FullData = remove_duplicates(Data ++ Index),
  Equery = equery(Pid),
  case pg_rec_sql:exist(Id, Table, equery(Pid)) of 
    true -> pg_rec_sql:updateById(Table, Id, FullData, Equery);
    false -> pg_rec_sql:insert(Table, FullData, Equery) 
  end.


find(Pid, {Table, Id, Index}) ->
  Equery = equery(Pid),
  Search = prepare_search(Index),
  case Id of 
    {_, undefined} -> pg_rec_sql:select(Table, Search, 1, Equery);
    {IdKey, IdVal} ->  pg_rec_sql:select(Table, [{IdKey, '=', IdVal}], 1, Equery)
  end.

find_many(Pid, {Table, Index}) ->
  Equery = equery(Pid),
  Search = prepare_search(Index),
  pg_rec_sql:select(Table, Search, Equery).

delete(undefined, {Table, Id}) ->
  db_pool:with_transaction(
    fun(Pid) -> 
      delete(Pid, {Table, Id})
    end);
delete(Pid, {Table, Id}) ->
  Equery = equery(Pid),
  case pg_rec_sql:exist(Id, Table, Equery)  of
    {ok, _} -> pg_rec_sql:delete_by_id(Table, Id, Equery);
    _ -> not_found
  end.

delete_many(undefined, {Table, Index}) ->
  db_pool:with_transaction(
    fun(Pid) ->
      delete_many(Pid, {Table, Index})
    end);
delete_many(Pid, {Table, Index}) ->
  Equery = equery(Pid),
  Search = prepare_search(Index),
  pg_rec_sql:delete_by_index(Table, Search, Equery).


prepare_search(Index) ->
  [{Key, '=', Val} || {Key, Val} <- Index].

remove_duplicates(Proplist) ->
  [{Key, proplists:get_value(Key, Proplist)} || Key <- proplists:get_keys(Proplist)].