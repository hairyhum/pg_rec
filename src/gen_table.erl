-module (gen_table).

-include_lib("epgsql/include/pgsql.hrl").
-include("pg_rec.hrl").

-export([behaviour_info/1]).
-export([save/1, save/2, find/1, find/2, findMany/2, findMany/3]).

behaviour_info(callbacks) ->
  [{to_sql_data, 1}, {from_sql_data, 2}, {sql_table, 1}, {sql_id, 1}];
behaviour_info(_) -> 
  undefined.
-type record_spec() :: {atom(), tuple()}.

-spec find(record_spec()) -> tuple() | not_found.
find({_Module, _Record} = RecordSpec) ->
  find_f(RecordSpec, equery()).

-spec find(record_spec(), pid()) -> tuple() | not_found.
find({_Module, _Record} = RecordSpec, Pid) ->
  find_f(RecordSpec, equery(Pid)).

-spec find_f(record_spec(), fun()) -> tuple() | not_found.
find_f({_Module, _Record} = RecordSpec, Equery) -> 
  Data = sql_data(RecordSpec),
  
  Filters = lists:map(fun({K,V}) -> {K, '=', V} end, Data),
  case findMany_f(RecordSpec, Filters, Equery) of 
    [Rec | _] -> Rec;
    [] -> not_found
  end.  

-spec findMany(record_spec(), [filter()]) -> [tuple()].
findMany({_Module, _Record} = RecordSpec, Search) ->
  findMany_f(RecordSpec, Search, equery()).
-spec findMany(record_spec(), [filter()], pid()) -> [tuple()].
findMany({_Module, _Record} = RecordSpec, Search, Pid) ->
  findMany_f(RecordSpec, Search, equery(Pid)).

-spec findMany_f(record_spec(), [filter()], fun()) -> [tuple()].
findMany_f({Module, Record}, Search, Equery) ->
  Plists = pg_rec_sql:select(Module:sql_table(Record), Search, Equery),
  lists:map(
    fun(Plist) ->
        Module:from_sql_data(Record, Plist)
    end,
    Plists).

-spec save(record_spec(), pid()) -> {ok, integer()}.
save({_Module, _Record} = RecordSpec, Pid) ->
  save_f(RecordSpec, equery(Pid)).

-spec save(record_spec()) -> {ok, integer()}.
save({_Module, _Record} = RecordSpec) ->
  save_f(RecordSpec, equery()).

-spec save_f(record_spec(), fun()) -> {ok, integer()}.
save_f({Module, Record} = RecordSpec, Equery) when is_function(Equery)  ->
  SqlData = sql_data(RecordSpec),
  Id = Module:sql_id(Record),
  Table = Module:sql_table(Record),
  case pg_rec_sql:exist(Id, Table, Equery) of 
    true -> pg_rec_sql:updateById(Table, Id, SqlData, Equery);
    false -> pg_rec_sql:insert(Table, SqlData, Equery)
  end.

sql_data({Module, Record}) -> 
  lists:filter(
      fun({_,undefined}) -> false; 
        (_) -> true end, 
      Module:to_sql_data(Record)).

-spec equery(pid()) -> fun((tuple(), list()) -> pg_connection:equery_result()).
equery(Pid) ->
  fun(Q,P) -> pg_connection:safe_equery(Q,P,Pid) end.
-spec equery() -> fun().
equery() -> 
  fun pg_connection:safe_equery/2.
