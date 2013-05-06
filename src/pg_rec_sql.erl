-module (pg_rec_sql).

-include_lib("epgsql/include/pgsql.hrl").
-include("pg_rec.hrl").

-export ([select/3, select/4, insert/3, exist/3, updateById/4]).

select(Table, Search, Equery) -> select(Table, Search, all, Equery).

-spec select(atom(), [filter()], all | integer(),  fun()) -> [plist()].
select(Table, Search, Limit, Equery) ->
  {Filters, Params} = make_quoted_filters(Search),
  Where = case Filters of
    [] -> {true, '=', true};
    Filters -> {'and', Filters}
  end,
  Query = {select, '*', {from, Table}, {where, Where}, {limit, Limit}},
  {ok, Columns, Rows} = Equery(Query, Params),
  {ok, rows_to_plists(Columns, Rows)}.

-spec insert(atom(), plist(), fun()) -> {ok, integer()}. 
insert(Table, Data, Equery) ->
  {Values, Params} = make_quoted_values(Data),
  Query = {insert, Table, Values},
  {ok, 1} = Equery(Query, Params).

-spec updateById(atom(), pair(), plist(), fun()) -> {ok, integer()}.
updateById(Table, {IdKey, IdVal}, Data, Equery) ->
  DataWithNoId = proplists:delete(IdKey, Data),
  {Values, Params} = make_quoted_values(DataWithNoId, 2),
  Query = {update, Table, Values, {where, {IdKey, '=', '$1'}}},
  {ok, 1} = Equery(Query, [IdVal | Params]).

-spec exist(pair(), atom(), fun()) -> boolean().
exist({_, undefined}, _, _) -> false;
exist({IdKey, IdVal}, Table, Equery) when is_atom(IdKey), is_function(Equery) ->
  GetQuery = {select, {call,count,[id]}, {from, Table}, {where, {IdKey, '=', '$1'}}},
  {ok, _Columns, [{Num}]} = Equery(GetQuery, [IdVal]),
  true = is_number(Num),
  Num > 0.

-spec make_quoted_values(plist()) -> {[{atom(), atom()}], [any()]}.
make_quoted_values(Plist) ->
  make_quoted_values(Plist, 1).
   
-spec make_quoted_values(plist(), integer()) -> {[{atom(), atom()}], [any()]}.
make_quoted_values(Plist, StartNum) ->
  FilteredPlist = [{Key,Val} || {Key,Val} <- Plist, Val =/= undefined],
  {Zipped, _} = lists:mapfoldl(
      fun({Key,Val}, Index) ->
          Res = {{Key, placeholder(Index)}, Val},
          {Res, Index+1}
      end,
      StartNum,
      FilteredPlist),
  lists:unzip(Zipped).

-spec make_quoted_filters([filter()]) -> {[{atom(), atom(), atom()}], [any()]}.
make_quoted_filters(Search) ->
  FilteredSearch = [ {Key, Qual, Val} || {Key, Qual, Val} <- Search, Val =/= undefined],
  {Filters, _} = lists:mapfoldl(
    fun({Key, Qual, Val}, Index) ->  
      Res = {{Key, Qual, placeholder(Index)}, Val},
      {Res, Index + 1}
    end, 
    1,
    FilteredSearch),
  lists:unzip(Filters).

-spec placeholder(integer()) -> atom().
placeholder(Num) when is_integer(Num) ->
  Str = "$" ++ integer_to_list(Num),
  list_to_atom(Str). 

-spec row_to_plist([#column{}], tuple()) -> plist().
row_to_plist(Columns, Row) when is_tuple(Row) ->
  RowList = tuple_to_list(Row),
  lists:zipwith(
    fun(#column{name = Name}, Value) ->
        {binary_to_atom(Name, unicode), Value}
    end,
    Columns,
    RowList).

-spec rows_to_plists([#column{}], [tuple()]) -> [plist()].
rows_to_plists(Columns, Rows) when is_list(Rows)->
  lists:map(
    fun(Row) -> row_to_plist(Columns, Row) end,
    Rows).
