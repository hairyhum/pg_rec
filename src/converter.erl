-module (converter).
-include_lib("epgsql/include/pgsql.hrl").

-export ([sql_to_plist/2, sql_to_rec/3]).

-spec sql_to_plist([#column{}], tuple()) -> [{atom(), any()}].
sql_to_plist(Columns, Row) when is_tuple(Row) ->
  RowList = tuple_to_list(Row),
  lists:zipwith(
    fun(#column{name = Name}, Value) ->
        {binary_to_atom(Name, unicode), Value}
    end,
    Columns,
    RowList).

-spec sql_to_rec([#column{}], tuple(), atom()) -> tuple().
sql_to_rec(Columns, Row, Module) when is_tuple(Row) ->
  Module:from_sql_data(sql_to_plist(Columns, Row));
sql_to_rec(Columns, Rows, Module) when is_list(Rows) ->
  lists:map(
    fun (Row) ->
        sql_to_rec(Columns, Row, Module)
    end,
    Rows).