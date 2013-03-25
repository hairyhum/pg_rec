-module (converter).
-include_lib("epgsql/include/pgsql.hrl").

-export ([sql_to_plist/2]).

-spec sql_to_plist([#column{}], tuple()) -> [{atom(), any()}].
sql_to_plist(Columns, Row) when is_tuple(Row) ->
  RowList = tuple_to_list(Row),
  lists:zipwith(
    fun(#column{name = Name}, Value) ->
        {binary_to_atom(Name, unicode), Value}
    end,
    Columns,
    RowList).
