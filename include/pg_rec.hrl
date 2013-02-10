-type sql_result() :: {ok, [#column{}], [tuple()]} | {ok, integer()} | {ok, integer(), [#column{}], [tuple()]} | {error, any()}.
-type pair() :: {atom(), any()}.
-type filter() :: {atom(), atom(), any()}.
-type plist() :: [pair()].


