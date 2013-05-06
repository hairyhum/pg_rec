-module(gen_storage).

-export([behaviour_info/1]).
-export([
  save/1, save/2, 
  find/1, find/2, 
  find_many/1, find_many/2, find_many/3, 
  delete/1, delete/2, 
  delete_many/1, delete_many/2, delete_many/3
  ]).

-type pair() :: {atom(), any()}.
-type filter() :: {atom(), atom(), any()}.
-type plist() :: [pair()].
-type record_spec() :: {atom(), tuple()}.


behaviour_info(callbacks) ->
  [{to_data, 1}, {from_data, 2}, {get_index, 1}, {provider, 1}, {get_table, 1}, {get_id, 1}];
behaviour_info(_) -> 
  undefined.

save({_Module, _Record} = RecordSpec) ->
  save(RecordSpec, undefined).

save({Module, Record} = _RecordSpec, Pid) ->
  Table = Module:get_table(Record),
  Id = Module:get_id(Record),
  Index = Module:get_index(Record),
  Data = Module:to_data(Record),
  Provider = Module:provider(Record),
  Provider:save(Pid, {Table, Id, Data, Index}).

find({_Module, _Record} = RecordSpec) ->
  find(RecordSpec, undefined).

find({Module, Record} = _RecordSpec, Pid) ->
  Table = Module:get_table(Record),
  Provider = Module:provider(Record),
  Id = Module:get_id(Record),
  Index = Module:get_index(Record),
  Result = Provider:find(Pid, {Table, Id, Index}),
  case Result of 
    {ok, []} -> not_found;
    {ok, Data} -> Module:from_data(Record, Data);
    Err -> Err
  end.
  
find_many({_Module, _Record} = RecordSpec) ->
  find_many(RecordSpec, []).
find_many({_Module, _Record} = RecordSpec, Search) ->
  find_many(RecordSpec, Search, undefined).

find_many({Module, Record} = _RecordSpec, Search, Pid) ->
  Table = Module:get_table(Record),
  Provider = Module:provider(Record),
  Index = Module:get_index(Record) ++ Search,
  Result = Provider:find_many(Pid, {Table, Index}),
  case Result of 
    {ok, Data} -> lists:map(fun(El) -> Module:from_data(Record, El) end, Data);
    Err -> Err
  end.


delete({_Module, _Record} = RecordSpec) -> 
  delete(RecordSpec, undefined).

delete({Module, Record} = _RecordSpec, Pid) ->
  Table = Module:get_table(Record),
  Provider = Module:provider(Record),
  Id = Module:get_id(Record),
  Provider:delete(Pid, {Table, Id}).

delete_many({_Module, _Record} = RecordSpec) ->
  delete_many(RecordSpec, []).
delete_many({_Module, _Record} = RecordSpec, Search) ->
  delete_many(RecordSpec, Search, undefined).
delete_many({Module, Record} = _RecordSpec, Search, Pid) ->
  Table = Module:get_table(Record),
  Provider = Module:provider(Record),
  Index = Module:get_index(Record) ++ Search,
  Provider:delete_many(Pid, {Table, Index}).




