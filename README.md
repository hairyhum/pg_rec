# Usage 

## Record-table behaviour

Implement module with behaviour gen_storage
There should be functions:
* to_data/1 - get property list ([{atom(), any()}]) with DB row data
* from_data/2 - get record from initial record and property list
* get_table/1 - get name of DB table (as atom) from record
* get_id/1 - get name and value of ID column from record ({atom(), any()})
* get_index/1 - get index columns (used for search) from record ({atom(), any()})
* provider/1 - get name (atom) of data provider used for storing data. There is only pg_provider available for the moment


Then you can use functions from gen_storage:
* find/1 :: find({Record, Module}) -> Record - find record matching Record spec 
* save/1 :: save({Record, Module}) -> {ok, integer()} - save Record
* find_many/1 :: find({Record, Module}) -> [Record]
* find_many/2 :: find({Record, Module}, [{key, Value} = Filter]) -> [Record] - find all records matching Record spec and filter conditions
* delete/1 :: delete({Record, Module}) -> ok | error - delete record from DB by Id
* delete_many/2 :: delete({Record, Module}, [Filter]) -> ok | error - delete all records by filter

And same with additional parameter Pid as the Pid of epgsql connection


Postgres provider uses poolboy and epgsql. Settings for provider in app env

```erlang
  {db_pool, [
      {size, 10},
      {max_overflow, 20}
      ]},
  {db, [
      {host, "<host>"},
      {port, <port>},
      {user, "<username>"},
      {password, "<password>"},
      {database, "<database>"}
    ]}
```
