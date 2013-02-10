# Usage 

## Record-table behaviour

Implement module with behaviour gen_table
There should be functions:
* to_sql_data/1 - get property list ([{atom(), any()}]) with DB row data
* from_sql_data/2 - get record from initial record and property list
* sql_table/1 - get name of DB table (as atom) from record
* sql_id/1 - get name and value of ID column from record ({atom(), any()})


Then you can use functions from gen_table:
* find/1 :: find({Record, Module}) -> Record - find record matching Record spec 
* save/1 :: save({Record, Module}) -> {ok, integer()} - save Record
* findMany/1 :: find({Record, Module}) -> [Record] - find all records matching Record spec 

And same with additional parameter Pid as the Pid of epgsql connection


Start pg_rec application with following parameters (specified in app.src):

```erlang
  {db, [
      {host, "<host>"},
      {port, <port>},
      {user, "<username>"},
      {password, "<password>"},
      {database, "<database>"}
    ]}
```
