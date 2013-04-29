-module(gen_db_provider).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [
    {save, 2}, 
    {find, 2}, 
    {find_many, 2}, 
    {delete, 2},
    {delete_many, 2}
    ];
behaviour_info(_) -> undefined.



