-module(helloworld).
-export([start/0,hello/1]).

hello(Count) ->
   receive
      {name, Name} -> 
         io:fwrite(Name ++ "\n"),
         hello(Count + 1)
   end.

start() ->
   Hello = spawn(helloworld, hello, [0]),
   Hello ! {name, "John Doe"},
   Hello ! {name, "Jane Doe"}.
Hell