-module(helloworld).
-export([start/0,myactor/1]).

myactor(Count) ->
   receive
      {msg, Msg} -> 
         io:fwrite(Msg ++ " ("),
         io:write(Count),
         io:fwrite(")\n"),
         myactor(Count + 1)
   end.

start() ->
   MyActor = spawn(helloworld, myactor, [0]),
   MyActor ! {msg, "hello"},
   MyActor ! {msg, "world"}.
