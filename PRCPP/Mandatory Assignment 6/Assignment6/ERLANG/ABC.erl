-module(helloworld).
-export([start/0,account/1,bank/0,clerk/0]).

n2s(N) -> lists:flatten(io_lib:format("~p", [N])). %% HACK!

account(Balance) ->
   receive 
      {deposit,Amount} ->
         account(Balance+Amount);
      {printbalance} ->
         io:fwrite(n2s(Balance) ++ "\n")
   end.

bank() ->
   receive
      {transfer,Amount,From,To} ->
         From ! {deposit,-Amount},
         To ! {deposit,+Amount},
         bank()
   end.

random(N) -> random:uniform(N) div 10.

ntransfers(0,_,_,_) -> true;
ntransfers(N,Bank,From,To) -> 
   R = random(100),
   Bank ! {transfer,R,From,To},
   ntransfers(N-1,Bank,From,To).

clerk() ->
   receive
      {start,Bank,From,To} ->
         random:seed(now()),
         ntransfers(100,Bank,From,To),
         clerk()
   end.

start() -> 
   A1 = spawn(helloworld,account,[0]),
   A2 = spawn(helloworld,account,[0]),
   B1 = spawn(helloworld,bank,[]),
   B2 = spawn(helloworld,bank,[]),
   C1 = spawn(helloworld,clerk,[]),
   C2 = spawn(helloworld,clerk,[]),
   C1 ! {start,B1,A1,A2},
   C2 ! {start,B2,A2,A1},
   timer:sleep(1000),
   A1 ! {printbalance},
   A2 ! {printbalance}.