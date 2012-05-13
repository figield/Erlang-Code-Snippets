-module(calc_server).

-export([main/0]).

%% ======================================

main() ->
  io:format("main process ~w: started~n", [self()]),
  ServerPid = spawn(fun server/0),
  io:format("main process ~w: spawned subprocess ~w~n", [self(), ServerPid]),

  ServerPid ! {self(), [3, 4, '+']},
  receive {From1, Rsp1} ->
      io:format("main process ~w: received ~w~n", [self(), {From1, Rsp1}])
  end,

  ServerPid ! {self(), [3, 4, '*']},
  receive {From2, Rsp2} ->
      io:format("main process ~w: received ~w~n", [self(), {From2, Rsp2}])
  end,
  
  exit(ServerPid, kill),
  
  ok.


%% ======================================

server() ->
  io:format("server process ~w: started~n", [self()]),
  server_loop().

server_loop() ->
  receive
    {From, Req} ->
      io:format("server process ~w: received ~w~n", [self(), {From, Req}]),
      Rsp = calc(Req),
      From ! {self(), Rsp},
      server_loop()
  end.

calc([A, B, '*']) -> A * B;
calc([A, B, '+']) -> A + B.
