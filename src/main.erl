%%%-------------------------------------------------------------------
%%% @author Michael Ivanov
%%%-------------------------------------------------------------------
-module(main).
-import(crypto,[hash/2]).
-import(string,[concat/2]).
-import(string,[trim/3]).

%% API
-export([start/2, start/1, start/0, halt/2]).

-define(Zero, 48).
-define(GatorID, "ivanovmichael").
-define(K, 4).
-define(Port, 6000).
-define(ExpireTime, 8000).

countZeros(String, Count) when length(String) > 0 ->
  [H | T] = String,
  if
    H == ?Zero ->
      countZeros(T, Count + 1);
    true ->
      Count
  end;
countZeros([], Count) ->
  Count.

getHash(String) ->
  EncodedHash = crypto:hash(sha256, String),
  io_lib:format("~64.16.0b", [binary:decode_unsigned(EncodedHash)]).

mine() ->
  RandomString = concat(?GatorID, randomString()),
  Hash = getHash(RandomString),
  case countZeros(Hash, 0) >= ?K of
    true -> io_lib:format("found~s\t~s", [RandomString, Hash]);
    false -> mine()
  end.

% https://stackoverflow.com/questions/12788799/how-to-generate-a-random-alphanumeric-string-with-erlang
randomString() ->
  B64String = base64:encode(crypto:strong_rand_bytes(16)),
  string:trim(B64String, trailing, "=").

controller(Sock) ->
  receive
    {udp, Sock, Host, Port, <<"found", Coin/binary>>} ->
      io:format("~s\n", [Coin]),
      gen_udp:send(Sock, Host, Port, "mine"),
      controller(Sock);
    {udp, Sock, Host, Port, <<"ready">>} ->
      gen_udp:send(Sock, Host, Port, "mine"),
      controller(Sock);
    {udp, _, _, _, <<"halt">>} ->
      gen_udp:close(Sock),
      erlang:system_time(seconds)
  end.

worker(Sock) ->
  receive
    {udp, Sock, Host, Port, <<"mine">>} ->
      io:format("mining..\n"),
      Coin = mine(),
      gen_udp:send(Sock, Host, Port, Coin),
      worker(Sock);
    {udp, _, _, _, <<"halt">>} ->
      gen_udp:close(Sock);
    _ -> % Controller closed
      gen_udp:close(Sock)
  end.

start(Host, Port) -> % Start worker with Host IP and specified Port
  {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
  gen_udp:send(Sock, Host, ?Port, "ready"),
  worker(Sock).

start(Host) -> % Start worker with Host IP
  {ok, Sock} = gen_udp:open(?Port, [binary, {active, true}]),
  gen_udp:send(Sock, Host, ?Port, "ready"),
  worker(Sock).

start() -> % Start controller and wait for workers
  StartTime = erlang:system_time(seconds),
  {ok, Sock} = gen_udp:open(?Port, [binary, {active,true}]),
  StopTime = controller(Sock),

  io:format("Real Time: ~p seconds\n", [StopTime - StartTime]).

halt(Host, Port) -> % Halt the controller
  {ok, Sock} = gen_udp:open(Port, [binary, {active, true}]),
  gen_udp:send(Sock, Host, ?Port, "halt").