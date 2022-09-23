%%%-------------------------------------------------------------------
%%% @author Michael Ivanov
%%%-------------------------------------------------------------------
-module(main).
-import(crypto,[hash/2]).
-import(string,[concat/2]).
-import(string,[trim/3]).

%% API
-export([start/1, start/0, mine/0, acceptTCP/1, controller/2]).

-define(Zero, 48).
-define(GatorID, "ivanovmichael").
-define(K, 4).
-define(Port, 6000).
-define(CoinsPerWorker, 5).

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

acceptTCP(LSock) ->
  {ok, ASock} = gen_tcp:accept(LSock),
  spawn(main, acceptTCP, [LSock]),
  controller(ASock, 0).

controller(ASock, CoinCount) when CoinCount < ?CoinsPerWorker ->
  inet:setopts(ASock, [{active, once}]),
  receive
    {tcp, ASock, <<"found", Coin/binary>>} ->
      io:format("~s\n", [Coin]),
      gen_tcp:send(ASock, "mine"),
      controller(ASock, CoinCount + 1);
    {tcp, ASock, <<"ready">>} ->
      gen_tcp:send(ASock, "mine"),
      controller(ASock, CoinCount)
  end;
controller(ASock, CoinCount) when CoinCount >= ?CoinsPerWorker ->
  gen_tcp:send(ASock, "halt"),
  gen_tcp:close(ASock),
  erlang:system_time(seconds).

worker(ASock) ->
  inet:setopts(ASock, [{active, once}]),
  receive
    {tcp, ASock, <<"mine">>} ->
      io:format("mining..\n"),
      Coin = mine(),
      gen_tcp:send(ASock, Coin),
      worker(ASock);
    {tcp, ASock, <<"halt">>} ->
      gen_tcp:close(ASock)
  end.

start(Host) ->
  {ok, ASock} = gen_tcp:connect(Host, ?Port, [binary, {active, true}]),
  gen_tcp:send(ASock, "ready"),
  worker(ASock).

start() ->
  StartTime = erlang:system_time(seconds),
  {ok, LSock} = gen_tcp:listen(?Port, [binary, {active, false}]),
  StopTime = acceptTCP(LSock),

  io:format("Real Time: ~p seconds\n", [StopTime - StartTime]).