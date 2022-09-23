%%%-------------------------------------------------------------------
%%% @author Michael Ivanov
%%%-------------------------------------------------------------------
-module(main).
-import(crypto,[hash/2]).
-import(string,[concat/2]).
-import(string,[trim/3]).

%% API
-export([start/1, start/0, mine/0, acceptTCP/1, controller/1, listenTCP/0]).

-define(Zero, 48).
-define(GatorID, "ivanovmichael").
-define(K, 4).
-define(Port, 6000).

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

listenTCP() ->
  {ok, LSock} = gen_tcp:listen(?Port, [binary, {active, false}]),
  spawn(main, acceptTCP, [LSock]),
  timer:sleep(infinity).

acceptTCP(LSock) ->
  {ok, ASock} = gen_tcp:accept(LSock),
  spawn(main, acceptTCP, [LSock]),
  controller(ASock).

controller(ASock) ->
  inet:setopts(ASock, [{active, once}]),
  receive
    {tcp, ASock, <<"found", Coin/binary>>} ->
      io:format("~s\n", [Coin]),
      gen_tcp:send(ASock, "mine"),
      controller(ASock);
    {tcp, ASock, <<"ready">>} ->
      gen_tcp:send(ASock, "mine"),
      controller(ASock);
    {} ->
        controller(ASock)
  end.

worker(ASock) ->
  inet:setopts(ASock, [{active, once}]),
  receive
    {tcp, ASock, <<"mine">>} ->
      io:format("mining..\n"),
      Coin = mine(),
      gen_tcp:send(ASock, Coin),
      worker(ASock)
  end.

start(Host) ->
  {ok, ASock} = gen_tcp:connect(Host, ?Port, [binary, {active, true}]),
  gen_tcp:send(ASock, "ready"),
  worker(ASock).

start() ->
  Pid = spawn_link(main, listenTCP, []),
  {ok, Pid}.