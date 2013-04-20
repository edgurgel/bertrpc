-module(bertrpc).

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = lager:start(),
  ok = application:start(bertrpc).
