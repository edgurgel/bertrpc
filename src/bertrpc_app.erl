-compile([{parse_transform, lager_transform}]).
-module(bertrpc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([call/3, cast/3]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  bertrpc_sup:start_link().

stop(_State) ->
  ok.

call(Module, Func, Args) ->
  poolboy:transaction(Module, fun(Worker) ->
        gen_server:call(Worker, {Module, Func, Args})
    end).

cast(Module, Func, Args) ->
  poolboy:transaction(Module, fun(Worker) ->
        gen_server:cast(Worker, {Module, Func, Args})
    end).
