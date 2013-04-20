-compile([{parse_transform, lager_transform}]).
-module(bertrpc_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {hostname, port}).

-define(TIMEOUT, 5000).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

init(Args) ->
  Hostname = proplists:get_value(hostname, Args),
  Port = proplists:get_value(port, Args),
  {ok, #state{hostname=Hostname, port=Port}}.

establish_connection(#state{hostname=Hostname, port=Port}=State) ->
  lager:info("Establishing connection"),
  case gen_tcp:connect(Hostname, Port, [binary, {packet, 4}, {active, false}]) of
    {ok, Socket} -> {ok, Socket};
    Other -> lager:error("Unable to establish connection ~p, ~n", [Other]),
      timer:sleep(1000),
      establish_connection(State)
  end.


handle_call({Module, Func, Args}, _From, State) ->
  {ok, Socket} = establish_connection(State),
  Data = bert:encode({call, Module, Func, Args}),
  ok = gen_tcp:send(Socket, Data),
  Reply = case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
    {ok, RecvData} -> lager:info("Received data"),
      case bert:decode(RecvData) of
        {reply, Result} -> lager:debug("Received ~p", [Result]),
          {reply, Result, State};
        Other -> lager:error("Received unexpected data: ~p", [Other])
      end;
    {error, timeout} -> lager:info("Timed out received"),
      {reply, error, State};
    {error, closed} -> lager:error("Closed TCP socket"),
      {reply, error, State};
    {error , Reason} -> lager:error("Error, reason: ~p", [Reason]),
      exit(Reason)
  end,
  gen_tcp:close(Socket), %ALWAYS close the socket
  Reply;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({Module, Func, Args}, State) ->
  {ok, Socket} = establish_connection(State),
  Data = bert:encode({cast, Module, Func, Args}),
  case gen_tcp:send(Socket, Data) of
    ok -> ok;
    {error, Reason} -> lager:error("Error while sending ~p, Reason: ~p", [Data, Reason])
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
