%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 10月 2025 19:18
%%%-------------------------------------------------------------------
-module(robot).
-author("caigou").


%% 服务启停接口
-export([start/1, stop/0]).
%% API
-export([]).


loop
handle_info(_Msg, State) ->
	{noreply, State}.


handle_call(_Req, _From, State) ->
	{reply, ok, State}.


handle_cast(stop, State) ->
	io:format("[chat_tcp_server] stopped.~n"),
	{stop, normal, State};

handle_cast(_, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.
