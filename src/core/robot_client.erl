%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 10月 2025 19:10
%%%-------------------------------------------------------------------
-module(robot_client).
-author("caiogu").

-behavior(gen_server).

%% 服务启停接口
-export([start/3]).
%% gen_server 回调接口
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).
%% API
-export([]).

start(Number,Host,Port) ->
	gen_server:start({local,?MODULE},?MODULE,[Number,Host,Port],[]).


init([Number,Host,Port]) ->
	{ok, start_robot(Number,Host,Port,[])}.

start_robot(0, Host, Port, PidList) -> PidList;
start_robot(Number, Host, Port, PidList) ->
	{ok, Socket} = gen_tcp:connect(Host,Port, [{packet, 4},binary]),
	Pid = spawn(robot,start,[Socket]),
	case gen_tcp:controlling_process(Socket,Pid) of
		ok -> Pid ! start, start(Number - 1, Host,Pid, Pid ++ PidList);
		{error,_} -> io:format("连接服务端失败"), Pid ! stop
	end.


handle_info(_Msg, State) ->
	{noreply, State}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	io:format("[chat_tcp_server] stopped.~n"),
	{stop, normal, State};

handle_cast(_, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("接受到停止请求,开始向所有robot发送断开连接请求"),
	[Pid ! stop || Pid <- State],
	ok.

