%%%-------------------------------------------------------------------
%%% @author caigou
%%% @doc 启动机器人压力测试
%%%-------------------------------------------------------------------
-module(start_robots).
-export([start/0, start/1, start/3]).

%% 使用默认参数启动
start() ->
	start(200).

%% 指定机器人数量启动 (连接本地服务器8080端口)
start(Number) ->
	start(Number, "172.22.2.101", 10086).

%% 自定义启动
start(Number, Host, Port) ->
	%% 启动客户端管理器
	case robot_client:start(Number, Host, Port) of
		{ok, Pid} ->
			io:format("压力测试启动成功!管理进程: ~p~n", [Pid]),
			{ok, Pid};
		{error, Reason} ->
			io:format("启动失败: ~p~n", [Reason]),
			{error, Reason}
	end.

%% 停止所有机器人
stop() ->
	io:format("正在停止所有机器人...~n"),
	robot_client:stop(),
	io:format("已发送停止信号~n").

