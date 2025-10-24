%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc 机器人客户端管理器
%%% 负责创建和管理多个机器人进程
%%% @end
%%% Created : 23. 10月 2025 19:10
%%%-------------------------------------------------------------------
-module(robot_client).
-author("caiogu").

-behavior(gen_server).

%% 服务启停接口
-export([start/3, stop/0]).
%% gen_server 回调接口
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).

%% 启动机器人客户端管理器
%% Number: 机器人数量
%% Host: 服务器地址
%% Port: 服务器端口
start(Number, Host, Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Number, Host, Port], []).

%% 停止机器人客户端管理器
stop() ->
	gen_server:cast(?MODULE, stop).

init([Number, Host, Port]) ->
	io:format("=== 使用机器人压力测试服务端 ===~n"),
	io:format("机器人数量: ~p~n", [Number]),
	
	%% 读取随机消息文件
	MessageList = read_message_file("data/random_message.txt"),
	io:format("加载随机消息数量: ~p~n", [length(MessageList)]),
	
	%% 启动所有机器人
	PidList = start_robots(Number, Host, Port, MessageList, []),
	io:format("成功启动机器人数量: ~p~n", [length(PidList)]),
	io:format("=== 所有机器人已启动完成 ===~n"),
	
	{ok, #{pids => PidList}}.

%% 启动多个机器人
start_robots(0, _Host, _Port, _MessageList, PidList) -> 
	PidList;
start_robots(Number, Host, Port, MessageList, PidList) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, once}]) of
		{ok, Socket} ->
			%% 为每个机器人分配一个ID (从1开始)
			RobotId = Number,
			Pid = spawn(fun() -> robot:start(Socket, RobotId, MessageList) end),
			
			case gen_tcp:controlling_process(Socket, Pid) of
				ok ->
					%% 通知机器人进程开始工作
					Pid ! start,
					%% 继续启动下一个机器人
					start_robots(Number - 1, Host, Port, MessageList, [Pid | PidList]);
				{error, Reason} ->

					io:format("机器人~4..0B 移交控制权失败: ~p~n", [RobotId, Reason]),
					Pid ! stop,
					gen_tcp:close(Socket),
					start_robots(Number - 1, Host, Port, MessageList, PidList)
			end;
		{error, Reason} ->
			io:format("机器人~4..0B 连接服务器失败: ~p~n", [Number, Reason]),
			%% 连接失败时等待一下再重试
			timer:sleep(100),
			start_robots(Number - 1, Host, Port, MessageList, PidList)
	end.

%% 读取随机消息文件
read_message_file(FilePath) ->
	case file:read_file(FilePath) of
		{ok, Binary} ->
			%% 按行分割，过滤空行
			Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
			%% 转换为字符串列表并过滤空字符串
			Messages = [unicode:characters_to_list(Line, utf8) || 
						Line <- Lines, 
						byte_size(Line) > 0],
			case Messages of
				[] -> ["Hello", "Test", "Nice"]; %% 如果文件为空，使用默认消息
				_ -> Messages
			end;
		{error, Reason} ->
			io:format("读取消息文件失败: ~p，使用默认消息~n", [Reason]),
			["Hello", "Test", "Nice", "Good", "Great"]
	end.

%% gen_server 回调函数
handle_info(_Msg, State) ->
	{noreply, State}.

handle_call(_Req, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	io:format("[robot_client] 正在停止所有机器人...~n"),
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	io:format("接收到停止请求，开始向所有机器人发送断开连接请求~n"),
	PidList = maps:get(pids, State, []),
	[Pid ! stop || Pid <- PidList, is_process_alive(Pid)],
	timer:sleep(1000), %% 等待所有机器人优雅关闭
	ok.
