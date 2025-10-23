%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc 单个机器人逻辑实现
%%% 每个机器人：
%%% - 每60秒发送一次随机消息
%%% - 每2秒随机走路一次
%%% @end
%%% Created : 23. 10月 2025 19:18
%%%-------------------------------------------------------------------
-module(robot).
-author("caigou").

-include("../../../include/geography.hrl").
-include("../../../include/chat_protocol.hrl").

%% API
-export([start/3]).

-define(WORLD_CHANNEL, <<"world">>).
-define(CHAT_INTERVAL, 60000).    %% 60秒 = 60000毫秒
-define(MOVE_INTERVAL, 2000).     %% 2秒 = 2000毫秒

%% 启动机器人
%% Socket: TCP连接
%% RobotId: 机器人ID (用于生成用户名)
%% MessageList: 随机消息列表
start(Socket, RobotId, MessageList) ->
	process_flag(trap_exit, true),
	UserName = list_to_binary(io_lib:format("Robot~4..0B", [RobotId])),
	Password = <<"123456">>,
	
	%% 登录
	case login(Socket, UserName, Password) of
		ok ->
			io:format("[~ts] 登录成功~n", [UserName]),
			%% 初始化状态
			State = #{
				socket => Socket,
				user_name => UserName,
				robot_id => RobotId,
				message_list => MessageList,
				last_chat_time => 0,
				last_move_time => 0
			},
			io:format("[~ts] 初始位置: (~p, ~p)~n", [UserName, maps:get(current_x, State), maps:get(current_y, State)]),
			
			%% 开始定时器
			erlang:send_after(?CHAT_INTERVAL, self(), chat_tick),
			erlang:send_after(?MOVE_INTERVAL, self(), move_tick),
			
			%% 进入循环
			loop(State);
		{error, Reason} ->
			io:format("[Robot~4..0B] 登录失败: ~p~n", [RobotId, Reason]),
			gen_tcp:close(Socket)
	end.

%% 主循环
loop(State) ->
	receive
		%% 聊天定时器触发
		chat_tick ->
			NewState = handle_chat(State),
			erlang:send_after(?CHAT_INTERVAL, self(), chat_tick),
			loop(NewState);
		
		%% 移动定时器触发
		move_tick ->
			NewState = handle_move(State),
			erlang:send_after(?MOVE_INTERVAL, self(), move_tick),
			loop(NewState);
		
		%% 接收服务器消息
		{tcp, Socket, <<ProtoId:16, JsonBin/binary>>} ->
			handle_server_message(ProtoId, JsonBin, State),
			inet:setopts(Socket, [{active, once}]),
			loop(State);
		
		%% TCP连接关闭
		{tcp_closed, _Socket} ->
			io:format("[~ts] 连接已关闭~n", [maps:get(user_name, State)]),
			ok;
		
		%% 停止信号
		stop ->
			io:format("[~ts] 收到停止信号~n", [maps:get(user_name, State)]),
			gen_tcp:close(maps:get(socket, State)),
			ok;
		
		_Other ->
			loop(State)
	end.

%% 处理聊天
handle_chat(State) ->
	Socket = maps:get(socket, State),
	UserName = maps:get(user_name, State),
	MessageList = maps:get(message_list, State),
	
	%% 随机选择一条消息
	Message = lists:nth(rand:uniform(length(MessageList)), MessageList),
	
	%% 构造消息包
	PayloadMap = #{
		sender => UserName,
		channel => ?WORLD_CHANNEL,
		message => list_to_binary(Message)
	},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<?MSG_REQUEST_PROTOCOL_NUMBER:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	
	gen_tcp:send(Socket, Packet),
	io:format("[~ts] 发送消息: ~ts~n", [UserName, Message]),
	
	State#{last_chat_time => erlang:system_time(second)}.

%% 处理移动
handle_move(State) ->
	Socket = maps:get(socket, State),
	UserName = maps:get(user_name, State),
	CurrentX = maps:get(current_x, State),
	CurrentY = maps:get(current_y, State),
	
	%% 随机选择方向并计算新位置
	Direction = lists:nth(rand:uniform(4), [up, down, left, right]),
	{NewX, NewY} = calculate_new_position(CurrentX, CurrentY, Direction),
	
	%% 构造移动包
	PayloadMap = #{
		user => UserName,
		channel => ?WORLD_CHANNEL,
		from_x => CurrentX,
		from_y => CurrentY,
		to_x => NewX,
		to_y => NewY
	},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<?MOVE_REQUEST_PROTOCOL_NUMBER:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	
	gen_tcp:send(Socket, Packet),
	io:format("[~ts] 从格子(~p,~p)移动到格子(~p,~p)~n", [UserName, CurrentX, CurrentY, NewX, NewY]),
	
	State#{
		current_x => NewX,
		current_y => NewY,
		last_move_time => erlang:system_time(second)
	}.

%% 计算新位置（确保不超出地图边界）
calculate_new_position(X, Y, up) ->
	{X, min(Y + 1, ?MAP_HEIGHT - 1)};
calculate_new_position(X, Y, down) ->
	{X, max(Y - 1, 0)};
calculate_new_position(X, Y, left) ->
	{max(X - 1, 0), Y};
calculate_new_position(X, Y, right) ->
	{min(X + 1, ?MAP_WIDTH - 1), Y}.

%% 处理服务器消息
handle_server_message(ProtoId, JsonBin, State) ->
	UserName = maps:get(user_name, State),
	DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
	
	case ProtoId of
		%% 登录响应
		?LOGIN_RESPONSE_PROTOCOL_NUMBER ->
			case maps:get(state, DataMap, false) of
				true ->
					io:format("[~ts] 收到登录成功响应~n", [UserName]);
				false ->
					Reason = maps:get(reason, DataMap, <<"未知原因">>),
					io:format("[~ts] 登录失败: ~ts~n", [UserName, Reason])
			end;

		%% 消息广播
		?MSG_BROADCAST_PROTOCOL_NUMBER ->
			Sender = maps:get(sender, DataMap),
			Channel = maps:get(channel, DataMap),
			Message = maps:get(message, DataMap),
			io:format("[~ts] 收到消息 [~ts@~ts]: ~ts~n", [UserName, Sender, Channel, Message]);

		%% 移动广播
		?MOVE_BROADCAST_PROTOCOL_NUMBER ->
			User = maps:get(user, DataMap),
			FromX = maps:get(from_x, DataMap),
			FromY = maps:get(from_y, DataMap),
			ToX = maps:get(to_x, DataMap),
			ToY = maps:get(to_y, DataMap),
			io:format("[~ts] 收到移动广播: ~ts 从(~p,~p)移动到(~p,~p)~n", 
				[UserName, User, FromX, FromY, ToX, ToY]);
		
		_Other ->
			io:format("[~ts] 收到未知协议 ~p: ~p~n", [UserName, ProtoId, DataMap])
	end.

%% 登录函数
login(Socket, UserName, Password) ->
	PayloadMap = #{userName => UserName, password => Password},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<10001:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	
	case gen_tcp:send(Socket, Packet) of
		ok ->
			inet:setopts(Socket, [{active, once}]),
			%% 等待登录响应
			receive
				{tcp, Socket, <<?LOGIN_RESPONSE_PROTOCOL_NUMBER:16, JsonBin/binary>>} ->
					ResponseMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
					case maps:get(state, ResponseMap, false) of
						true -> ok;
						false -> {error, maps:get(reason, ResponseMap, <<"未知错误">>)}
					end;
				{tcp_closed, _} ->
					{error, connection_closed}
			after 5000 ->
				{error, login_timeout}
			end;
		{error, Reason} ->
			{error, Reason}
	end.
