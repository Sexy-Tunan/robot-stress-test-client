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

-include("../../include/geography.hrl").
-include("../../include/chat_protocol.hrl").

%% API
-export([start/3]).

-define(WORLD_CHANNEL, <<"world">>).
-define(CHAT_INTERVAL, 20000).
-define(MOVE_INTERVAL, 2000).

%% 启动机器人
%% Socket: TCP连接
%% RobotId: 机器人ID (用于生成用户名)
%% MessageList: 随机消息列表
start(Socket, RobotId, MessageList) ->
	process_flag(trap_exit, true),
	UserName = unicode:characters_to_binary(io_lib:format("Robot~4..0B", [RobotId]),utf8,utf8),
	Password = <<"123456">>,

	%% 发送登录消息包，然后进入循环
	build_login_packet_and_send(Socket, UserName, Password),
	loop(#{robot_id => RobotId, user_name => UserName, socket => Socket, message_list => MessageList}).


%% 登录函数
build_login_packet_and_send(Socket, UserName, Password) ->
	PayloadMap = #{userName => UserName, password => Password},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<?LOGIN_REQUEST_PROTOCOL_NUMBER:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	case gen_tcp:send(Socket, Packet) of
		ok ->
			inet:setopts(Socket, [{active, once}]), ok;
		{error, Reason} ->
			{error, Reason}
	end.


build_map_packet_send(Socket, ChannelName) ->
	%% 构造消息包
	PayloadMap = #{
		channel => ChannelName
	},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<?MAP_REQUEST_PROTOCOL_NUMBER:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	case gen_tcp:send(Socket, Packet) of
		ok -> ok;
		{error, Reason} ->
			{error, Reason}
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
			case handle_server_message(ProtoId, JsonBin, State, Socket) of
				{ok,NewState} ->
					inet:setopts(Socket, [{active, once}]),
					loop(NewState);
				{error,Reason, NewState} ->
					io:format("错误信息[~ts]~n", [Reason]),
					inet:setopts(Socket, [{active, once}]),
					loop(NewState)
			end;


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

%% 处理服务器消息
%% {ok,NewState} | {error,Reason,NewState}
handle_server_message(ProtoId, JsonBin, State, Socket) ->

	DataMap = jsx:decode(JsonBin, [return_maps, {labels, atom}]),
	case ProtoId of
		%% 登录响应
		?LOGIN_RESPONSE_PROTOCOL_NUMBER ->
			UserName = maps:get(user,DataMap),
			case maps:get(state, DataMap, false) of
				true ->
					io:format("[~ts] 登录成功~n", [UserName]),
					build_map_packet_send(Socket, <<"world">>),
					{ok, State#{user_name => UserName}};

				false ->
					io:format("[~ts] 登录失败: ~ts~n", [UserName, maps:get(reason, DataMap, <<"未知错误">>)]),
					{error, maps:get(reason, DataMap, <<"未知错误">>), State}
			end;

		%% 地图响应
		?MAP_RESPONSE_PROTOCOL_NUMBER ->
			UserName = maps:get(user_name, State),
			%% 初始化状态
			case maps:get(state, DataMap, false) of
				true ->
					WorldMap = {maps:get(channel, DataMap), maps:get(width, DataMap), maps:get(height, DataMap)},

					%% 将经常修改的position变量存进程字典
					put(current_x,50),
					put(current_y,50),
					io:format("[~ts] 初始位置: (~p, ~p)~n", [UserName, get(current_x), get(current_y)]),

					%% 开始定时器
					erlang:send_after(?CHAT_INTERVAL, self(), chat_tick),
					erlang:send_after(?MOVE_INTERVAL, self(), move_tick),
					{ok, State#{current_map => WorldMap}};
		false ->
			io:format("[~ts]获取地图失败--原因: ~ts~n", [UserName, maps:get(reason, DataMap, <<"未知错误">>)]),
			{error, maps:get(reason, DataMap, <<"未知错误">>), State}
	end;

	%% 消息广播
	?MSG_BROADCAST_PROTOCOL_NUMBER ->
		UserName = maps:get(user_name, State),
		Sender = maps:get(sender, DataMap),
		%% 优化输出：只输出自己的消息广播，避免大量输出（压测时200个机器人会产生海量消息）
		case Sender =:= UserName of
			true ->
				Channel = maps:get(channel, DataMap),
				Message = maps:get(message, DataMap),
				io:format("[~ts] 消息发送成功 [@~ts]: ~ts~n", [UserName, Channel, Message]);
			false ->
				ok  %% 不输出其他机器人的消息，减少输出量
		end,
		{ok,State};

	%% 移动广播
	?MOVE_BROADCAST_PROTOCOL_NUMBER ->
		UserName = maps:get(user_name, State),
		User = maps:get(user, DataMap),
		%% 优化输出：只输出自己的移动广播，避免大量输出（不输出其他机器人的移动，减少输出量，但本质上并没有减少进程的消息队列消息，只是尝试减少 erl shell控制台输出是否有助于消息快速处理）
		case User =:= UserName of
			true ->
				FromX = maps:get(from_x, DataMap),
				FromY = maps:get(from_y, DataMap),
				ToX = maps:get(to_x, DataMap),
				ToY = maps:get(to_y, DataMap),
				io:format("[~ts] 移动成功: 从(~p,~p)移动到(~p,~p)~n",
					[UserName, FromX, FromY, ToX, ToY]);
			false ->
				ok
		end,
		{ok,State};

		_Other ->
%%			UserName = maps:get(user_name, State),
%%			io:format("[~ts] 收到未知协议 ~p: ~p~n", [UserName, ProtoId, DataMap]),
			{ok,State}
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
		message => unicode:characters_to_binary(Message)
	},
	PayloadJsonBin = jsx:encode(PayloadMap),
	Packet = <<?MSG_REQUEST_PROTOCOL_NUMBER:16/big-unsigned-integer, PayloadJsonBin/binary>>,
	
	gen_tcp:send(Socket, Packet),
	%% 不在此处输出，等收到服务器广播后再输出，避免重复
	State.


%% 处理移动
handle_move(State) ->
	Socket = maps:get(socket, State),
	UserName = maps:get(user_name, State),
%%	CurrentX = maps:get(current_x, State),
%%	CurrentY = maps:get(current_y, State),
 	CurrentX = get(current_x),
	CurrentY = get(current_y),

	{ChannelName, Width, Height} = maps:get(current_map, State),
	%% 随机选择方向并计算新位置
	Direction = lists:nth(rand:uniform(4), [up, down, left, right]),
	{NewX, NewY} = calculate_new_position(CurrentX, CurrentY, Direction, Width, Height),
	
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
	put(current_x, NewX),
	put(current_y, NewY),
	State.
%%	State#{
%%		current_x => NewX,
%%		current_y => NewY,
%%	}.


%% 计算新位置（确保不超出地图边界）,碰到边界按前进的相反方向后退一步
calculate_new_position(X, Y, up, MaxX, MaxY) ->
	case Y+1 > MaxY of
		true -> {X, Y - 1};
		false -> {X, Y + 1}
	end;
calculate_new_position(X, Y, down, MaxX, MaxY) ->
	case Y-1 < 0 of
		true -> {X, Y+1};
		false -> {X, Y-1}
	end;
calculate_new_position(X, Y, left, MaxX, MaxY) ->
	case X-1 < 0 of
		true -> {X+1, Y};
		false -> {X-1, Y}
	end;
calculate_new_position(X, Y, right, MaxX, MaxY) ->
	case X+1 > MaxX of
		true -> {X-1, Y};
		false -> {X+1, Y}
	end.




