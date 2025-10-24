%% ============================================================
%% CLIENT --> SERVER
-define(LOGIN_REQUEST_PROTOCOL_NUMBER, 10001).
-define(MSG_REQUEST_PROTOCOL_NUMBER, 11001).
-define(CHANNEL_CREATE_REQUEST_PROTOCOL_NUMBER, 12001).
-define(CHANNEL_DELETE_REQUEST_PROTOCOL_NUMBER, 12002).
-define(JOIN_CHANNEL_REQUEST_PROTOCOL_NUMBER, 13001).
-define(QUIT_CHANNEL_REQUEST_PROTOCOL_NUMBER, 13002).
-define(MOVE_REQUEST_PROTOCOL_NUMBER, 14001).
-define(MAP_REQUEST_PROTOCOL_NUMBER, 15001).



%%=============================================================
%% SERVER --> CLIENT
-define(LOGIN_RESPONSE_PROTOCOL_NUMBER, 20001).
-define(MSG_BROADCAST_PROTOCOL_NUMBER, 21001).
-define(JOIN_CHANNEL_RESPONSE_PROTOCOL_NUMBER, 22003).
-define(CREATE_CHANNEL_BROADCAST_PROTOCOL_NUMBER, 22001).
-define(DELETE_CHANNEL_BROADCAST_PROTOCOL_NUMBER, 22002).
-define(JOIN_CHANNEL_BROADCAST_PROTOCOL_NUMBER, 23001).
-define(QUIT_CHANNEL_BROADCAST_PROTOCOL_NUMBER, 23002).
-define(LIMIT_WORLD_SEND_PROTOCOL_NUMBER, 30001).
-define(MOVE_BROADCAST_PROTOCOL_NUMBER, 24001).
-define(MAP_RESPONSE_PROTOCOL_NUMBER, 25001).


%% ============================================================
%% 协议号决定行为，数据体就是单纯的数据，它的意义依据行为的不同而发生变化,例如创建删除频道都可通用channel_user_packet的数据包格式
-record(msg_packet,{
	sender,
	channel,
	message
}).

-record(login_packet,{
	user,
	password
}).

-record(channel_user_packet,{
	channel,
	user
}).


%% ===========================================================
%% 响应包格式
-record(login_response_packet,{
	state,
	user,
	%% {data ==>
	%% 		[{channel_name => ChannelName1, members => [usr1,user2,user3], creator => UserName}, {channel_name => ChannelName1, members => [usr1,user2,user3], creator => UserName}]
	%% }
	%% state为false时，data包装错误原因； state为true时，data包装已加入的频道以及频道所拥有的用户信息返回
	data
}).
-record(join_response_packet,{
	state,
	%% {data ==>
	%% 		[{channel_name => ChannelName1, members => [usr1,user2,user3]}, {channel_name => ChannelName1, members => [usr1,user2,user3]}]
	%% }
	data
}).
%% 聊天消息广播返回响应包格式
-record(msg_response_packet,{
	sender,
	channel,
	message
}).
%% 用户频道广播消息响应,依据协议号不同，user有不同的意义，如果是创建频道就是creater，如果是加入频道就是joiner
-record(common_response_packet,{
	user,
	channel
}).

%% 移动请求包格式
-record(move_request_packet,{
	user,
	channel,
	from_x,
	from_y,
	to_x,
	to_y
}).

%% 移动广播包格式
-record(move_broadcast_packet,{
	user,
	channel,
	from_x,
	from_y,
	to_x,
	to_y
}).


