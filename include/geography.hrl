%%%-------------------------------------------------------------------
%%% @author caigou
%%% @doc 地图地理信息定义
%%% Created : 2025/10/23/17.26
%%%-------------------------------------------------------------------

%% 地图坐标
-record(grid_position,{x,y}).
-type x() :: integer().
-type y() :: integer().
-type grid_position() :: #grid_position{x :: x(), y :: y()}.


-record(member_position,{user_name,grid_position}).
-type member_position() :: #member_position{user_name :: string(), grid_position :: grid_position()}.


%% 移动方向定义
-define(DIRECTION_UP, up).
-define(DIRECTION_DOWN, down).
-define(DIRECTION_LEFT, left).
-define(DIRECTION_RIGHT, right).

%% 移动协议包
-record(move_packet, {
	user :: binary(),        %% 用户名
	channel :: binary(),     %% 频道/地图名
	from_x :: integer(),     %% 起始X坐标
	from_y :: integer(),     %% 起始Y坐标
	to_x :: integer(),       %% 目标X坐标
	to_y :: integer()        %% 目标Y坐标
}).

