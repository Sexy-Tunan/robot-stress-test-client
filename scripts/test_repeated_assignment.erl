%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% 测试 dialyzer 检测重复赋值导致的模式匹配失败
%%% @end
%%% Created : 04. 11月 2025 10:47
%%%-------------------------------------------------------------------
-module(test_repeated_assignment).
-author("Administrator").

%% API
-export([run/1,test1/1]).

%% 添加类型规范，帮助 dialyzer 进行更精确的类型推断
-spec run(1) -> ok;
         (2) -> ok.
run(Type) ->
	A = trans(Type),
	NewType = change_type(Type),
	A = trans(NewType),  %% 这里会失败：如果Type=1，则A=1但trans(NewType)=2
	test1(Type),
	test2(Type),
	test3(Type),
	test4(Type),
	ok.

test1(Type) ->
	case Type of
		1 ->
			A = 1,
			A = 2;
		2 -> B = Type
 	end.

test3(Type) ->
	case Type of
		1 -> A = Type;
		2 -> A = Type
	end,
	A.

test4(Type) ->
	case Type of
		1 -> A = Type;
		2 -> A = Type
	end,
	A = Type +1.

test5(Type) ->
	if
		Type > 0 -> A = Type;
		Type < 0 -> A = Type+1;
		true -> A = -1
	end,
	A.

test6(Type) ->
	if
		Type > 0 ->
			A = Type,
			A = Type+1;
		Type < 0 -> A = Type+2;
		true -> A = -1
	end,
	A.

test7(Type) ->
	receive
		{a} -> A = Type;
		{b} -> A = Type+1;
		{c} -> A = Type+2
	end,
	A = Type + 3.


-spec trans(1) -> 1;
           (2) -> 2.
trans(Type) ->
	case Type of
		1 -> 1;
		2 -> 2
	end.

-spec change_type(1) -> 2;
                 (2) -> 1.
change_type(Type) ->
	case Type of
		1 -> 2;
		2 -> 1
	end.

get_data_info(RoleID, Type) ->
	{ok, #r_role_online_reward{infos = Infos}} = ?get_role_misc(r_role_online_reward),
	case lists:keyfind(Type, #r_role_online_info.type, Infos) of
		#r_role_online_info{ext_1 = Ext1, ext_2 = Ext2} = OldInfo ->
			NewExt1 = ?_if(is_integer(Ext1), Ext1, 0),
			NewExt2 = ?_if(is_integer(Ext2), Ext2, 0),
			Info = OldInfo#r_role_online_info{ext_1 = NewExt1, ext_2 = NewExt2};
		_ ->
			Info = #r_role_online_info{type = Type}
	end,
	Today = common_time:date(),
	ResetDate = common_time:get_monday(),
	case Type of
		?TYPE_NEW -> %%新手奖励
			case Info of
				#r_role_online_info{op_date = OpDate, reset_date = OldResetDate, acc_online_time = AccOnlineTime} when OldResetDate =/= 0, OpDate =/= Today ->
					CfgMaxTime = cfg_online_reward:max_online_time(Type),
					CfgAllIDs = get_id_list(Type, OldResetDate),
					case AccOnlineTime >= CfgMaxTime of
						true -> %%发放未领取奖励
							send_rewards(Info, RoleID),
							NewInfo = Info#r_role_online_info{op_date = Today, fetched_list = CfgAllIDs},
							%%加入每周奖励
							lists:keymember(?TYPE_WEEK, #r_role_online_info.type, Infos) orelse set_data_info(RoleID, ?TYPE_WEEK, #r_role_online_info{type = ?TYPE_WEEK});
						false ->
							NewInfo = Info#r_role_online_info{op_date = Today}
					end;
				_ ->
					NewInfo = Info
			end,
			NewInfo = Info;
		?TYPE_WEEK -> %%每周奖励
			case Info of
				#r_role_online_info{type = Type, reset_date = OldResetDate} when OldResetDate =/= 0, ResetDate =/= OldResetDate ->
					send_rewards(Info, RoleID),
					NewInfo = #r_role_online_info{type = Type, reset_date = ResetDate, op_date = Today};
				#r_role_online_info{op_date = OpDate} when OpDate =/= Today ->
					NewInfo = Info#r_role_online_info{op_date = Today, fetch_num = 0, store_num = 0};
				_ ->
					NewInfo = Info
			end
	end,
	NewInfo2 = NewInfo#r_role_online_info{reset_date = ResetDate, op_date = Today},
	Info =/= NewInfo2 andalso set_data_info(RoleID, Type, NewInfo2),
	{ok, NewInfo2}.
