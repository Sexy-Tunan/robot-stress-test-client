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
	ok.

test1(Type) ->
	case Type of
		1 ->
			A = 1,
			A = 2;
		2 -> B = Type
 	end.

%%test3(Type) ->
%%	case Type of
%%		1 -> A = Type;
%%		2 -> A = Type
%%	end,
%%	A.
%%
%%test4(Type) ->
%%	case Type of
%%		1 -> A = Type;
%%		2 -> A = Type
%%	end,
%%	A = Type +1.
%%
%%test5(Type) ->
%%	if
%%		Type > 0 -> A = Type;
%%		Type < 0 -> A = Type+1;
%%		true -> A = -1
%%	end,
%%	A.
%%
%%test6(Type) ->
%%	if
%%		Type > 0 ->
%%			A = Type,
%%			A = Type+1;
%%		Type < 0 -> A = Type+2;
%%		true -> A = -1
%%	end,
%%	A.
%%
%%test7(Type) ->
%%	receive
%%		{a} -> A = Type;
%%		{b} -> A = Type+1;
%%		{c} -> A = Type+2
%%	end,
%%	A = Type + 3.


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
			end;
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

do_chat_private1(DataIn, GoodsList, RouterType, ChatState) ->
	#chat_role_state{role_id = RoleID, gateway_pid = PID} = ChatState,
	#m_chat_channel_tos{
		to_role_id = ToRoleID,
		msg = Msg
	} = DataIn,
	IsShowGoods = (RouterType =:= chat_show_goods),
	IsShield = is_chat_shield(RoleID, DataIn),
	IsCross = mod_chat_misc:is_in_cross(ChatState),
	case catch mod_chat_verify:check_private(RoleID, ToRoleID, Msg, IsShowGoods, IsCross, ChatState) of
		ok ->
			{ok, FromRoleChatInfo} = mod_chat_misc:get_chat_role_info(?CHANNEL_TYPE_PAIRS, RoleID),
			if
				IsCross =:= true ->
					NewState = mod_chat_misc:set_chat_time(?CHANNEL_TYPE_PAIRS, ChatState);
				true ->
					case mod_role_friend:is_friend(RoleID, ToRoleID) andalso mod_chat_misc:get_chat_role_info(?CHANNEL_TYPE_PAIRS, ToRoleID) of
						{ok, ToRoleChatInfo} ->
							IsSendToRole = not IsShield,
							do_chat_private2(RoleID, DataIn, GoodsList, FromRoleChatInfo, ToRoleChatInfo, IsSendToRole, PID),
							NewState = mod_chat_misc:set_chat_time(?CHANNEL_TYPE_PAIRS, ChatState),
							%%jgrl_role:run(RoleID, fun() -> mod_role_limit:add_limit_times(RoleID, ?LIMIT_TYPE_1) end),
							mod_chat_logger:log_private(FromRoleChatInfo, ToRoleChatInfo, Msg, IsShield);
						_ ->
							case mod_chat_misc:get_chat_cross_friend_info(RoleID, ToRoleID) of
								{ok, ToRoleChatInfo} ->
									IsSendToRole = not IsShield,
									case mod_role_friend:is_open_cross_chat(RoleID) of
										?TRUE ->
											do_chat_cross_private(RoleID, DataIn, GoodsList, FromRoleChatInfo, ToRoleChatInfo, IsSendToRole, PID),
											NewState = mod_chat_misc:set_chat_time(?CHANNEL_TYPE_PAIRS, ChatState),
											mod_chat_logger:log_private(FromRoleChatInfo, ToRoleChatInfo, Msg, IsShield);
										_ ->
											NewState = ChatState,
											NeedYueka = cfg_misc_config:cross_pvp_chat_need_yeuka_type(),
											#cfg_fuli_yueka{name = Name} = cfg_fuli_yueka:find(NeedYueka),
											?UNICAST_TOC(#m_common_error_toc{err_code = ?ERR_FRIEND_CROSS_CHAT_NEED_YUEKA,params = [Name]})
									end;
								_ ->
									NewState = ChatState,
									?UNICAST_TOC(#m_common_error_toc{err_code = ?ERR_FRIEND_NOT_SAME_FRIEND})
							end
					end
			end;
		#error{} = Error ->
			is_pid(PID) andalso ?_common_error(Error),
			NewState = ChatState
	end,
	NewState.