%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%		application进程
%%% @end
%%% Created : 2025/10/24 10:49
%%%-------------------------------------------------------------------
-module(robot_client_app).
-author("caigou").
-behavior(application).
%% API
-export([start/2]).
-export([stop/1,prep_stop/1]).


start(_Type, _Args) ->
	robot_client:start(200,"172.22.2.101",10088).

prep_stop(State) ->
	robot_client:stop(),
	State.

stop(_State) ->
	ok.