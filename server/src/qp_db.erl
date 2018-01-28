%%%---------------------------------------------avatar_url----------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 十月 2016 17:44
%%%-------------------------------------------------------------------
-module(qp_db).
-author("yaohong").
-include("../deps/yhsql/include/yhsql.hrl").
-include("../deps/file_log/include/file_log.hrl").
-include("db_define.hrl").
%% API

-export([account_verify/2, account_verify/3]).
-export([load_game_data/1]).

account_verify(Acc, Pwd) ->
	account_verify(Acc, Pwd, true).
account_verify(Acc, Pwd, IsCreate) ->
	SelectSql = "select user_id, pwd from account where `acc`='" ++ Acc ++ "'",
	case yhsql:fetch(sql:pool_name(), SelectSql) of
		{data, #yhsql_result{rows = []}} ->
			if
				IsCreate =:= true ->
					InsertSql = yhsql_util:insert_query("account", ["acc", "pwd"], [Acc, Pwd]),
					case yhsql:fetch(sql:pool_name(), InsertSql) of
						{updated, #yhsql_result{affectedrows = 1, insertid = UserId}} ->
							{success, UserId, true};
						Other1 ->
							?FILE_LOG_ERROR("~p", [Other1]),
							{failed, ?DB_SYSTEM_ERROR}
					end;
				true -> {failed, ?DB_ACCOUNT_NOT_EXIST}
			end;
		{data, #yhsql_result{rows = [UserData]}} ->
			[UserId, DbPwd] = UserData,
			case pwd_compare(qp_util:to_binary(Pwd), qp_util:to_binary(DbPwd)) of
				true -> {success, qp_util:to_integer(UserId), false};
				false -> {failed, ?DB_PWD_ERROR}
			end;
		Other ->
			?FILE_LOG_ERROR("~p", [Other]),
			{failed, ?DB_SYSTEM_ERROR}
	end.

pwd_compare(PWD, PWD) -> true;
pwd_compare(_, _) -> false.



load_game_data(UserId) ->
	load_game_data(UserId, true).
load_game_data(UserId, IsCreate) ->
	SelectSql = "select room_card, nickname, avatar_url from game_data where `user_id`=" ++ qp_util:to_list(UserId),
	case yhsql:fetch(sql:pool_name(), SelectSql) of
		{data, #yhsql_result{rows = []}} ->
			if
				IsCreate =:= true ->
					InsertSql = yhsql_util:insert_query("game_data",
						["user_id", "room_card", "nickname", "avatar_url"],
						[qp_util:to_binary(UserId), <<"10">>, qp_util:to_binary(UserId), <<"">>]),
					case yhsql:fetch(sql:pool_name(), InsertSql) of
						{updated, #yhsql_result{affectedrows = 1}} ->
							{success, {10, qp_util:to_list(UserId), ""}, true};
						Other1 ->
							?FILE_LOG_ERROR("~p", [Other1]),
							{failed, ?DB_SYSTEM_ERROR}
					end;
				true -> {failed, ?DB_GAMEDATA_NOT_EXIST}
			end;
		{data, #yhsql_result{rows = [DbGameData]}} ->
			[DbRoomCard, DbNickname, DbAvatarUrl] = DbGameData,
			{success, {qp_util:to_integer(DbRoomCard), qp_util:to_list(DbNickname), qp_util:to_list(DbAvatarUrl)}, false};
		Other ->
			?FILE_LOG_ERROR("~p", [Other]),
			{failed, ?DB_SYSTEM_ERROR}
	end.