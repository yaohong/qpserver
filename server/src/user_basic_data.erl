%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2018 20:09
%%%-------------------------------------------------------------------
-module(user_basic_data).
-author("yaohong").

%% API
-export([
	new/4,
	get/2,

	send_room_msg/2
]).


new(Id, Pid, NickName, AvatarUrl) when is_integer(Id) andalso is_pid(Pid) andalso is_list(NickName) andalso is_list(AvatarUrl) ->
	{?MODULE, [Id, Pid, NickName, AvatarUrl]}.


get(id, {?MODULE, [Id, _, _NickName, _AvatarUrl]}) ->
	Id;
get(nickname, {?MODULE, [_Id, _, NickName, _AvatarUrl]}) ->
	NickName;
get(avatar_url, {?MODULE, [_Id, _, _NickName, AvatarUrl]}) ->
	AvatarUrl.

send_room_msg(Msg, {?MODULE, [_, Pid, _NickName, _AvatarUrl]}) ->
	Pid ! {room_msg, Msg}.