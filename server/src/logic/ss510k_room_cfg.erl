%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2018 13:54
%%%-------------------------------------------------------------------
-module(ss510k_room_cfg).
-author("yaohong").

%% API
-export([
	new/9,
	get/2
	]).


new(RoomName, AA, DoubleDownScore, IsLaiziPlayMethod, IsOb, IsRandom, IsNotVoice, IsSafeMode, LockIdList)
	when is_list(RoomName) andalso is_boolean(AA) andalso is_integer(DoubleDownScore) andalso is_boolean(IsLaiziPlayMethod)
	andalso is_boolean(IsOb) andalso is_boolean(IsRandom) andalso is_boolean(IsNotVoice) andalso is_boolean(IsSafeMode) andalso is_list(LockIdList)  ->
	{?MODULE, [RoomName, AA, DoubleDownScore, IsLaiziPlayMethod, IsOb, IsRandom, IsNotVoice,IsSafeMode, LockIdList]}.

get(name, {?MODULE, [RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	RoomName;
get(aa, {?MODULE, [_RoomName, AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	AA;
get(double_down_score, {?MODULE, [_RoomName, _AA, DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	DoubleDownScore;
get(is_laizi_playmethod, {?MODULE, [_RoomName, _AA, _DoubleDownScore, IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	IsLaiziPlayMethod;
get(is_ob, {?MODULE, [_RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	IsOb;
get(is_random, {?MODULE, [_RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, IsRandom, _IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	IsRandom;
get(is_not_voice, {?MODULE, [_RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, IsNotVoice, _IsSafeMode, _LockIdList]}) ->
	IsNotVoice;
get(is_safe_mode, {?MODULE, [_RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, IsSafeMode, _LockIdList]}) ->
	IsSafeMode;
get(lock_id_list, {?MODULE, [_RoomName, _AA, _DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode, LockIdList]}) ->
	LockIdList.

