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
	new/6,
	get/2
	]).


new(DoubleDownScore, IsLaiziPlayMethod, IsOb, IsRandom, IsNotVoice, IsSafeMode)
	when is_integer(DoubleDownScore) andalso is_boolean(IsLaiziPlayMethod)
	andalso is_boolean(IsOb) andalso is_boolean(IsRandom) andalso is_boolean(IsNotVoice) andalso is_boolean(IsSafeMode)  ->
	{?MODULE, [DoubleDownScore, IsLaiziPlayMethod, IsOb, IsRandom, IsNotVoice,IsSafeMode]}.


get(double_down_score, {?MODULE, [DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode]}) ->
	DoubleDownScore;
get(is_laizi_playmethod, {?MODULE, [_DoubleDownScore, IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, _IsSafeMode]}) ->
	IsLaiziPlayMethod;
get(is_ob, {?MODULE, [_DoubleDownScore, _IsLaiziPlayMethod, IsOb, _IsRandom, _IsNotVoice, _IsSafeMode]}) ->
	IsOb;
get(is_random, {?MODULE, [_DoubleDownScore, _IsLaiziPlayMethod, _IsOb, IsRandom, _IsNotVoice, _IsSafeMode]}) ->
	IsRandom;
get(is_not_voice, {?MODULE, [_DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, IsNotVoice, _IsSafeMode]}) ->
	IsNotVoice;
get(is_safe_mode, {?MODULE, [_DoubleDownScore, _IsLaiziPlayMethod, _IsOb, _IsRandom, _IsNotVoice, IsSafeMode]}) ->
	IsSafeMode.

