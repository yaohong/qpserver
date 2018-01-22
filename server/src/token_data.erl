%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 18:36
%%%-------------------------------------------------------------------
-module(token_data).
-author("yaohong").

%% API
-export([new/1]).
-export([
	kick/1,
	compare/2,

	value/1,

	send_bin/2
]).

new(QpUserPid) when is_pid(QpUserPid) ->
	{?MODULE, [QpUserPid, erlang:make_ref()]}.




kick({?MODULE, [QpUserPid, _]}) ->
	catch QpUserPid ! kick.

value({?MODULE, Value}) -> Value.


send_bin(Bin, {?MODULE, [QpUserPid, _]}) ->
	catch QpUserPid ! {bin, Bin}.


compare(TokenData,{?MODULE, RValue}) ->
	LValue = TokenData:value(),
	compare_1(LValue, RValue).

compare_1([], []) -> true;
compare_1([V|T1], [V|T2]) -> compare_1(T1, T2);
compare_1(_, _) -> false.

