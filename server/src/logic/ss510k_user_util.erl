%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2018 22:02
%%%-------------------------------------------------------------------
-module(ss510k_user_util).
-author("yaohong").
-include("ss510k_user.hrl").
-include("ss510k.hrl").
%% API
-export(
	[
		create_user/2,
		clientcard_prehandle/1
	]
).
-export([
	is_replace_card/1,
	clear_laizi_mark/1
]).

-export([
	test_clientcard_prehandle/1,
	calc_score/1,
	calc_cardtype_score/1
]).


test_clientcard_prehandle(CardList) ->
	L =
	lists:map(
		fun({IsLaizi, Color, Value}) ->
			V = IsLaizi bsl 15,
			Card = ss510k_util:generate_card({Color, Value}),
			V bor Card
		end, CardList),
	{ClearReplaceCardList, Score, ReplaceCardList} = clientcard_prehandle(L),
	S = ss510k_card_type:format_laizi_replace_info(ReplaceCardList),
	ss510k_card_type:print_playcard_type(ss510k_card_type:get_playcard_type(ClearReplaceCardList)),
	io:format("score=~p, ~ts", [Score, S]).




is_replace_card(ClientCard) ->
	(ClientCard bsr 15) =:= 1.
clear_laizi_mark(ClientCard) ->
	ClientCard band 2#011111111111111.

create_user(SeatNumber, HandCardList) when is_integer(SeatNumber) andalso is_list(HandCardList) ->
	#ss510k_user{
		seat_number = SeatNumber,
		hand_card_list = HandCardList,
		the_card_list1 = [],
		the_card_list2 = [],
		current_score = 0,
		total_score = 0
	}.

%%客户端出牌里不会直接有癞子(除非四大天王)，只有癞子替换之后的牌(如果是癞子代的牌,则最高位为1，高八位的低七位表示花色,低8表示卡牌值)
clientcard_prehandle(ClientCardList) ->
	%%查找代牌
	{ClearReplaceCardList, ServerCardList, ReplaceCardList} = find_replace_card(ClientCardList),
	%%根据ClearReplaceCardList来获取牌型
	%%根据ServerCardList来将手牌里的牌移除掉和计算分值
	%%ReplaceCardList 为癞子替换的牌

	%%计算本次出牌的分值(5, 10, K) + 加上炸弹本身的分值
	CardScore = ss510k_user_util:calc_score(ServerCardList),
	%%获取牌型的分值
	CardTypeScore = ss510k_user_util:calc_cardtype_score(ClearReplaceCardList),
	{ClearReplaceCardList, ServerCardList, CardScore + CardTypeScore, ReplaceCardList}.

%%查找癞子替换的牌
find_replace_card(ClientCardList) ->
	find_replace_card(ClientCardList, [], [], []).
find_replace_card([], ClearReplaceCardList, ServerCardList, ReplaceCardList) ->
	{lists:reverse(ClearReplaceCardList), lists:reverse(ServerCardList), ReplaceCardList};
find_replace_card([ClientCard|T], ClearReplaceCardList, ServerCardList, ReplaceCardList) ->
	IsReplaceCard = ss510k_user_util:is_replace_card(ClientCard),
	if
		IsReplaceCard =:= true ->
			%%是癞子替换的牌
			ServerCard = ss510k_user_util:clear_laizi_mark(ClientCard),
			find_replace_card(T, [ServerCard|ClearReplaceCardList], [ss510k_util:generate_card(?COLOR_LAIZI, ?VALUE_LAIZI)|ServerCardList], [ServerCard|ReplaceCardList]);
		IsReplaceCard =:= false ->
			find_replace_card(T, [ClientCard|ClearReplaceCardList], [ClientCard|ServerCardList], ReplaceCardList)
	end.

calc_score(ServerCardList) ->
	calc_score(ServerCardList, 0).
calc_score([], Score) -> Score;
calc_score([ServerCard|T], TotalScore) ->
	CardScore =
		case ss510k_util:parse_card(ServerCard) of
			{_, ?VALUE_5} -> 5;
			{_, ?VALUE_10} -> 10;
			{_, ?VALUE_K} -> 10;
			_ -> 0
		end,
	calc_score(T, TotalScore + CardScore).


calc_cardtype_score(ClearReplaceCardList) ->
	case ss510k_card_type:get_playcard_type(ClearReplaceCardList) of
		{?CARD_DAN, _} -> 0;
		{?CARD_DUI, _} -> 0;
		{?CARD_SAN, _} -> 0;
		{?CARD_510K, _} -> 0;
		{?CARD_BOMB, {_, Count}} ->
			if
				Count =:= 4 -> 0;
				Count =:= 5 -> 200;
				Count =:= 6 -> 500;
				Count =:= 7 -> 1500;
				Count =:= 8 -> 3000;
				Count =:= 9 -> 6000;
				Count =:= 10 -> 12000
			end;
		{?CARD_TIANWANG, _} -> 1500
	end.

