%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 一月 2018 下午12:08
%%%-------------------------------------------------------------------
-module(ss510k_util).
-author("yaohong").
-include("ss510k.hrl").
-include("../../deps/file_log/include/file_log.hrl").
%% API
-export([
	generate_card/1, generate_card/2,
	parse_card/1
]).

-export([
	sort/1, sort/2, card_asc_sort/1,
	card_color_format/1, card_value_format/1, card_format/1,
	test_print/0,

	find_card_count/2,
	delete_card/2
]).


%%每一张牌由一个16位的无符号整数构成,高8位存储类型,低8位存储数值

generate_card({Color, Value}) ->
	generate_card(Color, Value).
generate_card(Color, Value) ->
	(Color bsl 8) bor Value.


parse_card(Card) when is_integer(Card) ->
	Value = Card band 2#000000001111111,
	Color = (Card band 2#1111111100000000) bsr 8,
	{Color, Value}.




%%给手牌排序
sort(CardList) when is_list(CardList) ->
	sort(CardList, ?ST_NORMAL).
sort(CardList, ?ST_NORMAL) when is_list(CardList) ->
	SortFunc =
		fun(L, R) ->
			{LC, LV} = parse_card(L),
			{RC, RV} = parse_card(R),
			if
				LV =:= RV ->
					LC > RC;
				true ->
					LV > RV
			end
		end,
	lists:sort(SortFunc, CardList);
sort(CardList, ?ST_BOMB) when is_list(CardList) ->
	{NewCardList, ReplyCardList} = delete_card(?VALUE_LAIZI, CardList),
	{NewCardList1, CardBombList} = extract_bomb(NewCardList),
	{NewCardList2, Card510KList} = extract_510k(NewCardList1),
	%%剩下的牌取510k
	%%剩下的牌在做升序排列
	L = lists:flatten([card_asc_sort(lists:flatten(NewCardList2)),Card510KList, CardBombList|ReplyCardList]),
	lists:reverse(L);
sort(CardList, ?ST_510K) when is_list(CardList) ->
	{NewCardList, ReplyCardList} = delete_card(?VALUE_LAIZI, CardList),
	{NewCardList1, Card510KList} = extract_510k(NewCardList),
	{NewCardList2, CardBombList} = extract_bomb(NewCardList1),
	%%剩下的牌取510k
	%%剩下的牌在做升序排列
	L = lists:flatten([card_asc_sort(lists:flatten(NewCardList2)), Card510KList, CardBombList|ReplyCardList]),
	lists:reverse(L).

extract_bomb(CardList) ->
	CardValueList = [?VALUE_2, ?VALUE_A, ?VALUE_K, ?VALUE_Q, ?VALUE_J, ?VALUE_10, ?VALUE_9, ?VALUE_8, ?VALUE_7, ?VALUE_6, ?VALUE_5, ?VALUE_4, ?VALUE_3],
	lists:foldl(
		fun(BombCount, {TmpNewCardList, TmpReplyCardList}) ->
			lists:foldl(
				fun(CardValue, {TmpNewCardList1, TmpReplyCardList1}) ->
					FindCardCount = find_card_count(CardValue, TmpNewCardList1),
					if
						FindCardCount == BombCount ->
							{TmpNewCardList2, DeleteCardList} = delete_card(CardValue, TmpNewCardList1),
							{TmpNewCardList2, [color_asc_sort(DeleteCardList)|TmpReplyCardList1]};
						true -> {TmpNewCardList1, TmpReplyCardList1}
					end
				end, {TmpNewCardList, TmpReplyCardList}, CardValueList)
		end, {CardList, []}, [8, 7, 6, 5, 4]).

extract_510k(CardList) ->
	{NewCardList, Card_5_List} = delete_card(?VALUE_5, CardList),
	{NewCardList1, Card_10_List} = delete_card(?VALUE_10, NewCardList),
	{NewCardList2, Card_K_List} = delete_card(?VALUE_K, NewCardList1),
	extract_510k(color_desc_sort(Card_5_List), color_desc_sort(Card_10_List), color_desc_sort(Card_K_List), NewCardList2, []).

extract_510k([], Card_10_List, Card_K_List, CardList, ReplyCardList) ->
	{lists:flatten([Card_10_List, Card_K_List|CardList]), ReplyCardList};
extract_510k(Card_5_List, [], Card_K_List, CardList, ReplyCardList) ->
	{lists:flatten([Card_5_List, Card_K_List|CardList]), ReplyCardList};
extract_510k(Card_5_List, Card_10_List, [], CardList, ReplyCardList) ->
	{lists:flatten([Card_5_List, Card_10_List|CardList]), ReplyCardList};
extract_510k([Card5|Card_5_List], [Card10|Card_10_List], [CardK|Card_K_List], CardList, ReplyCardList) ->
	extract_510k(Card_5_List, Card_10_List, Card_K_List, CardList, [Card5, Card10, CardK|ReplyCardList]).

%%按花色升序排列(方块-梅花-红桃-黑桃) (值必须相等) 主要是用炸弹的花色排序
color_asc_sort(CardList) ->
	SortFunc =
		fun(L, R) ->
			{LC, V} = parse_card(L),
			{RC, V} = parse_card(R),
			LC < RC
		end,
	lists:sort(SortFunc, CardList).

color_desc_sort(CardList) ->
	SortFunc =
		fun(L, R) ->
			{LC, V} = parse_card(L),
			{RC, V} = parse_card(R),
			LC > RC
		end,
	lists:sort(SortFunc, CardList).

card_asc_sort(CardList) when is_list(CardList) ->
	SortFunc =
		fun(L, R) ->
			{LC, LV} = parse_card(L),
			{RC, RV} = parse_card(R),
			if
				LV =:= RV ->
					LC < RC;
				true ->
					LV < RV
			end
		end,
	lists:sort(SortFunc, CardList).

find_card_count(CardValue, CardList) ->
	find_card_count_1(CardValue, CardList, 0).
find_card_count_1(_, [], CardCount) -> CardCount;
find_card_count_1(CardValue, [Card|T], CardCount) ->
	case ss510k_util:parse_card(Card) of
		{_, CardValue} -> find_card_count_1(CardValue, T, CardCount + 1);
		_ -> find_card_count_1(CardValue, T, CardCount)
	end.


delete_card(CardValue, CardList) ->
	delete_card_1(CardValue, CardList, [], []).
delete_card_1(_, [], NewCardList, DeleteCardList) -> {lists:reverse(NewCardList), DeleteCardList};
delete_card_1(CardValue, [Card|OldCardList], NewCardList, DeleteCardList) ->
	case ss510k_util:parse_card(Card) of
		{_, CardValue} -> delete_card_1(CardValue, OldCardList, NewCardList, [Card|DeleteCardList]);
		_ -> delete_card_1(CardValue, OldCardList, [Card|NewCardList], DeleteCardList)
	end.




card_format(CardList) when is_list(CardList) ->
	ValueList =
		lists:map(
			fun(Card) ->
				{Color, Value} = parse_card(Card),
				card_color_format(Color) ++ "-" ++ card_value_format(Value)
			end, CardList),
	V2 = string:join(ValueList, " "),
	lists:flatten(V2).

card_color_format(?COLOR_FANGKUAI) -> "方块";
card_color_format(?COLOR_MEIHUA) -> "梅花";
card_color_format(?COLOR_HONGTAO) -> "红桃";
card_color_format(?COLOR_HEITAO) -> "黑桃";
card_color_format(?COLOR_DA) -> "大王";
card_color_format(?COLOR_LAIZI) -> "癞子".

card_value_format(?VALUE_3) -> "3";
card_value_format(?VALUE_4) -> "4";
card_value_format(?VALUE_5) -> "5";
card_value_format(?VALUE_6) -> "6";
card_value_format(?VALUE_7) -> "7";
card_value_format(?VALUE_8) -> "8";
card_value_format(?VALUE_9) -> "9";
card_value_format(?VALUE_10) -> "10";
card_value_format(?VALUE_J) -> "J";
card_value_format(?VALUE_Q) -> "Q";
card_value_format(?VALUE_K) -> "K";
card_value_format(?VALUE_A) -> "A";
card_value_format(?VALUE_2) -> "2";
card_value_format(?VALUE_DA) -> "大王";
card_value_format(?VALUE_LAIZI) -> "癞子".


test_print() ->
	L = ss510k_game_before:init_card_pool(),
	{L1, L2, L3, L4} = ss510k_game_before:deal(L),
	statistics(wall_clock),
	L11 = ss510k_util:sort(L1, ?ST_NORMAL),
	L12 = ss510k_util:sort(L2, ?ST_NORMAL),
	L13 = ss510k_util:sort(L3, ?ST_NORMAL),
	L14 = ss510k_util:sort(L4, ?ST_NORMAL),

	BOMB1 = ss510k_util:sort(L1, ?ST_BOMB),
	BOMB2 = ss510k_util:sort(L2, ?ST_BOMB),
	BOMB3 = ss510k_util:sort(L3, ?ST_BOMB),
	BOMB4 = ss510k_util:sort(L4, ?ST_BOMB),

	P510K1 = ss510k_util:sort(L1, ?ST_510K),
	P510K2 = ss510k_util:sort(L2, ?ST_510K),
	P510K3 = ss510k_util:sort(L3, ?ST_510K),
	P510K4 = ss510k_util:sort(L4, ?ST_510K),
	{_, Time} = statistics(wall_clock),
	io:format("~p~n", [Time]),
	io:format("~ts~n~ts~n~ts~n~n", [card_format(L11), card_format(BOMB1), card_format(P510K1)]),
	io:format("~ts~n~ts~n~ts~n~n", [card_format(L12), card_format(BOMB2), card_format(P510K2)]),
	io:format("~ts~n~ts~n~ts~n~n", [card_format(L13), card_format(BOMB3), card_format(P510K3)]),
	io:format("~ts~n~ts~n~ts~n~n", [card_format(L14), card_format(BOMB4), card_format(P510K4)]).