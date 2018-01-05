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
	sort/2,
	card_format/1,
	test_print/0
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
	%%炸弹优先排序
	
	ok.


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
	L11 = ss510k_util:sort(L1, ?ST_NORMAL),
	L12 = ss510k_util:sort(L2, ?ST_NORMAL),
	L13 = ss510k_util:sort(L3, ?ST_NORMAL),
	L14 = ss510k_util:sort(L4, ?ST_NORMAL),
	io:format("~ts~n", [card_format(L11)]),
	io:format("~ts~n", [card_format(L12)]),
	io:format("~ts~n", [card_format(L13)]),
	io:format("~ts~n", [card_format(L14)]).