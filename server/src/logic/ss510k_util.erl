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
%% API
-export([
	generate_card/1, generate_card/2,
	parse_card/1
]).

-export([
	sort/2,
	card_format/1
]).


%%每一张牌由一个16位的无符号整数构成,高8位存储类型,低8位存储数值

generate_card({Color, Value}) ->
	generate_card(Color, Value).
generate_card(Color, Value) ->
	(Color bsl 8) bor Value.


parse_card(Card) when is_integer(Card) ->
	Value = Card band 2#00001111,
	Color = (Card band 2#11110000) bsr 8,
	{Color, Value}.




%%给手牌排序
sort(CardList, ?ST_NORMAL) when is_list(CardList) ->
	SortFunc =
		fun(L, R) ->
			{_, LV} = parse_card(L),
			{_, RV} = parse_card(R),
			LV > RV
		end,
	lists:sort(SortFunc, CardList);
sort(CardList, ?ST_BOMB) when is_list(CardList) ->
	ok.


card_format(CardList) when is_list(CardList) ->
	ValueList =
		lists:map(
			fun(Card) ->
				{Color, Value} = parse_card(Card),
				[card_color_format(Color), "-", card_value_format(Value)]
			end, CardList),
	lists:flatten(ValueList).

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