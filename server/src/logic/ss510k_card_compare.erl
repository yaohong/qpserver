%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 一月 2018 15:46
%%%-------------------------------------------------------------------
-module(ss510k_card_compare).
-author("yaohong").
-include("ss510k.hrl").

%% API
-export([
	test/2,
	compare/2
]).

test(L, R) ->
	LCardList =
		lists:map(
			fun(LItem) ->
				ss510k_util:generate_card(LItem)
			end, L),

	RCardList =
		lists:map(
			fun(RItem) ->
				ss510k_util:generate_card(RItem)
			end, R),
	io:format("左边的牌: ~ts~n", [ss510k_util:card_format(ss510k_util:card_asc_sort(LCardList))]),
	io:format("右边的牌: ~ts~n", [ss510k_util:card_format(ss510k_util:card_asc_sort(RCardList))]),
	compare(LCardList, RCardList).

%%都是没有癞子的(除了四大天王)(左边大于右边为ture, 否则为false)
compare(LCardList, RCardList) ->
	LCardType = ss510k_card_type:get_playcard_type(LCardList),
	RCardType = ss510k_card_type:get_playcard_type(RCardList),
	compare_type(LCardType, RCardType).


compare_type({?CARD_DAN, LV}, {?CARD_DAN, RV}) -> LV > RV;
compare_type({?CARD_DAN, _}, {?CARD_DUI, _}) -> not_compare;
compare_type({?CARD_DAN, _}, {?CARD_SAN, _}) -> not_compare;
compare_type({?CARD_DAN, _}, {?CARD_510K, _}) -> false;
compare_type({?CARD_DAN, _}, {?CARD_BOMB, _}) -> false;
compare_type({?CARD_DAN, _}, {?CARD_TIANWANG, _}) -> false;

compare_type({?CARD_DUI, _}, {?CARD_DAN, _}) -> not_compare;
compare_type({?CARD_DUI, {LV, LCount}}, {?CARD_DUI, {RV, RCount}}) ->
	if
		LCount =/= RCount -> not_compare;
		true ->
			LV > RV
	end;
compare_type({?CARD_DUI, _}, {?CARD_SAN, _}) -> not_compare;
compare_type({?CARD_DUI, _}, {?CARD_510K, _}) -> false;
compare_type({?CARD_DUI, _}, {?CARD_BOMB, _}) -> false;
compare_type({?CARD_DUI, _}, {?CARD_TIANWANG, _}) -> false;

compare_type({?CARD_SAN, _}, {?CARD_DAN, _}) -> not_compare;
compare_type({?CARD_SAN, _}, {?CARD_DUI, _}) -> not_compare;
compare_type({?CARD_SAN, {LV, LCount}}, {?CARD_SAN, {RV, RCount}}) ->
	if
		LCount =/= RCount -> not_compare;
		true ->
			LV > RV
	end;
compare_type({?CARD_SAN, _}, {?CARD_510K, _}) -> false;
compare_type({?CARD_SAN, _}, {?CARD_BOMB, _}) -> false;
compare_type({?CARD_SAN, _}, {?CARD_TIANWANG, _}) -> false;

compare_type({?CARD_510K, _}, {?CARD_DAN, _}) -> true;
compare_type({?CARD_510K, _}, {?CARD_DUI, _}) -> true;
compare_type({?CARD_510K, _}, {?CARD_SAN, _}) -> true;
compare_type({?CARD_510K, {true, _}}, {?CARD_510K, false}) -> true;
compare_type({?CARD_510K, {true, _}}, {?CARD_510K, {true, _}}) -> false;
compare_type({?CARD_510K, _}, {?CARD_510K, _}) -> false;
compare_type({?CARD_510K, _}, {?CARD_BOMB, _}) -> false;
compare_type({?CARD_510K, _}, {?CARD_TIANWANG, _}) -> false;

compare_type({?CARD_BOMB, _}, {?CARD_DAN, _}) -> true;
compare_type({?CARD_BOMB, _}, {?CARD_DUI, _}) -> true;
compare_type({?CARD_BOMB, _}, {?CARD_SAN, _}) -> true;
compare_type({?CARD_BOMB, _}, {?CARD_510K, _}) -> true;
compare_type({?CARD_BOMB, {LV, LCount}}, {?CARD_BOMB, {RV, RCount}}) ->
	if
		LCount > RCount -> true;
		LCount =:= RCount ->
			LV > RV;
		LCount < RCount -> false
	end;
compare_type({?CARD_BOMB, {_, LCount}}, {?CARD_TIANWANG, _}) ->
	%%四大天王只比八个小
	LCount >= 8;


compare_type({?CARD_TIANWANG, _}, {?CARD_BOMB, {_, Count}}) when Count >= 8 -> false;
compare_type({?CARD_TIANWANG, _}, _) -> true.
%%四大天王只比八个小



