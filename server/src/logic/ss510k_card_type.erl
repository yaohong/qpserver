%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 一月 2018 20:40
%%%-------------------------------------------------------------------
-module(ss510k_card_type).
-author("yaohong").
-include("ss510k.hrl").
%% API
-export([test_get_playcard_type/1, print_playcard_type/1]).
-export([get_playcard_type/1]).
-export([
	is_dui/1,
	is_san/1,
	is_510k/2,
	is_bomb/1
]).

%%打印出牌的结果
print_playcard_type({?CARD_DAN, V}) -> io:format("没有癞子-单牌-~ts~n", [ss510k_util:card_value_format(V)]);
print_playcard_type({?CARD_DUI, {V, Count}}) -> io:format("没有癞子-~p对-最大对[~ts]~n", [Count, ss510k_util:card_value_format(V)]);
print_playcard_type({?CARD_SAN, {V, Count}}) -> io:format("没有癞子-~p个三条-最大三条[~ts]~n", [Count, ss510k_util:card_value_format(V)]);
print_playcard_type({?CARD_510K, {true, ColorValue}}) -> io:format("没有癞子-同花510k-花色[~ts]~n", [ss510k_util:card_color_format(ColorValue)]);
print_playcard_type({?CARD_510K, false}) -> io:format("没有癞子-510k~n", []);
print_playcard_type({?CARD_BOMB, {V, Count}}) -> io:format("没有癞子-[~p]炸-~ts~n", [Count, ss510k_util:card_value_format(V)]);
print_playcard_type({?CARD_TIANWANG, none}) -> io:format("四大天王~n", []);
print_playcard_type(L) when is_list(L) ->
	lists:foreach(
		fun(Item) ->
			print_playcard_type_1(Item)
		end, L).
print_playcard_type_1({{?CARD_DAN, V}, LaiziInfo}) ->
	io:format("[~p]个癞子-单牌-~ts-~ts~n", [length(LaiziInfo), ss510k_util:card_value_format(V), format_laizi_replace_info(LaiziInfo)]);
print_playcard_type_1({{?CARD_DUI, {V, Count}}, LaiziInfo}) ->
	io:format("[~p]个癞子-~p对-最大对[~ts]-~ts~n", [length(LaiziInfo), Count, ss510k_util:card_value_format(V), format_laizi_replace_info(LaiziInfo)]);
print_playcard_type_1({{?CARD_SAN, {V, Count}}, LaiziInfo}) ->
	io:format("[~p]个癞子-~p个三条-最大三条[~ts]-~ts~n", [length(LaiziInfo), Count, ss510k_util:card_value_format(V), format_laizi_replace_info(LaiziInfo)]);
print_playcard_type_1({{?CARD_510K, {true, ColorValue}}, LaiziInfo}) ->
	io:format("[~p]个癞子-同花510k-花色[~ts]-~ts~n", [length(LaiziInfo), ss510k_util:card_color_format(ColorValue), format_laizi_replace_info(LaiziInfo)]);
print_playcard_type_1({{?CARD_510K, false}, LaiziInfo}) ->
	io:format("[~p]个癞子-510k-~ts~n", [length(LaiziInfo), format_laizi_replace_info(LaiziInfo)]);
print_playcard_type_1({{?CARD_BOMB, {V, Count}}, LaiziInfo}) ->
	io:format("[~p]个癞子-[~p]炸-~ts-~ts~n", [length(LaiziInfo), Count, ss510k_util:card_value_format(V), format_laizi_replace_info(LaiziInfo)]).

format_laizi_replace_info([Card]) ->
	{C, V} = ss510k_util:parse_card(Card),
	"癞子替换成" ++ ss510k_util:card_color_format(C) ++ ss510k_util:card_value_format(V);
format_laizi_replace_info([Card1, Card2]) ->
	{C1, V1} = ss510k_util:parse_card(Card1),
	{C2, V2} = ss510k_util:parse_card(Card2),
	"癞子替换成" ++ ss510k_util:card_color_format(C1) ++ ss510k_util:card_value_format(V1) ++ "和"
		++ ss510k_util:card_color_format(C2) ++ ss510k_util:card_value_format(V2).

test_get_playcard_type(CVList) ->
	L =
	lists:map(
		fun(CV) ->
			ss510k_util:generate_card(CV)
		end, CVList),
	io:format("~ts~n", [ss510k_util:card_format(ss510k_util:card_asc_sort(L))]),
	statistics(wall_clock),
	V = get_playcard_type(L),
	{_, Time} = statistics(wall_clock),
	io:format("耗时~p~n", [Time]),
	print_playcard_type(V).

%%出的牌从小到大排序
get_playcard_type(CardList) ->
	%%先把癞子取出来
	{NewCardList, CardLaiziList} = ss510k_util:delete_card(?VALUE_LAIZI, CardList),
	LaiziCount = length(CardLaiziList),
	CardCount = length(NewCardList),
	if
		CardCount =:= 0 ->
			%%没有出牌
			failed;
		CardCount > 0 ->
			SortCardList = ss510k_util:card_asc_sort(NewCardList),
			CardValueList = lists:map(fun(TmpCard) -> {_, V} = ss510k_util:parse_card(TmpCard), V end, SortCardList),
			CardColorList = lists:map(fun(TmpCard) -> {C, _} = ss510k_util:parse_card(TmpCard), C end, SortCardList),
			IsTonghua = is_tonghua(CardColorList),
			if
				LaiziCount =:= 0 ->
					%%没有癞子
					get_type(CardValueList, IsTonghua);
				LaiziCount =:= 1 ->
					get_type_1(CardValueList, IsTonghua);
				LaiziCount =:= 2->
					case CardValueList of
						[?VALUE_DA, ?VALUE_DA] -> {?CARD_TIANWANG, none};
						_ -> get_type_2(CardValueList, IsTonghua)
					end
			end
	end.


is_tonghua(L) ->
	is_equal(L).

is_equal([A]) -> {true, A};
is_equal([A, B]) when A =:= B -> {true, A};
is_equal([A, B|T]) when A =:= B -> is_equal([B|T]);
is_equal(_) -> false.

get_type(CardValueList, IsTonghua) ->
	case is_dan(CardValueList) of
		no ->
			case is_510k(CardValueList, IsTonghua) of
				no ->
					case is_bomb(CardValueList) of
						no ->
							case is_dui(CardValueList) of
								no ->
									case is_san(CardValueList) of
										no -> failed;
										ReplySan -> ReplySan
									end;
								ReplyDui -> ReplyDui
							end ;
						ReplyBomb -> ReplyBomb
					end;
				Reply510k -> Reply510k
			end;
		ReplyDan -> ReplyDan
	end.
get_type_1(CardValueList, IsTonghua) ->
	get_type_1(CardValueList, IsTonghua, ?VALID_VALUE_LIST, []).
get_type_1(_, _, [], ReplyValueLList) -> ReplyValueLList;
get_type_1(CardValueList, IsTonghua, [Laizi|T], ReplyValueLList) ->
	SortCardList = ss510k_util:card_asc_sort([Laizi|CardValueList]),
	ColorValue =
		case IsTonghua of
			{true, CV} -> CV;
			false -> ?COLOR_HEITAO
		end,
	NewReplyValueLList =
	case get_type(SortCardList, IsTonghua) of
		failed -> ReplyValueLList;
		ReplyValue -> [{ReplyValue, [ss510k_util:generate_card(ColorValue, Laizi)]}|ReplyValueLList]
	end,
	get_type_1(CardValueList, IsTonghua, T, NewReplyValueLList).

get_type_2(CardValueList, IsTonghua) ->
	get_type_2(CardValueList, IsTonghua, ?VALID_VALUE_LIST, ?VALID_VALUE_LIST, []).
get_type_2(_, _, _,[], ReplyValueLList) -> ReplyValueLList;
get_type_2(CardValueList, IsTonghua, [],[_|T], ReplyValueLList) ->
	get_type_2(CardValueList, IsTonghua, ?VALID_VALUE_LIST, T, ReplyValueLList);
get_type_2(CardValueList, IsTonghua, [Laizi1|T1],[Laiz2|_] = T2, ReplyValueLList) ->
	SortCardList = ss510k_util:card_asc_sort([Laizi1, Laiz2|CardValueList]),
	ColorValue =
		case IsTonghua of
			{true, CV} -> CV;
			false -> ?COLOR_HEITAO
		end,
	NewReplyValueLList =
	case get_type(SortCardList, IsTonghua) of
		failed -> ReplyValueLList;
		ReplyValue -> [{ReplyValue, [ss510k_util:generate_card(ColorValue, Laizi1), ss510k_util:generate_card(ColorValue, Laiz2)]}|ReplyValueLList]
	end,
	get_type_2(CardValueList, IsTonghua, T1, T2, NewReplyValueLList).
%%
is_dan([V]) ->
	%%一张牌
	{?CARD_DAN, V};
is_dan(_) -> no.


%%判断是否是对子
is_dui([V, V]) ->
	{?CARD_DUI, {V, 1}};
is_dui([V, V|T]) when V < ?VALUE_A ->
	is_dui_1(T, V+1, 1);
is_dui(_) -> no.

is_dui_1([V, V], V, CurDuiCount) when V =< ?VALUE_A ->
	{?CARD_DUI, {V, CurDuiCount + 1}};
is_dui_1([V, V|T], V, CurDuiCount) when V < ?VALUE_A ->
	is_dui_1(T, V+1, CurDuiCount + 1);
is_dui_1(_, _, _)  ->
	no.

%%判断是否是三条
is_san([V, V, V]) ->
	{?CARD_SAN, {V, 1}};
is_san([V, V, V|T]) when V < ?VALUE_A ->
	is_san_1(T, V + 1, 1);
is_san(_) -> no.

is_san_1([V,V,V], V, CurCount) when V =< ?VALUE_A  ->
	{?CARD_SAN, {V, CurCount + 1}};
is_san_1([V,V,V|T], V, CurCount) when V < ?VALUE_A ->
	is_san_1(T, V + 1, CurCount + 1);
is_san_1(_, _, _) ->
	no.


is_510k([?VALUE_5, ?VALUE_10, ?VALUE_K], IsTonghua) -> {?CARD_510K ,IsTonghua};
is_510k(_, _) -> no.

is_bomb([V, V, V, V]) -> {?CARD_BOMB, {V, 4}};
is_bomb([V, V, V, V, V]) -> {?CARD_BOMB, {V, 5}};
is_bomb([V, V, V, V, V, V]) -> {?CARD_BOMB, {V, 6}};
is_bomb([V, V, V, V, V, V, V]) -> {?CARD_BOMB, {V, 7}};
is_bomb([V, V, V, V, V, V, V, V]) -> {?CARD_BOMB, {V, 8}};
is_bomb([V, V, V, V, V, V, V, V, V]) -> {?CARD_BOMB, {V, 9}};
is_bomb([V, V, V, V, V, V, V, V, V, V]) -> {?CARD_BOMB, {V, 10}};
is_bomb(_) -> no.



%%有癞子的情况下不可能是单牌
%%癞子可以代3-4-5-6-7-8-9-10-J-Q-K-A-2-大王 14张牌





