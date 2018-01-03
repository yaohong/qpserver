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

%% API
-export([
  generate_card/1, generate_card/2,
  parse_card/1
]).

-export([
  sort/1
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
sort(CardList) when is_list(CardList) ->
  ok.