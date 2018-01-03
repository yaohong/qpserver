%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 一月 2018 上午11:35
%%%-------------------------------------------------------------------
-module(ss510k_game_before).
-author("yaohong").
-include("ss510k.hrl").
%% API
-export([
  init_card_pool/0,
  deal/1
]).


%%初始化牌库
init_card_pool() ->
  L =
    [
      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_3), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_3),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_3), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_3),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_3), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_3),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_3), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_3),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_4), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_4),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_4), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_4),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_4), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_4),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_4), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_4),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_5), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_5),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_5), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_5),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_5), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_5),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_5), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_5),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_6), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_6),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_6), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_6),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_6), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_6),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_6), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_6),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_7), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_7),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_7), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_7),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_7), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_7),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_7), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_7),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_8), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_8),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_8), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_8),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_8), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_8),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_8), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_8),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_9), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_9),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_9), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_9),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_9), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_9),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_9), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_9),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_10), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_10),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_10), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_10),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_10), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_10),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_10), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_10),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_J), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_J),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_J), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_J),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_J), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_J),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_J), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_J),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_Q), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_Q),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_Q), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_Q),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_Q), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_Q),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_Q), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_Q),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_K), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_K),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_K), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_K),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_K), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_K),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_K), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_K),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_A), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_A),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_A), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_A),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_A), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_A),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_A), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_A),

      ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_2), ss510k_util:generate_card(?COLOR_FANGKUAI, ?VALUE_2),
      ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_2), ss510k_util:generate_card(?COLOR_MEIHUA, ?VALUE_2),
      ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_2), ss510k_util:generate_card(?COLOR_HONGTAO, ?VALUE_2),
      ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_2), ss510k_util:generate_card(?COLOR_HEITAO, ?VALUE_2),

      ss510k_util:generate_card(?COLOR_DA, ?VALUE_DA), ss510k_util:generate_card(?COLOR_DA, ?VALUE_DA),
      ss510k_util:generate_card(?COLOR_LAIZI, 0), ss510k_util:generate_card(?COLOR_LAIZI, 0)
    ],
  %%混乱排序
  L2 = [{Card, qp_util:random_in_range(1000000, 9999999)} || Card <- L],
  SortFunc =
    fun(L, R) ->
      {_, LV} = L,
      {_, RV} = R,
      LV > RV
    end,
  lists:sort(SortFunc, L2).


%%
%%给四个位置发牌
deal(CardPool) when is_list(CardPool) ->
  deal(CardPool, [[], [], [], []]).
deal([], [L1, L2, L3, L4]) -> {L1, L2, L3, L4};
deal([Card|TCardList], [L|T]) ->
  deal(TCardList, T ++ [Card|L]).
