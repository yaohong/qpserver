%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 一月 2018 上午11:36
%%%-------------------------------------------------------------------
-author("yaohong").


-ifndef(_ss510k_h__).
-define(_ss510k_h__, 0).



-define(CARD_DAN, 1).         %%单牌
-define(CARD_DUI, 2).         %%对子
-define(CARD_SAN, 3).         %%三条
-define(CARD_510K, 4).        %%510k
-define(CARD_BOMB, 5).      %%炸弹
-define(CARD_TIANWANG, 6).    %%四大天王



%%牌的花色(方块,梅花,红桃,黑桃)
-define(COLOR_FANGKUAI, 1).
-define(COLOR_MEIHUA, 2).
-define(COLOR_HONGTAO, 3).
-define(COLOR_HEITAO, 4).

-define(COLOR_DA, 5).    %%大王
-define(COLOR_LAIZI, 6).    %%癞子

%%扑克的数值
-define(VALUE_3, 3).
-define(VALUE_4, 4).
-define(VALUE_5, 5).
-define(VALUE_6, 6).
-define(VALUE_7, 7).
-define(VALUE_8, 8).
-define(VALUE_9, 9).
-define(VALUE_10, 10).
-define(VALUE_J, 11).
-define(VALUE_Q, 12).
-define(VALUE_K, 13).
-define(VALUE_A, 14).
-define(VALUE_2, 15).
%%-define(VALUE_XIAO, 16).
-define(VALUE_DA, 16).






-endif.