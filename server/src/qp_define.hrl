%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 18:30
%%%-------------------------------------------------------------------
-author("yaohong").


-ifndef(_qp_define_h__).
-define(_qp_define_h__, 0).

%%-record(token_data, {
%%	user_pid :: pid()
%%}).

%%房间解散的原因
-define(RDT_RAND_END, 1).       %%一轮结束解散
-define(RDT_TIMEOUT_END, 2).    %%超时解散
-define(RDT_ERROR_END, 3).      %%因为错误解散


%%从房间被移除的原因
-define(RT_GAME_START_NOT_OB, 1).  %%游戏开始了不能观看





-endif.