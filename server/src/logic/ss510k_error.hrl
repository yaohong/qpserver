%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2018 17:26
%%%-------------------------------------------------------------------
-author("yaohong").

-ifndef(_ss510k_error_h__).
-define(_ss510k_error_h__, 0).

-define(SS510K_ERROR_REPEAT_JOIN, 10001).      %%重复进入房间
-define(SS510K_ERROR_NOT_IN_ROOM_NOT_SITDOWN, 10002).       %%不在房间不能坐下
-define(SS510K_ERROR_NOT_OB_NOT_SITDOWN, 10003).            %%不在OB状态下,不能坐下
-define(SS510K_ERROR_SEAT_FULL, 10004).                     %%座位满了
-define(SS510K_ERROR_SEAT_SOMEONE, 10005).                  %%座位上有人了
-endif.