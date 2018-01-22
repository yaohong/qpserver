%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 0:05
%%%-------------------------------------------------------------------
-author("yaohong").
-ifndef(_qp_error_h__).
-define(_qp_error_h__, 0).

-include("../deps/file_log/include/file_log.hrl").

-define(printSystemError(What, Reason), ?FILE_LOG_ERROR("~p,~p,~p", [What, Reason, erlang:get_stacktrace()])).
-define(SYSTEM_ERROR, -1).
-define(SYSTEM_TRY_AGAIN, 1000).      %%进行中重试
-define(SYSTEM_TOKEN_ERROR, 1001).      %%进行中重试


-define(LOGIC_ERROR_REPEAT_JOIN, 10001).      %%重复进入房间
-define(LOGIC_ERROR_NOT_IN_ROOM_NOT_SITDOWN, 10002).       %%不在房间不能坐下
-define(LOGIC_ERROR_NOT_OB_NOT_SITDOWN, 10003).            %%不在OB状态下,不能坐下
-define(LOGIC_ERROR_SEAT_FULL, 10004).                     %%座位满了
-define(LOGIC_ERROR_SEAT_SOMEONE, 10005).                  %%座位上有人了
-define(LOGIC_ERROR_SEATNUM_EXCEPTION, 10006).             %%座位号异常
-define(LOGIC_ERROR_SEAT_NOT_SELF, 10007).                 %%座位上的人不是自己，不能起立
-define(LOGIC_ERROR_NOT_OB, 10008).                        %%当前不在OB位
-define(LOGIC_ERROR_CARD_NOT_ENOUGH, 10009).               %%房卡不够
-define(LOGIC_ERROR_CREATE_ROOM_FAILED, 10010).               %%创建房间失败
-define(LOGIC_ERROR_JOIN_ROOM_FAILED, 10011).               %%创建房间失败
-endif.