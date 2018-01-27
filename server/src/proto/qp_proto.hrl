%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2016 21:17
%%%-------------------------------------------------------------------
-author("yaohong").
-ifndef(_qp_proto_h__).
-define(_qp_proto_h__, 1).
-define(CMD_QP_LOGIN_REQ, 1001).
-define(CMD_QP_LOGIN_RSP, 1002).

-define(CMD_QP_CREATE_ROOM_REQ, 1003).
-define(CMD_QP_CREATE_ROOM_RSP, 1004).

-define(CMD_QP_JOIN_ROOM_REQ, 1005).
-define(CMD_QP_JOIN_ROOM_RSP, 1006).
-define(CMD_QP_JOIN_ROOM_PUSH, 1007).


-define(CMD_QP_SITDOWN_REQ, 1008).
-define(CMD_QP_SITDOWN_RSP, 1009).
-define(CMD_QP_SITDOWN_PUSH, 1010).

-define(CMD_QP_STANDUP_REQ, 1011).
-define(CMD_QP_STANDUP_RSP, 1012).
-define(CMD_QP_STANDUP_PUSH, 1013).

-define(CMD_QP_EXIT_ROOM_REQ, 1014).
-define(CMD_QP_EXIT_ROOM_RSP, 1015).
-define(CMD_QP_EXIT_ROOM_PUSH, 1016).
-define(CMD_QP_ROOM_DISSMISS, 1017).
-define(CMD_QP_ROOM_KICK, 1018).
-define(CMD_QP_USER_ONLINE_STATE_CHANGE, 1019).

-define(CMD_QP_GAME_DATA, 2001).

-define(CMD_QP_PING_REQ, 5001).
-define(CMD_QP_PING_RSP, 5002).

-endif.