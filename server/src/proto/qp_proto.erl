%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 十月 2016 21:16
%%%-------------------------------------------------------------------
-module(qp_proto).
-author("yaohong").
-include("../../include/common_pb.hrl").
-include("qp_proto.hrl").
-include("../qp_type.hrl").
%% API

-export([decode_qp_packet/1]).
-export([encode_qp_packet/1]).

decode_qp_packet(Bin) ->
    #qp_packet{cmd = Cmd, serialized = Body} = common_pb:decode_qp_packet(Bin),
    decode_qp_packet(Cmd, Body).
decode_qp_packet(?CMD_QP_LOGIN_REQ, Bin) ->
    common_pb:decode_qp_login_req(Bin);
decode_qp_packet(?CMD_QP_CREATE_ROOM_REQ, Bin) ->
    common_pb:decode_qp_create_room_req(Bin);
decode_qp_packet(?CMD_QP_JOIN_ROOM_REQ, Bin) ->
    common_pb:decode_qp_join_room_req(Bin);
decode_qp_packet(?CMD_QP_SITDOWN_REQ, Bin) ->
    common_pb:decode_qp_sitdown_req(Bin);
decode_qp_packet(?CMD_QP_STANDUP_REQ, Bin) ->
    common_pb:decode_qp_standup_req(Bin);
decode_qp_packet(?CMD_QP_EXIT_ROOM_REQ, Bin) ->
    common_pb:decode_qp_exit_room_req(Bin);
decode_qp_packet(?CMD_QP_GAME_DATA, Bin) ->
    common_pb:decode_qp_game_data(Bin);
decode_qp_packet(?CMD_QP_PING_REQ, Bin) ->
    common_pb:decode_qp_ping_req(Bin);
decode_qp_packet(Cmd, _P2) -> throw({unkown_cmd, Cmd}).



encode_qp_packet(Packet) when is_record(Packet, qp_login_rsp)  ->
    encode_qp_packet(?CMD_QP_LOGIN_RSP, common_pb:encode_qp_login_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_create_room_rsp)  ->
    encode_qp_packet(?CMD_QP_CREATE_ROOM_RSP, common_pb:encode_qp_create_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_join_room_rsp)  ->
    encode_qp_packet(?CMD_QP_JOIN_ROOM_RSP, common_pb:encode_qp_join_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_join_room_push)  ->
    encode_qp_packet(?CMD_QP_JOIN_ROOM_PUSH, common_pb:encode_qp_join_room_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_sitdown_rsp)  ->
    encode_qp_packet(?CMD_QP_SITDOWN_RSP, common_pb:encode_qp_sitdown_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_sitdown_push)  ->
    encode_qp_packet(?CMD_QP_SITDOWN_PUSH, common_pb:encode_qp_sitdown_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_standup_rsp)  ->
    encode_qp_packet(?CMD_QP_STANDUP_RSP, common_pb:encode_qp_standup_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_standup_push)  ->
    encode_qp_packet(?CMD_QP_STANDUP_PUSH, common_pb:encode_qp_standup_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_exit_room_rsp)  ->
    encode_qp_packet(?CMD_QP_EXIT_ROOM_RSP, common_pb:encode_qp_exit_room_rsp(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_exit_room_push)  ->
    encode_qp_packet(?CMD_QP_EXIT_ROOM_PUSH, common_pb:encode_qp_exit_room_push(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_room_dissmiss)  ->
    encode_qp_packet(?CMD_QP_ROOM_DISSMISS, common_pb:encode_qp_room_dissmiss(Packet));

encode_qp_packet(Packet) when is_record(Packet, qp_room_kick)  ->
    encode_qp_packet(?CMD_QP_ROOM_KICK, common_pb:encode_qp_room_kick(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_user_online_state_change)  ->
    encode_qp_packet(?CMD_QP_USER_ONLINE_STATE_CHANGE, common_pb:encode_qp_user_online_state_change(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_kick)  ->
    encode_qp_packet(?CMD_QP_KICK, common_pb:encode_qp_kick(Packet));

encode_qp_packet(Packet) when is_record(Packet, qp_game_data)  ->
    encode_qp_packet(?CMD_QP_GAME_DATA, common_pb:encode_qp_game_data(Packet));
encode_qp_packet(Packet) when is_record(Packet, qp_ping_rsp)  ->
    encode_qp_packet(?CMD_QP_PING_RSP, common_pb:encode_qp_ping_rsp(Packet)).
encode_qp_packet(Cmd, Body) ->
    Packet = #qp_packet{cmd = Cmd, serialized = Body},
    Bin = common_pb:encode_qp_packet(Packet),
    Len = size(Bin),
    <<Len:?BIG_UINT32, Bin/binary>>.
