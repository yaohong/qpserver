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




encode_qp_packet(Packet)  ->
    encode_qp_packet(1, common_pb:encode_qp_login_rsp(Packet)).

encode_qp_packet(Cmd, Body) ->
    Packet = #qp_packet{cmd = Cmd, serialized = Body, seq_id = 0},
    Bin = common_pb:encode_qp_packet(Packet),
    Len = size(Bin),
    <<Len:?BIG_UINT32, Bin/binary>>.
