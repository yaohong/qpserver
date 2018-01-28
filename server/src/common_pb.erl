-file("src/common_pb.erl", 1).

-module(common_pb).

-author('erlangonrails@gmail.com').

-export([encode_qp_ping_rsp/1, decode_qp_ping_rsp/1,
	 encode_qp_ping_req/1, decode_qp_ping_req/1,
	 encode_qp_game_data/1, decode_qp_game_data/1,
	 encode_qp_kick/1, decode_qp_kick/1,
	 encode_qp_user_online_state_change/1,
	 decode_qp_user_online_state_change/1,
	 encode_qp_room_kick/1, decode_qp_room_kick/1,
	 encode_qp_room_dissmiss/1, decode_qp_room_dissmiss/1,
	 encode_qp_exit_room_push/1, decode_qp_exit_room_push/1,
	 encode_qp_exit_room_rsp/1, decode_qp_exit_room_rsp/1,
	 encode_qp_exit_room_req/1, decode_qp_exit_room_req/1,
	 encode_qp_standup_push/1, decode_qp_standup_push/1,
	 encode_qp_standup_rsp/1, decode_qp_standup_rsp/1,
	 encode_qp_standup_req/1, decode_qp_standup_req/1,
	 encode_qp_sitdown_push/1, decode_qp_sitdown_push/1,
	 encode_qp_sitdown_rsp/1, decode_qp_sitdown_rsp/1,
	 encode_qp_sitdown_req/1, decode_qp_sitdown_req/1,
	 encode_qp_join_room_push/1, decode_qp_join_room_push/1,
	 encode_qp_join_room_rsp/1, decode_qp_join_room_rsp/1,
	 encode_pb_room_user/1, decode_pb_room_user/1,
	 encode_pb_room_data/1, decode_pb_room_data/1,
	 encode_qp_join_room_req/1, decode_qp_join_room_req/1,
	 encode_qp_create_room_rsp/1,
	 decode_qp_create_room_rsp/1,
	 encode_qp_create_room_req/1,
	 decode_qp_create_room_req/1, encode_pb_room_cfg/1,
	 decode_pb_room_cfg/1, encode_qp_login_rsp/1,
	 decode_qp_login_rsp/1, encode_pb_user_private_data/1,
	 decode_pb_user_private_data/1,
	 encode_pb_user_public_data/1,
	 decode_pb_user_public_data/1, encode_qp_login_req/1,
	 decode_qp_login_req/1, encode_qp_packet/1,
	 decode_qp_packet/1]).

-record(qp_ping_rsp, {noop}).

-record(qp_ping_req, {noop}).

-record(qp_game_data, {game_data}).

-record(qp_kick, {noop}).

-record(qp_user_online_state_change,
	{room_id, user_id, online}).

-record(qp_room_kick, {room_id, user_id, type}).

-record(qp_room_dissmiss, {room_id, type}).

-record(qp_exit_room_push, {user_id, seat_num}).

-record(qp_exit_room_rsp, {result}).

-record(qp_exit_room_req, {seat_num}).

-record(qp_standup_push, {seat_num}).

-record(qp_standup_rsp, {state}).

-record(qp_standup_req, {seat_num}).

-record(qp_sitdown_push, {seat_num, user_id}).

-record(qp_sitdown_rsp, {result, seat_num}).

-record(qp_sitdown_req, {seat_num}).

-record(qp_join_room_push, {public_data}).

-record(qp_join_room_rsp, {result, room_data}).

-record(pb_room_user, {user_public_data, seat_number}).

-record(pb_room_data,
	{room_id, cfg, room_users, game_data}).

-record(qp_join_room_req, {room_id}).

-record(qp_create_room_rsp, {state, room_id}).

-record(qp_create_room_req, {cfg}).

-record(pb_room_cfg,
	{room_name, is_aa, double_down_score,
	 is_laizi_playmethod, is_ob, is_random, is_not_voice,
	 is_safe_mode, lock_userid_list}).

-record(qp_login_rsp,
	{state, public_data, private_data, room_data}).

-record(pb_user_private_data, {room_card_count}).

-record(pb_user_public_data,
	{user_id, avatar_url, nick_name}).

-record(qp_login_req, {account, pwd}).

-record(qp_packet, {cmd, serialized}).

encode_qp_ping_rsp(Record)
    when is_record(Record, qp_ping_rsp) ->
    encode(qp_ping_rsp, Record).

encode_qp_ping_req(Record)
    when is_record(Record, qp_ping_req) ->
    encode(qp_ping_req, Record).

encode_qp_game_data(Record)
    when is_record(Record, qp_game_data) ->
    encode(qp_game_data, Record).

encode_qp_kick(Record)
    when is_record(Record, qp_kick) ->
    encode(qp_kick, Record).

encode_qp_user_online_state_change(Record)
    when is_record(Record, qp_user_online_state_change) ->
    encode(qp_user_online_state_change, Record).

encode_qp_room_kick(Record)
    when is_record(Record, qp_room_kick) ->
    encode(qp_room_kick, Record).

encode_qp_room_dissmiss(Record)
    when is_record(Record, qp_room_dissmiss) ->
    encode(qp_room_dissmiss, Record).

encode_qp_exit_room_push(Record)
    when is_record(Record, qp_exit_room_push) ->
    encode(qp_exit_room_push, Record).

encode_qp_exit_room_rsp(Record)
    when is_record(Record, qp_exit_room_rsp) ->
    encode(qp_exit_room_rsp, Record).

encode_qp_exit_room_req(Record)
    when is_record(Record, qp_exit_room_req) ->
    encode(qp_exit_room_req, Record).

encode_qp_standup_push(Record)
    when is_record(Record, qp_standup_push) ->
    encode(qp_standup_push, Record).

encode_qp_standup_rsp(Record)
    when is_record(Record, qp_standup_rsp) ->
    encode(qp_standup_rsp, Record).

encode_qp_standup_req(Record)
    when is_record(Record, qp_standup_req) ->
    encode(qp_standup_req, Record).

encode_qp_sitdown_push(Record)
    when is_record(Record, qp_sitdown_push) ->
    encode(qp_sitdown_push, Record).

encode_qp_sitdown_rsp(Record)
    when is_record(Record, qp_sitdown_rsp) ->
    encode(qp_sitdown_rsp, Record).

encode_qp_sitdown_req(Record)
    when is_record(Record, qp_sitdown_req) ->
    encode(qp_sitdown_req, Record).

encode_qp_join_room_push(Record)
    when is_record(Record, qp_join_room_push) ->
    encode(qp_join_room_push, Record).

encode_qp_join_room_rsp(Record)
    when is_record(Record, qp_join_room_rsp) ->
    encode(qp_join_room_rsp, Record).

encode_pb_room_user(Record)
    when is_record(Record, pb_room_user) ->
    encode(pb_room_user, Record).

encode_pb_room_data(Record)
    when is_record(Record, pb_room_data) ->
    encode(pb_room_data, Record).

encode_qp_join_room_req(Record)
    when is_record(Record, qp_join_room_req) ->
    encode(qp_join_room_req, Record).

encode_qp_create_room_rsp(Record)
    when is_record(Record, qp_create_room_rsp) ->
    encode(qp_create_room_rsp, Record).

encode_qp_create_room_req(Record)
    when is_record(Record, qp_create_room_req) ->
    encode(qp_create_room_req, Record).

encode_pb_room_cfg(Record)
    when is_record(Record, pb_room_cfg) ->
    encode(pb_room_cfg, Record).

encode_qp_login_rsp(Record)
    when is_record(Record, qp_login_rsp) ->
    encode(qp_login_rsp, Record).

encode_pb_user_private_data(Record)
    when is_record(Record, pb_user_private_data) ->
    encode(pb_user_private_data, Record).

encode_pb_user_public_data(Record)
    when is_record(Record, pb_user_public_data) ->
    encode(pb_user_public_data, Record).

encode_qp_login_req(Record)
    when is_record(Record, qp_login_req) ->
    encode(qp_login_req, Record).

encode_qp_packet(Record)
    when is_record(Record, qp_packet) ->
    encode(qp_packet, Record).

encode(qp_packet, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_packet.cmd, none), int32, []),
		      pack(2, optional,
			   with_default(Record#qp_packet.serialized, none),
			   bytes, [])]);
encode(qp_login_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_login_req.account, none),
			   string, []),
		      pack(2, required,
			   with_default(Record#qp_login_req.pwd, none), string,
			   [])]);
encode(pb_user_public_data, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#pb_user_public_data.user_id,
					none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#pb_user_public_data.avatar_url,
					none),
			   string, []),
		      pack(3, required,
			   with_default(Record#pb_user_public_data.nick_name,
					none),
			   string, [])]);
encode(pb_user_private_data, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#pb_user_private_data.room_card_count,
					none),
			   int32, [])]);
encode(qp_login_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_login_rsp.state, none), int32,
			   []),
		      pack(2, optional,
			   with_default(Record#qp_login_rsp.public_data, none),
			   pb_user_public_data, []),
		      pack(3, optional,
			   with_default(Record#qp_login_rsp.private_data, none),
			   pb_user_private_data, []),
		      pack(4, optional,
			   with_default(Record#qp_login_rsp.room_data, none),
			   pb_room_data, [])]);
encode(pb_room_cfg, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#pb_room_cfg.room_name, none),
			   string, []),
		      pack(2, required,
			   with_default(Record#pb_room_cfg.is_aa, none), bool,
			   []),
		      pack(3, required,
			   with_default(Record#pb_room_cfg.double_down_score,
					none),
			   int32, []),
		      pack(4, required,
			   with_default(Record#pb_room_cfg.is_laizi_playmethod,
					none),
			   bool, []),
		      pack(5, required,
			   with_default(Record#pb_room_cfg.is_ob, none), bool,
			   []),
		      pack(6, required,
			   with_default(Record#pb_room_cfg.is_random, none),
			   bool, []),
		      pack(7, required,
			   with_default(Record#pb_room_cfg.is_not_voice, none),
			   bool, []),
		      pack(8, required,
			   with_default(Record#pb_room_cfg.is_safe_mode, none),
			   bool, []),
		      pack(9, repeated,
			   with_default(Record#pb_room_cfg.lock_userid_list,
					none),
			   int32, [])]);
encode(qp_create_room_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_create_room_req.cfg, none),
			   pb_room_cfg, [])]);
encode(qp_create_room_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_create_room_rsp.state, none),
			   int32, []),
		      pack(2, optional,
			   with_default(Record#qp_create_room_rsp.room_id,
					none),
			   int32, [])]);
encode(qp_join_room_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_join_room_req.room_id, none),
			   int32, [])]);
encode(pb_room_data, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#pb_room_data.room_id, none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#pb_room_data.cfg, none),
			   pb_room_cfg, []),
		      pack(3, repeated,
			   with_default(Record#pb_room_data.room_users, none),
			   pb_room_user, []),
		      pack(4, optional,
			   with_default(Record#pb_room_data.game_data, none),
			   bytes, [])]);
encode(pb_room_user, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#pb_room_user.user_public_data,
					none),
			   pb_user_public_data, []),
		      pack(2, required,
			   with_default(Record#pb_room_user.seat_number, none),
			   int32, [])]);
encode(qp_join_room_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_join_room_rsp.result, none),
			   int32, []),
		      pack(2, optional,
			   with_default(Record#qp_join_room_rsp.room_data,
					none),
			   pb_room_data, [])]);
encode(qp_join_room_push, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_join_room_push.public_data,
					none),
			   pb_user_public_data, [])]);
encode(qp_sitdown_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_sitdown_req.seat_num, none),
			   int32, [])]);
encode(qp_sitdown_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_sitdown_rsp.result, none),
			   int32, []),
		      pack(2, optional,
			   with_default(Record#qp_sitdown_rsp.seat_num, none),
			   int32, [])]);
encode(qp_sitdown_push, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_sitdown_push.seat_num, none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_sitdown_push.user_id, none),
			   int32, [])]);
encode(qp_standup_req, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#qp_standup_req.seat_num, none),
			   int32, [])]);
encode(qp_standup_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_standup_rsp.state, none),
			   int32, [])]);
encode(qp_standup_push, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_standup_push.seat_num, none),
			   int32, [])]);
encode(qp_exit_room_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_exit_room_req.seat_num, none),
			   int32, [])]);
encode(qp_exit_room_rsp, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_exit_room_rsp.result, none),
			   int32, [])]);
encode(qp_exit_room_push, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_exit_room_push.user_id, none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_exit_room_push.seat_num,
					none),
			   int32, [])]);
encode(qp_room_dissmiss, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_room_dissmiss.room_id, none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_room_dissmiss.type, none),
			   int32, [])]);
encode(qp_room_kick, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_room_kick.room_id, none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_room_kick.user_id, none),
			   int32, []),
		      pack(3, required,
			   with_default(Record#qp_room_kick.type, none), int32,
			   [])]);
encode(qp_user_online_state_change, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_user_online_state_change.room_id,
					none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_user_online_state_change.user_id,
					none),
			   int32, []),
		      pack(3, required,
			   with_default(Record#qp_user_online_state_change.online,
					none),
			   bool, [])]);
encode(qp_kick, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#qp_kick.noop, none), int32,
			   [])]);
encode(qp_game_data, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_game_data.game_data, none),
			   bytes, [])]);
encode(qp_ping_req, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#qp_ping_req.noop, none), int32,
			   [])]);
encode(qp_ping_rsp, Record) ->
    iolist_to_binary([pack(1, optional,
			   with_default(Record#qp_ping_rsp.noop, none), int32,
			   [])]).

with_default(undefined, none) -> undefined;
with_default(undefined, Default) -> Default;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _) ->
    protobuffs:encode(FNum, Data, Type).

decode_qp_ping_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_ping_rsp, Bytes).

decode_qp_ping_req(Bytes) when is_binary(Bytes) ->
    decode(qp_ping_req, Bytes).

decode_qp_game_data(Bytes) when is_binary(Bytes) ->
    decode(qp_game_data, Bytes).

decode_qp_kick(Bytes) when is_binary(Bytes) ->
    decode(qp_kick, Bytes).

decode_qp_user_online_state_change(Bytes)
    when is_binary(Bytes) ->
    decode(qp_user_online_state_change, Bytes).

decode_qp_room_kick(Bytes) when is_binary(Bytes) ->
    decode(qp_room_kick, Bytes).

decode_qp_room_dissmiss(Bytes) when is_binary(Bytes) ->
    decode(qp_room_dissmiss, Bytes).

decode_qp_exit_room_push(Bytes) when is_binary(Bytes) ->
    decode(qp_exit_room_push, Bytes).

decode_qp_exit_room_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_exit_room_rsp, Bytes).

decode_qp_exit_room_req(Bytes) when is_binary(Bytes) ->
    decode(qp_exit_room_req, Bytes).

decode_qp_standup_push(Bytes) when is_binary(Bytes) ->
    decode(qp_standup_push, Bytes).

decode_qp_standup_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_standup_rsp, Bytes).

decode_qp_standup_req(Bytes) when is_binary(Bytes) ->
    decode(qp_standup_req, Bytes).

decode_qp_sitdown_push(Bytes) when is_binary(Bytes) ->
    decode(qp_sitdown_push, Bytes).

decode_qp_sitdown_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_sitdown_rsp, Bytes).

decode_qp_sitdown_req(Bytes) when is_binary(Bytes) ->
    decode(qp_sitdown_req, Bytes).

decode_qp_join_room_push(Bytes) when is_binary(Bytes) ->
    decode(qp_join_room_push, Bytes).

decode_qp_join_room_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_join_room_rsp, Bytes).

decode_pb_room_user(Bytes) when is_binary(Bytes) ->
    decode(pb_room_user, Bytes).

decode_pb_room_data(Bytes) when is_binary(Bytes) ->
    decode(pb_room_data, Bytes).

decode_qp_join_room_req(Bytes) when is_binary(Bytes) ->
    decode(qp_join_room_req, Bytes).

decode_qp_create_room_rsp(Bytes)
    when is_binary(Bytes) ->
    decode(qp_create_room_rsp, Bytes).

decode_qp_create_room_req(Bytes)
    when is_binary(Bytes) ->
    decode(qp_create_room_req, Bytes).

decode_pb_room_cfg(Bytes) when is_binary(Bytes) ->
    decode(pb_room_cfg, Bytes).

decode_qp_login_rsp(Bytes) when is_binary(Bytes) ->
    decode(qp_login_rsp, Bytes).

decode_pb_user_private_data(Bytes)
    when is_binary(Bytes) ->
    decode(pb_user_private_data, Bytes).

decode_pb_user_public_data(Bytes)
    when is_binary(Bytes) ->
    decode(pb_user_public_data, Bytes).

decode_qp_login_req(Bytes) when is_binary(Bytes) ->
    decode(qp_login_req, Bytes).

decode_qp_packet(Bytes) when is_binary(Bytes) ->
    decode(qp_packet, Bytes).

decode(qp_packet, Bytes) when is_binary(Bytes) ->
    Types = [{2, serialized, bytes, []},
	     {1, cmd, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_packet, Decoded);
decode(qp_login_req, Bytes) when is_binary(Bytes) ->
    Types = [{2, pwd, string, []},
	     {1, account, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_login_req, Decoded);
decode(pb_user_public_data, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, nick_name, string, []},
	     {2, avatar_url, string, []}, {1, user_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(pb_user_public_data, Decoded);
decode(pb_user_private_data, Bytes)
    when is_binary(Bytes) ->
    Types = [{1, room_card_count, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(pb_user_private_data, Decoded);
decode(qp_login_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{4, room_data, pb_room_data, [is_record]},
	     {3, private_data, pb_user_private_data, [is_record]},
	     {2, public_data, pb_user_public_data, [is_record]},
	     {1, state, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_login_rsp, Decoded);
decode(pb_room_cfg, Bytes) when is_binary(Bytes) ->
    Types = [{9, lock_userid_list, int32, [repeated]},
	     {8, is_safe_mode, bool, []},
	     {7, is_not_voice, bool, []}, {6, is_random, bool, []},
	     {5, is_ob, bool, []},
	     {4, is_laizi_playmethod, bool, []},
	     {3, double_down_score, int32, []}, {2, is_aa, bool, []},
	     {1, room_name, string, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(pb_room_cfg, Decoded);
decode(qp_create_room_req, Bytes)
    when is_binary(Bytes) ->
    Types = [{1, cfg, pb_room_cfg, [is_record]}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_create_room_req, Decoded);
decode(qp_create_room_rsp, Bytes)
    when is_binary(Bytes) ->
    Types = [{2, room_id, int32, []},
	     {1, state, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_create_room_rsp, Decoded);
decode(qp_join_room_req, Bytes) when is_binary(Bytes) ->
    Types = [{1, room_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_join_room_req, Decoded);
decode(pb_room_data, Bytes) when is_binary(Bytes) ->
    Types = [{4, game_data, bytes, []},
	     {3, room_users, pb_room_user, [is_record, repeated]},
	     {2, cfg, pb_room_cfg, [is_record]},
	     {1, room_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(pb_room_data, Decoded);
decode(pb_room_user, Bytes) when is_binary(Bytes) ->
    Types = [{2, seat_number, int32, []},
	     {1, user_public_data, pb_user_public_data,
	      [is_record]}],
    Decoded = decode(Bytes, Types, []),
    to_record(pb_room_user, Decoded);
decode(qp_join_room_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{2, room_data, pb_room_data, [is_record]},
	     {1, result, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_join_room_rsp, Decoded);
decode(qp_join_room_push, Bytes)
    when is_binary(Bytes) ->
    Types = [{1, public_data, pb_user_public_data,
	      [is_record]}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_join_room_push, Decoded);
decode(qp_sitdown_req, Bytes) when is_binary(Bytes) ->
    Types = [{1, seat_num, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_sitdown_req, Decoded);
decode(qp_sitdown_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{2, seat_num, int32, []},
	     {1, result, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_sitdown_rsp, Decoded);
decode(qp_sitdown_push, Bytes) when is_binary(Bytes) ->
    Types = [{2, user_id, int32, []},
	     {1, seat_num, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_sitdown_push, Decoded);
decode(qp_standup_req, Bytes) when is_binary(Bytes) ->
    Types = [{1, seat_num, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_standup_req, Decoded);
decode(qp_standup_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{1, state, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_standup_rsp, Decoded);
decode(qp_standup_push, Bytes) when is_binary(Bytes) ->
    Types = [{1, seat_num, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_standup_push, Decoded);
decode(qp_exit_room_req, Bytes) when is_binary(Bytes) ->
    Types = [{1, seat_num, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_exit_room_req, Decoded);
decode(qp_exit_room_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{1, result, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_exit_room_rsp, Decoded);
decode(qp_exit_room_push, Bytes)
    when is_binary(Bytes) ->
    Types = [{2, seat_num, int32, []},
	     {1, user_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_exit_room_push, Decoded);
decode(qp_room_dissmiss, Bytes) when is_binary(Bytes) ->
    Types = [{2, type, int32, []}, {1, room_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_room_dissmiss, Decoded);
decode(qp_room_kick, Bytes) when is_binary(Bytes) ->
    Types = [{3, type, int32, []}, {2, user_id, int32, []},
	     {1, room_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_room_kick, Decoded);
decode(qp_user_online_state_change, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, online, bool, []}, {2, user_id, int32, []},
	     {1, room_id, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_user_online_state_change, Decoded);
decode(qp_kick, Bytes) when is_binary(Bytes) ->
    Types = [{1, noop, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_kick, Decoded);
decode(qp_game_data, Bytes) when is_binary(Bytes) ->
    Types = [{1, game_data, bytes, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_game_data, Decoded);
decode(qp_ping_req, Bytes) when is_binary(Bytes) ->
    Types = [{1, noop, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_ping_req, Decoded);
decode(qp_ping_rsp, Bytes) when is_binary(Bytes) ->
    Types = [{1, noop, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_ping_rsp, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(Bytes, Types, Acc) ->
    {{FNum, WireType}, Rest} =
	protobuffs:read_field_num_and_wire_type(Bytes),
    case lists:keysearch(FNum, 1, Types) of
      {value, {FNum, Name, Type, Opts}} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {V, R} = protobuffs:decode_value(Rest,
								   WireType,
								   bytes),
				  RecVal =
				      decode(list_to_atom(string:to_lower(atom_to_list(Type))),
					     V),
				  {RecVal, R};
			      false ->
				  {V, R} = protobuffs:decode_value(Rest,
								   WireType,
								   Type),
				  {unpack_value(V, Type), R}
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([Value1 | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types, [{FNum, Name, [Value1]} | Acc])
		end;
	    false ->
		decode(Rest1, Types, [{FNum, Name, Value1} | Acc])
	  end;
      false -> exit({error, {unexpected_field_index, FNum}})
    end.

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(qp_packet, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_packet), Record,
					 Name, Val)
		end,
		#qp_packet{}, DecodedTuples);
to_record(qp_login_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_login_req),
					 Record, Name, Val)
		end,
		#qp_login_req{}, DecodedTuples);
to_record(pb_user_public_data, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     pb_user_public_data),
					 Record, Name, Val)
		end,
		#pb_user_public_data{}, DecodedTuples);
to_record(pb_user_private_data, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     pb_user_private_data),
					 Record, Name, Val)
		end,
		#pb_user_private_data{}, DecodedTuples);
to_record(qp_login_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_login_rsp),
					 Record, Name, Val)
		end,
		#qp_login_rsp{}, DecodedTuples);
to_record(pb_room_cfg, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, pb_room_cfg),
					 Record, Name, Val)
		end,
		#pb_room_cfg{}, DecodedTuples);
to_record(qp_create_room_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     qp_create_room_req),
					 Record, Name, Val)
		end,
		#qp_create_room_req{}, DecodedTuples);
to_record(qp_create_room_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     qp_create_room_rsp),
					 Record, Name, Val)
		end,
		#qp_create_room_rsp{}, DecodedTuples);
to_record(qp_join_room_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_join_room_req),
					 Record, Name, Val)
		end,
		#qp_join_room_req{}, DecodedTuples);
to_record(pb_room_data, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, pb_room_data),
					 Record, Name, Val)
		end,
		#pb_room_data{}, DecodedTuples);
to_record(pb_room_user, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, pb_room_user),
					 Record, Name, Val)
		end,
		#pb_room_user{}, DecodedTuples);
to_record(qp_join_room_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_join_room_rsp),
					 Record, Name, Val)
		end,
		#qp_join_room_rsp{}, DecodedTuples);
to_record(qp_join_room_push, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_join_room_push),
					 Record, Name, Val)
		end,
		#qp_join_room_push{}, DecodedTuples);
to_record(qp_sitdown_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_sitdown_req),
					 Record, Name, Val)
		end,
		#qp_sitdown_req{}, DecodedTuples);
to_record(qp_sitdown_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_sitdown_rsp),
					 Record, Name, Val)
		end,
		#qp_sitdown_rsp{}, DecodedTuples);
to_record(qp_sitdown_push, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_sitdown_push),
					 Record, Name, Val)
		end,
		#qp_sitdown_push{}, DecodedTuples);
to_record(qp_standup_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_standup_req),
					 Record, Name, Val)
		end,
		#qp_standup_req{}, DecodedTuples);
to_record(qp_standup_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_standup_rsp),
					 Record, Name, Val)
		end,
		#qp_standup_rsp{}, DecodedTuples);
to_record(qp_standup_push, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_standup_push),
					 Record, Name, Val)
		end,
		#qp_standup_push{}, DecodedTuples);
to_record(qp_exit_room_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_exit_room_req),
					 Record, Name, Val)
		end,
		#qp_exit_room_req{}, DecodedTuples);
to_record(qp_exit_room_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_exit_room_rsp),
					 Record, Name, Val)
		end,
		#qp_exit_room_rsp{}, DecodedTuples);
to_record(qp_exit_room_push, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_exit_room_push),
					 Record, Name, Val)
		end,
		#qp_exit_room_push{}, DecodedTuples);
to_record(qp_room_dissmiss, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_room_dissmiss),
					 Record, Name, Val)
		end,
		#qp_room_dissmiss{}, DecodedTuples);
to_record(qp_room_kick, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_room_kick),
					 Record, Name, Val)
		end,
		#qp_room_kick{}, DecodedTuples);
to_record(qp_user_online_state_change, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     qp_user_online_state_change),
					 Record, Name, Val)
		end,
		#qp_user_online_state_change{}, DecodedTuples);
to_record(qp_kick, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_kick), Record,
					 Name, Val)
		end,
		#qp_kick{}, DecodedTuples);
to_record(qp_game_data, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_game_data),
					 Record, Name, Val)
		end,
		#qp_game_data{}, DecodedTuples);
to_record(qp_ping_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_ping_req),
					 Record, Name, Val)
		end,
		#qp_ping_req{}, DecodedTuples);
to_record(qp_ping_rsp, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_ping_rsp),
					 Record, Name, Val)
		end,
		#qp_ping_rsp{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

