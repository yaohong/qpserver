-file("src/mj_pb.erl", 1).

-module(mj_pb).

-author('erlangonrails@gmail.com').

-export([encode_qp_mj_oper_error/1,
	 decode_qp_mj_oper_error/1, encode_qp_mj_oper_notify/1,
	 decode_qp_mj_oper_notify/1, encode_qp_mj_oper_req/1,
	 decode_qp_mj_oper_req/1, encode_qp_mj_game_end_notify/1,
	 decode_qp_mj_game_end_notify/1,
	 encode_seat_score_item/1, decode_seat_score_item/1,
	 encode_qp_mj_game_start_notify/1,
	 decode_qp_mj_game_start_notify/1, encode_qp_logic/1,
	 decode_qp_logic/1]).

-record(qp_mj_oper_error,
	{error_type, error_value1, error_value2, oper_flag,
	 oper_value1, oper_value2}).

-record(qp_mj_oper_notify,
	{seat_numer, type, v1, v2, v3, next_oper_seat_num,
	 next_oper_flag, next_oper_value1, next_oper_value2}).

-record(qp_mj_oper_req, {type, v1, v2}).

-record(qp_mj_game_end_notify,
	{end_type, hupai_seatnumber, hupai_value, hupai_type,
	 fangpao_seatnumber, seat0_pai, seat1_pai, seat2_pai,
	 seat3_pai, scores}).

-record(seat_score_item, {seat_number, score}).

-record(qp_mj_game_start_notify,
	{pai, banker_seat_number, oper_flag}).

-record(qp_logic, {cmd, serialized}).

encode_qp_mj_oper_error(Record)
    when is_record(Record, qp_mj_oper_error) ->
    encode(qp_mj_oper_error, Record).

encode_qp_mj_oper_notify(Record)
    when is_record(Record, qp_mj_oper_notify) ->
    encode(qp_mj_oper_notify, Record).

encode_qp_mj_oper_req(Record)
    when is_record(Record, qp_mj_oper_req) ->
    encode(qp_mj_oper_req, Record).

encode_qp_mj_game_end_notify(Record)
    when is_record(Record, qp_mj_game_end_notify) ->
    encode(qp_mj_game_end_notify, Record).

encode_seat_score_item(Record)
    when is_record(Record, seat_score_item) ->
    encode(seat_score_item, Record).

encode_qp_mj_game_start_notify(Record)
    when is_record(Record, qp_mj_game_start_notify) ->
    encode(qp_mj_game_start_notify, Record).

encode_qp_logic(Record)
    when is_record(Record, qp_logic) ->
    encode(qp_logic, Record).

encode(qp_logic, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_logic.cmd, none), int32, []),
		      pack(2, optional,
			   with_default(Record#qp_logic.serialized, none),
			   bytes, [])]);
encode(qp_mj_game_start_notify, Record) ->
    iolist_to_binary([pack(1, repeated,
			   with_default(Record#qp_mj_game_start_notify.pai,
					none),
			   uint32, []),
		      pack(2, required,
			   with_default(Record#qp_mj_game_start_notify.banker_seat_number,
					none),
			   int32, []),
		      pack(3, optional,
			   with_default(Record#qp_mj_game_start_notify.oper_flag,
					none),
			   uint32, [])]);
encode(seat_score_item, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#seat_score_item.seat_number,
					none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#seat_score_item.score, none),
			   int32, [])]);
encode(qp_mj_game_end_notify, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_mj_game_end_notify.end_type,
					none),
			   uint32, []),
		      pack(2, required,
			   with_default(Record#qp_mj_game_end_notify.hupai_seatnumber,
					none),
			   int32, []),
		      pack(3, required,
			   with_default(Record#qp_mj_game_end_notify.hupai_value,
					none),
			   uint32, []),
		      pack(4, required,
			   with_default(Record#qp_mj_game_end_notify.hupai_type,
					none),
			   uint32, []),
		      pack(5, required,
			   with_default(Record#qp_mj_game_end_notify.fangpao_seatnumber,
					none),
			   int32, []),
		      pack(6, repeated,
			   with_default(Record#qp_mj_game_end_notify.seat0_pai,
					none),
			   uint32, []),
		      pack(7, repeated,
			   with_default(Record#qp_mj_game_end_notify.seat1_pai,
					none),
			   uint32, []),
		      pack(8, repeated,
			   with_default(Record#qp_mj_game_end_notify.seat2_pai,
					none),
			   uint32, []),
		      pack(9, repeated,
			   with_default(Record#qp_mj_game_end_notify.seat3_pai,
					none),
			   uint32, []),
		      pack(10, repeated,
			   with_default(Record#qp_mj_game_end_notify.scores,
					none),
			   seat_score_item, [])]);
encode(qp_mj_oper_req, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_mj_oper_req.type, none),
			   uint32, []),
		      pack(2, optional,
			   with_default(Record#qp_mj_oper_req.v1, none), uint32,
			   []),
		      pack(3, optional,
			   with_default(Record#qp_mj_oper_req.v2, none), uint32,
			   [])]);
encode(qp_mj_oper_notify, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_mj_oper_notify.seat_numer,
					none),
			   int32, []),
		      pack(2, required,
			   with_default(Record#qp_mj_oper_notify.type, none),
			   uint32, []),
		      pack(3, optional,
			   with_default(Record#qp_mj_oper_notify.v1, none),
			   uint32, []),
		      pack(4, optional,
			   with_default(Record#qp_mj_oper_notify.v2, none),
			   uint32, []),
		      pack(5, optional,
			   with_default(Record#qp_mj_oper_notify.v3, none),
			   int32, []),
		      pack(6, optional,
			   with_default(Record#qp_mj_oper_notify.next_oper_seat_num,
					none),
			   int32, []),
		      pack(7, optional,
			   with_default(Record#qp_mj_oper_notify.next_oper_flag,
					none),
			   uint32, []),
		      pack(8, optional,
			   with_default(Record#qp_mj_oper_notify.next_oper_value1,
					none),
			   uint32, []),
		      pack(9, optional,
			   with_default(Record#qp_mj_oper_notify.next_oper_value2,
					none),
			   uint32, [])]);
encode(qp_mj_oper_error, Record) ->
    iolist_to_binary([pack(1, required,
			   with_default(Record#qp_mj_oper_error.error_type,
					none),
			   uint32, []),
		      pack(2, optional,
			   with_default(Record#qp_mj_oper_error.error_value1,
					none),
			   uint32, []),
		      pack(3, optional,
			   with_default(Record#qp_mj_oper_error.error_value2,
					none),
			   uint32, []),
		      pack(4, optional,
			   with_default(Record#qp_mj_oper_error.oper_flag,
					none),
			   uint32, []),
		      pack(5, optional,
			   with_default(Record#qp_mj_oper_error.oper_value1,
					none),
			   uint32, []),
		      pack(6, optional,
			   with_default(Record#qp_mj_oper_error.oper_value2,
					none),
			   uint32, [])]).

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

decode_qp_mj_oper_error(Bytes) when is_binary(Bytes) ->
    decode(qp_mj_oper_error, Bytes).

decode_qp_mj_oper_notify(Bytes) when is_binary(Bytes) ->
    decode(qp_mj_oper_notify, Bytes).

decode_qp_mj_oper_req(Bytes) when is_binary(Bytes) ->
    decode(qp_mj_oper_req, Bytes).

decode_qp_mj_game_end_notify(Bytes)
    when is_binary(Bytes) ->
    decode(qp_mj_game_end_notify, Bytes).

decode_seat_score_item(Bytes) when is_binary(Bytes) ->
    decode(seat_score_item, Bytes).

decode_qp_mj_game_start_notify(Bytes)
    when is_binary(Bytes) ->
    decode(qp_mj_game_start_notify, Bytes).

decode_qp_logic(Bytes) when is_binary(Bytes) ->
    decode(qp_logic, Bytes).

decode(qp_logic, Bytes) when is_binary(Bytes) ->
    Types = [{2, serialized, bytes, []},
	     {1, cmd, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_logic, Decoded);
decode(qp_mj_game_start_notify, Bytes)
    when is_binary(Bytes) ->
    Types = [{3, oper_flag, uint32, []},
	     {2, banker_seat_number, int32, []},
	     {1, pai, uint32, [repeated]}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_mj_game_start_notify, Decoded);
decode(seat_score_item, Bytes) when is_binary(Bytes) ->
    Types = [{2, score, int32, []},
	     {1, seat_number, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(seat_score_item, Decoded);
decode(qp_mj_game_end_notify, Bytes)
    when is_binary(Bytes) ->
    Types = [{10, scores, seat_score_item,
	      [is_record, repeated]},
	     {9, seat3_pai, uint32, [repeated]},
	     {8, seat2_pai, uint32, [repeated]},
	     {7, seat1_pai, uint32, [repeated]},
	     {6, seat0_pai, uint32, [repeated]},
	     {5, fangpao_seatnumber, int32, []},
	     {4, hupai_type, uint32, []},
	     {3, hupai_value, uint32, []},
	     {2, hupai_seatnumber, int32, []},
	     {1, end_type, uint32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_mj_game_end_notify, Decoded);
decode(qp_mj_oper_req, Bytes) when is_binary(Bytes) ->
    Types = [{3, v2, uint32, []}, {2, v1, uint32, []},
	     {1, type, uint32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_mj_oper_req, Decoded);
decode(qp_mj_oper_notify, Bytes)
    when is_binary(Bytes) ->
    Types = [{9, next_oper_value2, uint32, []},
	     {8, next_oper_value1, uint32, []},
	     {7, next_oper_flag, uint32, []},
	     {6, next_oper_seat_num, int32, []}, {5, v3, int32, []},
	     {4, v2, uint32, []}, {3, v1, uint32, []},
	     {2, type, uint32, []}, {1, seat_numer, int32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_mj_oper_notify, Decoded);
decode(qp_mj_oper_error, Bytes) when is_binary(Bytes) ->
    Types = [{6, oper_value2, uint32, []},
	     {5, oper_value1, uint32, []},
	     {4, oper_flag, uint32, []},
	     {3, error_value2, uint32, []},
	     {2, error_value1, uint32, []},
	     {1, error_type, uint32, []}],
    Decoded = decode(Bytes, Types, []),
    to_record(qp_mj_oper_error, Decoded).

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

to_record(qp_logic, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_logic), Record,
					 Name, Val)
		end,
		#qp_logic{}, DecodedTuples);
to_record(qp_mj_game_start_notify, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     qp_mj_game_start_notify),
					 Record, Name, Val)
		end,
		#qp_mj_game_start_notify{}, DecodedTuples);
to_record(seat_score_item, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, seat_score_item),
					 Record, Name, Val)
		end,
		#seat_score_item{}, DecodedTuples);
to_record(qp_mj_game_end_notify, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields,
						     qp_mj_game_end_notify),
					 Record, Name, Val)
		end,
		#qp_mj_game_end_notify{}, DecodedTuples);
to_record(qp_mj_oper_req, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_mj_oper_req),
					 Record, Name, Val)
		end,
		#qp_mj_oper_req{}, DecodedTuples);
to_record(qp_mj_oper_notify, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_mj_oper_notify),
					 Record, Name, Val)
		end,
		#qp_mj_oper_notify{}, DecodedTuples);
to_record(qp_mj_oper_error, DecodedTuples) ->
    lists:foldl(fun ({_FNum, Name, Val}, Record) ->
			set_record_field(record_info(fields, qp_mj_oper_error),
					 Record, Name, Val)
		end,
		#qp_mj_oper_error{}, DecodedTuples).

set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> 0.

