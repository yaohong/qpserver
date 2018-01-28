%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. 一月 2018 1:44
%%%-------------------------------------------------------------------
-module(ss510k_room).
-author("yaohong").
-include("../qp_error.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../../include/common_pb.hrl").
-include("../qp_define.hrl").
-behaviour(gen_fsm).

%% API
-export([start/3, start_link/3]).

%% gen_fsm callbacks
-export([init/1,
	%%wait/2,
	wait/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).

-export([
	cb_check_game_whethertostart/2
]).


-export([
	join_room/2,
	sitdown/3, standup/3, exitroom/3,
	get_room_pbdata/1
]).


-define(SERVER, ?MODULE).
-define(CHECK_GAME_WHETHER_TO_START_TIMEOUT, 10 * 60).
-record(state, {
	room_id :: integer(),
	owner_basic_data :: any(),
	room_cfg :: any(),

	ob_set :: gb_sets:set(),                %%存放观众的集合


	user_basicdata_tree :: gb_trees:tree(),   %%存放玩家基础数据的树

	seat_tree :: gb_trees:tree(),            %%描述座位信息




	exit_make=0 :: integer()                  %%退出标志(打完结束，中途解散) 0: 中途解散 1:打完结束
}).

%%%===================================================================
%%% API
%%%===================================================================
%%进入房间携带账号基础信息(id,昵称,头像)
join_room(RoomPid, UserBasicData) ->
	gen_fsm:sync_send_event(RoomPid, {join_room, UserBasicData}, infinity).

sitdown(RoomPid, UserId, SeatNum) ->
	gen_fsm:sync_send_event(RoomPid, {sitdown, {UserId, SeatNum}}, infinity).

standup(RoomPid, UserId, SeatNum) ->
	gen_fsm:sync_send_event(RoomPid, {standup, {UserId, SeatNum}}, infinity).

exitroom(RoomPid, UserId, SeatNum) ->
	gen_fsm:sync_send_event(RoomPid, {exitroom, {UserId, SeatNum}}, infinity).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_room_pbdata(PoomPid) ->
	gen_fsm:sync_send_event(PoomPid, get_room_pbdata, infinity).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%检查游戏是否开始的定时器
cb_check_game_whethertostart(_Ref, Pid) ->
	Pid ! check_game_whethertostart.
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------


start(RoomId, OwnerBasicData, RoomCfg) ->
	supervisor:start_child(room_sup, [RoomId, OwnerBasicData, RoomCfg]).


-spec(start_link(RoomId :: integer(), OwnerBasicData :: term(), RoomCfg :: any()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(RoomId, OwnerBasicData, RoomCfg) ->
	gen_fsm:start_link(?MODULE, [RoomId, OwnerBasicData, RoomCfg], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, StateName :: atom(), StateData :: #state{}} |
	{ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([RoomId, OwnerBasicData, RoomCfg]) ->
	%%启动定时器，多久没开始游戏，自动退出
	random:seed(erlang:now()),
	CurrentTime = qp_util:timestamp(),
	timer_manager:addDelayTask(RoomId, CurrentTime + ?CHECK_GAME_WHETHER_TO_START_TIMEOUT, ?MODULE, cb_check_game_whethertostart, [self()]),
	?FILE_LOG_DEBUG("room_init room_id=~p, owner_basic_data=~p, room_cfg=~p", [RoomId, OwnerBasicData, RoomCfg]),
	T1 = gb_trees:insert(0, undefined, gb_trees:empty()),
	T2 = gb_trees:insert(1, undefined, T1),
	T3 = gb_trees:insert(2, undefined, T2),
	T4 = gb_trees:insert(3, undefined, T3),
	{ok,
		wait,
		#state{
			room_id = RoomId,
			owner_basic_data = OwnerBasicData,
			room_cfg = RoomCfg,
			ob_set = gb_sets:empty(),
			user_basicdata_tree = gb_trees:empty(),
			seat_tree = T4,
			exit_make = 0
		}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
%%-spec(wait(Event :: term(), State :: #state{}) ->
%%	{next_state, NextStateName :: atom(), NextState :: #state{}} |
%%	{next_state, NextStateName :: atom(), NextState :: #state{},
%%		timeout() | hibernate} |
%%	{stop, Reason :: term(), NewState :: #state{}}).
%%wait(_Event, State) ->
%%	{next_state, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(wait(Event :: term(), From :: {pid(), term()},
	State :: #state{}) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
	{reply, Reply, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewState :: #state{}} |
	{stop, Reason :: normal | term(), Reply :: term(),
		NewState :: #state{}}).
wait({join_room, UserBasicData}, _From, #state{room_cfg = RoomCfg} = State) ->
	#state{
		room_id = RoomId,
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree
	} = State,
	?FILE_LOG_DEBUG("join_room ~p, room_id=~p", [UserBasicData, RoomId]),
	UserId = UserBasicData:get(id),
	case gb_trees:is_defined(UserId, UserBasicDataTree) of
		true ->
			%%之前已经在里面了
			{reply, {failed, ?LOGIC_ERROR_REPEAT_JOIN}, wait, State};
		false ->
			NewUserBasicDataTree = gb_trees:insert(UserId, UserBasicData, UserBasicDataTree),
			NewObSet = gb_sets:insert(UserId, ObSet),

			%%给房间里的其他人发送玩家进入的消息
			PbJoinRoomPush = #qp_join_room_push{public_data = make_publicdata(UserBasicData)},
			PushPbBin = qp_proto:encode_qp_packet(PbJoinRoomPush),
			room_broadcast(UserBasicDataTree, {bin, PushPbBin}),
			%%给自己发送房间里其他玩家的信息

			PbRoomData = make_pbroomdata(RoomId, RoomCfg, NewObSet, SeatTree, NewUserBasicDataTree, undefined),
			PbJoinRoomRsp =
				#qp_join_room_rsp{
					result = 0,
					room_data = PbRoomData},
			PbJoinRoomRspBin = qp_proto:encode_qp_packet(PbJoinRoomRsp),
			UserBasicData:send_room_msg({bin, PbJoinRoomRspBin}),
			{reply, success, wait, State#state{user_basicdata_tree = NewUserBasicDataTree, ob_set = NewObSet}}
	end;
wait({sitdown, {UserId, SeatNum}}, _From, State) ->
	#state{
		room_id = RoomId,
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree,
		room_cfg = RoomCfg
	} = State,
	?FILE_LOG_DEBUG("sitdown user_id=~p seat_num=~p, room_id=~p", [UserId, SeatNum, RoomId]),
	case gb_trees:lookup(UserId, UserBasicDataTree) of
		none ->
			{reply, {failed, ?LOGIC_ERROR_NOT_IN_ROOM_NOT_SITDOWN}, wait, State};
		{value, _} ->
			case gb_sets:is_element(UserId, ObSet) of
				false ->
					{reply, {failed, ?LOGIC_ERROR_NOT_OB_NOT_SITDOWN}, wait, State};
				true ->
					case select_seatnum(SeatTree, UserId, SeatNum, RoomCfg:get(is_random), RoomCfg:get(lock_id_list)) of
						{_, ErrorCode} when ErrorCode > 0 ->
							{reply, {failed, ErrorCode}, wait, State};
						{NewSeatTree, SelectSeatNum, 0} ->
							%%广播所有人坐下成功
							SitdownPush = #qp_sitdown_push{seat_num = SelectSeatNum, user_id = UserId},
							PushPbBin = qp_proto:encode_qp_packet(SitdownPush),
							room_broadcast(UserBasicDataTree, {bin, PushPbBin}, UserId),
							%%告诉本人成功,并返回
							%%检查是否四个座位都坐满了(游戏开始)
							%%
							case check_full(NewSeatTree) of
								true ->
									%%满了,移除OB的玩家
									{NewObSet, NewUserBasicDataTree} = remove_obuser(RoomId, gb_sets:delete(UserId, ObSet), NewSeatTree, UserBasicDataTree, RoomCfg:get(is_ob)),
									%%初始化游戏数据，通知所有人游戏开始了

									{reply, {success, SelectSeatNum}, game, State#state{seat_tree = NewSeatTree, ob_set = NewObSet, user_basicdata_tree = NewUserBasicDataTree}};
								false ->
									{reply, {success, SelectSeatNum}, wait, State#state{seat_tree = NewSeatTree, ob_set = gb_sets:delete(UserId, ObSet)}}
							end
					end
			end
	end;
wait({standup, {UserId, SeatNum}}, _From, State) ->
	#state{
		room_id = RoomId,
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree
	} = State,
	?FILE_LOG_DEBUG("standup user_id=~p, seat_num=~p, room_id=~p", [UserId, SeatNum, RoomId]),
	case gb_trees:lookup(UserId, UserBasicDataTree) of
		none ->
			{reply, {failed, ?LOGIC_ERROR_NOT_IN_ROOM_NOT_SITDOWN}, wait, State};
		{value, _} ->
			case gb_trees:lookup(SeatNum, SeatTree) of
				{value, UserId} ->
					%%可以起立
					NewSeatTree = gb_trees:update(SeatNum, undefined, SeatTree),
					NewObSet = gb_sets:insert(UserId, ObSet),

					PbPush = #qp_standup_push{seat_num = SeatNum},
					%%给其他人广播我起立的消息(过滤掉自己)
					room_broadcast(UserBasicDataTree, {bin, qp_proto:encode_qp_packet(PbPush)}, UserId),

					{reply, success, wait, State#state{ob_set = NewObSet, seat_tree = NewSeatTree}};
				{value, OtherUserId} ->
					?FILE_LOG_WARNING("standup_failed, other_user_id ~p", [OtherUserId]),
					{reply, {failed, ?LOGIC_ERROR_SEAT_NOT_SELF}, wait, State};
				none ->
					%%座位号异常
					?FILE_LOG_WARNING("standup_failed, exception_seat_num ~p", [SeatNum]),
					{reply, {failed, ?LOGIC_ERROR_SEATNUM_EXCEPTION}, wait, State}
			end
	end;
wait({exitroom, {UserId, SeatNum}}, _From, State) ->
	#state{
		room_id = RoomId,
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree
	} = State,
	?FILE_LOG_DEBUG("exitroom user_id=~p, seat_num=~p, room_id=~p", [UserId, SeatNum, RoomId]),
	case gb_trees:lookup(UserId, UserBasicDataTree) of
		none ->
			{reply, {failed, ?LOGIC_ERROR_NOT_IN_ROOM_NOT_SITDOWN}, wait, State};
		{value, _} ->
			if
				SeatNum =:= -1 ->
					%%从OB位退出
					case gb_sets:is_element(UserId, ObSet) of
						false ->
							%%当前并不在ob位
							{reply, {failed, ?LOGIC_ERROR_NOT_OB}, wait, State};
						true ->
							NewObSet = gb_sets:delete(UserId, ObSet),
							NewUserBasicDataTree = gb_trees:delete(UserId, UserBasicDataTree),

							%%给其他人广播玩家退出房间的消息
							PbPush = #qp_exit_room_push{user_id = UserId, seat_num = -1},
							room_broadcast(NewUserBasicDataTree, {bin, qp_proto:encode_qp_packet(PbPush)}),

							{reply, success, wait, State#state{ob_set = NewObSet, user_basicdata_tree = NewUserBasicDataTree}}
					end;
				true ->
					%%从座位上退出
					case gb_trees:lookup(SeatNum, SeatTree) of
						{value, UserId} ->
							NewSeatTree = gb_trees:update(SeatNum, undefined, SeatTree),
							NewUserBasicDataTree = gb_trees:delete(UserId, UserBasicDataTree),
							PbPush = #qp_exit_room_push{user_id = UserId, seat_num = SeatNum},
							room_broadcast(NewUserBasicDataTree, {bin, qp_proto:encode_qp_packet(PbPush)}),
							{reply, success, wait, State#state{seat_tree = NewSeatTree, user_basicdata_tree = NewUserBasicDataTree}};
						{value, OtherUserId} ->
							?FILE_LOG_WARNING("exitroom_failed, other_user_id ~p", [OtherUserId]),
							{reply, {failed, ?LOGIC_ERROR_SEAT_NOT_SELF}, wait, State};
						none ->
							%%座位号异常
							?FILE_LOG_WARNING("exitroom_failed, exception_seat_num ~p", [SeatNum]),
							{reply, {failed, ?LOGIC_ERROR_SEATNUM_EXCEPTION}, wait, State}
					end
			end
	end;
wait(get_room_pbdata, _From, #state{room_id = RoomId, room_cfg = RoomCfg, ob_set = ObSet, seat_tree = SeatTree, user_basicdata_tree = UserBasicDataTree} = State) ->
	PbRoomData = make_pbroomdata(RoomId, RoomCfg, ObSet, SeatTree, UserBasicDataTree, undefined),
	{reply, {success, PbRoomData}, wait, State};
wait(Event, _From, State) ->
	?FILE_LOG_DEBUG("wait, ~p", Event),
	{reply, ignore, wait, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
	StateData :: #state{}) ->
	{next_state, NextStateName :: atom(), NewStateData :: #state{}} |
	{next_state, NextStateName :: atom(), NewStateData :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
	StateName :: atom(), StateData :: term()) ->
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
	{reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
	{stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
	Reply = ok,
	{reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
	StateData :: term()) ->
	{next_state, NextStateName :: atom(), NewStateData :: term()} |
	{next_state, NextStateName :: atom(), NewStateData :: term(),
		timeout() | hibernate} |
	{stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(check_game_whethertostart, wait, #state{room_id = RoomId} = State) ->
	%%十分钟都没开始
	?FILE_LOG_DEBUG("state[wait] timeout, room_id=~p, timestamp=~p", [RoomId, qp_util:timestamp()]),
	{stop, normal, State};
handle_info(_Info, StateName, State) ->
	?FILE_LOG_WARNING("info=~p, state_name=~p", [_Info, StateName]),
	{next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName,
	#state{
		room_id = RoomId,
		owner_basic_data = OwnerBasicData,
		room_cfg = RoomCfg,
		ob_set = ObSet,
		seat_tree = SeatTree,
		exit_make = ExitMake,
		user_basicdata_tree = UserBasicDataTree}) ->
	?FILE_LOG_DEBUG("room_id[~p] dissmiss, make=~p", [RoomId, ExitMake]),
	%%房间管理器回收房间ID
	room_mgr:destroy_room(RoomId),
	%%通知房间发送房间解散的消息
	%%给OB位的返送信息
	lists:foreach(
		fun(ObUserId) ->
			ObUserBasicData = gb_trees:get(ObUserId, UserBasicDataTree),
			ObUserBasicData:send_room_msg({dissmiss, {ExitMake, RoomId, self(), -1, OwnerBasicData:get(id), RoomCfg:get(aa)}})
		end, gb_sets:to_list(ObSet)),
	lists:foreach(
		fun({SeatNumber, SeatUserId}) ->
			if
				SeatUserId =/= undefined ->
					SetUserBasicData = gb_trees:get(SeatUserId, UserBasicDataTree),
					SetUserBasicData:send_room_msg({dissmiss, {ExitMake, RoomId, self(), SeatNumber, OwnerBasicData:get(id), RoomCfg:get(aa)}});
				true -> ok
			end
		end, gb_trees:to_list(SeatTree)),
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
	StateData :: #state{}, Extra :: term()) ->
	{ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%public_data转化成pb_room_user
make_roomuser(UserBasicData, SeatNum) ->
	#pb_room_user{
		user_public_data = make_publicdata(UserBasicData),
		seat_number = SeatNum}.

make_publicdata(UserBasicData) ->
	#pb_user_public_data{
		user_id = UserBasicData:get(id),
		avatar_url = UserBasicData:get(avatar_url),
		nick_name = UserBasicData:get(nickname)}.

make_roomcfg(RoomCfg) ->
	#pb_room_cfg{
		room_name = RoomCfg:get(name),
		is_aa = RoomCfg:get(aa),
		double_down_score = RoomCfg:get(double_down_score),
		is_laizi_playmethod = RoomCfg:get(is_laizi_playmethod),
		is_ob = RoomCfg:get(is_ob),
		is_random = RoomCfg:get(is_random),
		is_not_voice = RoomCfg:get(is_not_voice),
		is_safe_mode = RoomCfg:get(is_safe_mode),
		lock_userid_list = RoomCfg:get(lock_id_list)
	}.


make_pbroomdata(RoomId, RoomCfg, ObSet, SeatTree, UserBasicDataTree, GameData) ->
	UserDataList =
		lists:map(
			fun(ObUserId) ->
				ObUserBasicData = gb_trees:get(ObUserId, UserBasicDataTree),
				{ObUserBasicData, -1}
			end, gb_sets:to_list(ObSet)),
	%%下发座位上选手的信息
	UserDataList2 =
		lists:foldl(
			fun({SeatNum, SeatUserId}, TmpPbRoomUserList) ->
				if
					SeatUserId =/= undefined ->
						SeatUserBasicData = gb_trees:get(SeatUserId, UserBasicDataTree),
						[{SeatUserBasicData, SeatNum}|TmpPbRoomUserList];
					true -> TmpPbRoomUserList
				end
			end, UserDataList, gb_trees:to_list(SeatTree)),
	AllRoomUsers =
		lists:map(
			fun({TmpUserBasicData, TmpSeatNum}) ->
				make_roomuser(TmpUserBasicData, TmpSeatNum)
			end, UserDataList2),
	#pb_room_data{
		room_id = RoomId,
		cfg = make_roomcfg(RoomCfg),
		room_users = AllRoomUsers,
		game_data = GameData}.


select_seatnum(SeatTree, UserId, SeatNum, IsRandom, LockIdList) ->
	%%计算能够坐下的剩余位置
	if
		LockIdList =:= [] ->
			select_seatnum(SeatTree, UserId, SeatNum, IsRandom);
		true ->
			case lists:member(UserId, LockIdList) of
				true ->
					%%在锁定列表里
					select_seatnum(SeatTree, UserId, SeatNum, IsRandom);
				false ->
					LockIdCount = length(LockIdList),
					%%不在锁定列表里
					if
						LockIdCount =:= 4 ->
							{SeatTree, ?LOGIC_ERROR_SEAT_FULL};
						true ->
							AvailableSeatCount =
								lists:foldr(
									fun(TmpUserId, TmpAvailableSeatCount) ->
										if
											TmpUserId =:= undefined -> TmpAvailableSeatCount;
											true ->
												case lists:member(TmpUserId, LockIdList) of
													true -> TmpAvailableSeatCount;
													false ->  TmpAvailableSeatCount - 1
												end
										end
									end, 4 - LockIdCount, gb_trees:values(SeatTree)),
							if
								AvailableSeatCount > 0 ->
									select_seatnum(SeatTree, UserId, SeatNum, IsRandom);
								true ->
									{SeatTree, ?LOGIC_ERROR_SEAT_FULL}
							end
					end
			end
	end.

select_seatnum(SeatTree, UserId, SeatNum, IsRandom) when IsRandom =:= true orelse (SeatNum < 0 andalso SeatNum > 3) ->
	%%随机选择
	case random_select_seatnum(SeatTree) of
		full ->
			{SeatTree, ?LOGIC_ERROR_SEAT_FULL};
		SelectSeatNum when is_integer(SelectSeatNum) -> {gb_trees:update(SelectSeatNum, UserId, SeatTree), SelectSeatNum, 0}
	end;
select_seatnum(SeatTree, UserId, SeatNum, _) ->
	case gb_trees:get(SeatNum, SeatTree) of
		{value, undefined} -> {gb_trees:update(SeatNum, UserId, SeatTree), SeatNum, 0};
		{value, _} -> {SeatTree, ?LOGIC_ERROR_SEAT_SOMEONE}
	end.


random_select_seatnum(SeatTree) ->
	LL =
		lists:foldr(
			fun({SeatNum, UserId}, L) ->
				if
					UserId =:= undefined -> [SeatNum|L];
					true -> L
				end
			end, [], gb_trees:to_list(SeatTree)),
	?FILE_LOG_DEBUG("ll = ~p", [LL]),
	if
		LL =:= [] -> full;
		true ->
			Index = qp_util:random_in_range(1, length(LL)),
			lists:nth(Index, LL)
	end.

room_broadcast(UserBasicDataTree, Msg) ->
	lists:foreach(
		fun(RoomUserBasicData) ->
			RoomUserBasicData:send_room_msg(Msg)
		end, gb_trees:values(UserBasicDataTree)).

room_broadcast(UserBasicDataTree, Msg, FilterUserId) ->
	lists:foreach(
		fun(RoomUserBasicData) ->
			case RoomUserBasicData:get(id) of
				FilterUserId -> ok;
				_ -> RoomUserBasicData:send_room_msg(Msg)
			end
		end, gb_trees:values(UserBasicDataTree)).


%%给座位上的人广播
room_seat_broadcast(UserBasicDataTree, SeatTree, Msg) ->
	lists:foreach(
		fun(SeatUserId) ->
			UserBasicData = gb_trees:get(SeatUserId, UserBasicDataTree),
			UserBasicData:send_room_msg(Msg)
		end, gb_trees:values(SeatTree)).


check_full(SeatTree) ->
	SeatCount =
		lists:foldl(
			fun({_SeatNum, UserId}, Count) ->
				if
					UserId =/= undefined -> Count + 1;
					true -> Count
				end
			end, 0, gb_trees:to_list(SeatTree)),
	SeatCount =:= 4.


remove_obuser(_RoomId, ObSet, _SeatTree, UserBasicDataTree, true) -> {ObSet, UserBasicDataTree};
remove_obuser(RoomId, ObSet, SeatTree, UserBasicDataTree, false) ->
	lists:foreach(
		fun(ObUserId) ->
			Push = #qp_exit_room_push{user_id = ObUserId, seat_num = -1},
			PushBin = qp_proto:encode_qp_packet(Push),
			room_seat_broadcast(UserBasicDataTree, SeatTree, {bin, PushBin}),
			ObUserBasicData = gb_trees:get(ObUserId, UserBasicDataTree),
			ObUserBasicData:send_room_msg({kick, {RoomId, self(), ?RT_GAME_START_NOT_OB}})
		end, gb_sets:to_list(ObSet)),
	NewUserBasicDataTree =
		lists:foldl(
			fun(ObUserId, T) ->
				gb_trees:delete(ObUserId, T)
			end, UserBasicDataTree, gb_sets:to_list(ObSet)),
	{gb_sets:empty(), NewUserBasicDataTree}.
