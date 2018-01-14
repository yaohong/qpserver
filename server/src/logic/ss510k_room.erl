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
-include("ss510k_error.hrl").
-include("../../deps/file_log/include/file_log.hrl").
-include("../../include/common_pb.hrl").
-behaviour(gen_fsm).

%% API
-export([start_link/5]).

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
	sitdown/3
]).


-define(SERVER, ?MODULE).
-define(CHECK_GAME_WHETHER_TO_START_TIMEOUT, 10 * 60).
-record(state, {
	room_id :: integer(),
	room_name :: list(),
	owner_id :: integer(),
	lock_id_list :: list(),                 %%只允许这些人进房
	room_cfg :: any(),


	ob_set :: gb_sets:set(),                %%存放观众的集合


	user_basicdata_tree :: gb_trees:tree(),   %%存放玩家基础数据的树

	seat_tree :: gb_trees:tree()            %%描述座位信息
}).

%%%===================================================================
%%% API
%%%===================================================================
%%进入房间携带账号基础信息(id,昵称,头像)
join_room(RoomPid, UserBasicData) ->
	gen_fsm:sync_send_event(RoomPid, {join_room, UserBasicData}, infinity).

sitdown(RoomPid, UserId, SeatNum) ->
	gen_fsm:sync_send_event(RoomPid, {sitdown, {UserId, SeatNum}}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
-spec(start_link(RoomId :: integer(), RoomName :: list(), OwnerId :: integer(), LockIdList :: [integer()], RoomCfg :: any()) -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link(RoomId, RoomName, OwnerId, LockIdList, RoomCfg) ->
	gen_fsm:start_link(?MODULE, [RoomId, RoomName, OwnerId, LockIdList, RoomCfg], []).

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
init([RoomId, RoomName, OwnerBasicData, LockIdList, RoomCfg]) ->
	%%启动定时器，多久没开始游戏，自动退出
	CurrentTime = qp_util:timestamp(),
	timer_manager:addDelayTask(RoomId, CurrentTime + ?CHECK_GAME_WHETHER_TO_START_TIMEOUT, ?MODULE, cb_check_game_whethertostart, [self()]),
	Id = OwnerBasicData:get(id),
	?FILE_LOG_DEBUG("room_init room_id=~p, room_name=~ts, owner_id=~p, lock_id_list=~p, room_cfg=~p", [RoomId, RoomName, Id, LockIdList, RoomCfg]),
	T1 = gb_trees:insert(0, undefined, gb_trees:empty()),
	T2 = gb_trees:insert(1, undefined, T1),
	T3 = gb_trees:insert(2, undefined, T2),
	T4 = gb_trees:insert(3, undefined, T3),
	{ok,
		wait,
		#state{
			room_id = RoomId,
			room_name = RoomName,
			owner_id = Id,
			lock_id_list = LockIdList,
			room_cfg = RoomCfg,
			ob_set = gb_sets:insert(Id, gb_sets:empty()),
			user_basicdata_tree = gb_trees:insert(Id, OwnerBasicData, gb_trees:empty()),
			seat_tree = T4
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
wait({join_room, UserBasicData}, _From, State) ->
	#state{
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree
	} = State,
	?FILE_LOG_DEBUG("join_room ~p", [UserBasicData]),
	UserId = UserBasicData:get(id),
	case gb_trees:is_defined(UserId, UserBasicDataTree) of
		true ->
			%%之前已经在里面了
			{reply, {failed, ?SS510K_ERROR_REPEAT_JOIN}, wait, State};
		false ->
			NewUserBasicData = gb_trees:insert(UserId, UserBasicData, UserBasicDataTree),
			NewObSet = gb_sets:insert(UserId, ObSet),
			%%下发OB选手的信息
			PbRoomUserDataList =
				lists:map(
					fun(ObUserId) ->
						ObUserBasicData = gb_trees:get(ObUserId, UserBasicDataTree),
						make_roomuser(ObUserBasicData, -1)
					end, gb_sets:to_list(ObSet)),
			%%下发座位上选手的信息
			PbRoomUserDataList2 =
				lists:foldl(
					fun({SeatNum, SeatUserId}, TmpPbRoomUserList) ->
						if
							SeatUserId =/= undefined ->
								SeatUserBasicData = gb_trees:get(SeatUserId, UserBasicDataTree),
								[make_roomuser(SeatUserBasicData, SeatNum)|TmpPbRoomUserList];
							true -> TmpPbRoomUserList
						end
					end, PbRoomUserDataList, gb_trees:to_list(SeatTree)),
			PbJoinRoomRsp = #qp_join_room_rsp{result = 0, room_users = PbRoomUserDataList2},
			RspPbBin = qp_proto:encode_qp_packet(PbJoinRoomRsp),
			UserBasicData:send_room_bin(RspPbBin),

			PbJoinRoomPush = #qp_join_room_push{public_data = make_publicdata(NewUserBasicData)},
			PushPbBin = qp_proto:encode_qp_packet(PbJoinRoomPush),
			%%给房间里的其他人发送玩家进入的消息
			lists:foreach(
				fun(RoomUserId) ->
					RoomUserBasicData = gb_trees:get(RoomUserId, UserBasicDataTree),
					RoomUserBasicData:send_room_bin(PushPbBin)
				end, gb_trees:keys(UserBasicDataTree)),
			{reply, success, wait, State#state{user_basicdata_tree = NewUserBasicData, ob_set = NewObSet}}
	end;
wait({sitdown, {sitdown, {UserId, SeatNum}}}, _From, State) ->
	#state{
		user_basicdata_tree = UserBasicDataTree,
		ob_set = ObSet,
		seat_tree = SeatTree,
		room_cfg = RoomCfg
	} = State,
	?FILE_LOG_DEBUG("sitdown user_id=~p seat_num=~p", [UserId, SeatNum]),
	case gb_trees:lookup(UserId, UserBasicDataTree) of
		none ->
			{reply, {failed, ?SS510K_ERROR_NOT_IN_ROOM_NOT_SITDOWN}, wait, State};
		{value, _} ->
			case gb_sets:is_element(UserId, ObSet) of
				false ->
					{reply, {failed, ?SS510K_ERROR_NOT_OB_NOT_SITDOWN}, wait, State};
				true ->
					case select_seatnum(SeatTree, UserId, SeatNum, RoomCfg:get(is_random)) of
						{_, ErrorCode} when ErrorCode > 0 ->
							{reply, {failed, ErrorCode}, wait, State};
						{NewSeatTree, SelectSeatNum, _} ->
							{reply, {success, SelectSeatNum}, wait, State#state{seat_tree = NewSeatTree}}
					end
			end
	end;
wait(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, state_name, State}.

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
handle_info(_Info, StateName, State) ->
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
terminate(_Reason, _StateName, _State) ->
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




select_seatnum(SeatTree, UserId, SeatNum, IsRandom) when IsRandom =:= true orelse (SeatNum < 0 andalso SeatNum > 3) ->
	%%随机选择
	case random_select_seatnum(SeatTree) of
		full -> {SeatTree, ?SS510K_ERROR_SEAT_FULL};
		SelectSeatNum when is_integer(SelectSeatNum) -> {gb_trees:update(SelectSeatNum, UserId, SeatTree), SelectSeatNum, 0}
	end;
select_seatnum(SeatTree, UserId, SeatNum, _) ->
	case gb_trees:get(SeatNum, SeatTree) of
		{value, undefined} -> {gb_trees:update(SeatNum, UserId, SeatTree), SeatNum, 0};
		{value, _} -> {SeatTree, ?SS510K_ERROR_SEAT_SOMEONE}
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
	if
		LL =:= [] -> full;
		true ->
			Index = qp_util:random_in_range(1, length(LL)),
			lists:nth(Index, LL)
	end.