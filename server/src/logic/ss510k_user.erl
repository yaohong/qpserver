%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 一月 2018 12:10
%%%-------------------------------------------------------------------
-module(ss510k_user).
-author("yaohong").

-behaviour(gen_fsm).
-include("../../deps/file_log/include/file_log.hrl").
-include("../qp_error.hrl").
-include("../qp_define.hrl").
%% API
-export([start/1, start_link/1]).

%% gen_fsm callbacks
-export([init/1,
	hall/2, room/2,
	hall/3, room/3,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).
-export([post/3]).
-define(SERVER, ?MODULE).

-record(state, {
	user_id,
	nickname :: string(),
	avatar_url :: string(),

	card_count :: integer(),            %%房卡数量
	lock_card_count :: integer(),       %%锁定的房卡数量

	token_data :: term(),               %%当前qpuser绑定的数据

	room_logic_mod,

	room_pid :: pid() | undefined      %%当前缩在房间的进程PID

}).

%%%===================================================================
%%% API
%%%===================================================================
%%参数由两部分组成{token_data, event_data}.
post(Pid, From, Param) ->
	gen_fsm:send_event(Pid, {From, Param}).
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------

-spec start(UserId :: integer()) ->
	{ok, pid()}.
start(UserId) ->
	supervisor:start_child(room_user_sup, [UserId]).

start_link(UserId) ->
	gen_fsm:start_link(?MODULE, [UserId], []).

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
init([UserId]) ->
	%%{stop, test}.
	?FILE_LOG_DEBUG("ss510k_user start success, user_id=~p", [UserId]),
	%%从数据库加载信息

	case qp_db:load_game_data(UserId) of
		{success, {RoomCard, NickName, AvatarUrl}, _} ->
			{success, RoomLogicMod} = qp_config:get_cfg(room_logic_mod),
			{ok,
				hall,
				#state{
					user_id = UserId,
					nickname = NickName,
					avatar_url = AvatarUrl,
					card_count = RoomCard,
					lock_card_count = 0,
					token_data = undefined,
					room_logic_mod = RoomLogicMod,

					room_pid = undefined
				}};
		{failed, DbErrorCode} ->
			?FILE_LOG_ERROR("load_game_data failed, ~p", [DbErrorCode]),
			{stop, load_data_failed}
	end.

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
-spec(hall(Event :: term(), State :: #state{}) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).

hall({From, {login, QpUserPid}}, #state{token_data = OldTokenData, card_count = CardCount, nickname = Nickname, avatar_url = AvatarUrl} = State) ->
	try
		if
			OldTokenData =/= undefined ->
				%%之前有绑定qpuserid
				%%降之前的T下线
				?FILE_LOG_DEBUG("old_token_data=~p, kick", [OldTokenData]),
					catch OldTokenData:kick(),
				ok;
			true -> ok
		end,

		NewTokenData = token_data:new(QpUserPid),
		?FILE_LOG_DEBUG("[hall]connect success, [~p]=>[~p]", [OldTokenData, NewTokenData]),
		From:reply({success, {NewTokenData, {CardCount, Nickname, AvatarUrl, undefined}}}),
		{next_state, hall, State#state{token_data = NewTokenData}}
	catch
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, hall, State}
	end;
hall({From, {disconnect, ReqTokenData}}, #state{token_data = TokenData, user_id = UserId} = State) ->
	try
		compare_token(ReqTokenData, TokenData),
		From:reply(success),
		?FILE_LOG_DEBUG("[hall]user_id=~p, ~p, disconnect", [UserId, TokenData]),
		{next_state, hall, State#state{token_data = undefined}}
	catch
		throw:{custom, ErrorCode} when is_integer(ErrorCode) ->
			From:reply({failed, ErrorCode}),
			{next_state, hall, State};
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, hall, State}
	end;
hall({From, {ReqTokenData, {create_room, {RoomName, AA, DoubleDownScore, IsLaiziPlaymethod, IsOb, IsRandom, IsNotVoice, IsSafeMode, LockIdList}}}},
	#state{
		user_id = UserId,
		nickname = Nickname,
		avatar_url = AvatarUrl,

		token_data = TokenData,

		card_count = CardCount,
		lock_card_count = LockCardCount,

		room_logic_mod = RoomLogicMod} = State) ->
	try
		compare_token(ReqTokenData, TokenData),
		ConsumeCardCount =
			if
				AA =:= true -> 1;
				true -> 4
			end,
		ValidCardCount = valid_card_count(CardCount, LockCardCount),
		if
			ValidCardCount < ConsumeCardCount ->
				%%房卡不够不能开
				?FILE_LOG_WARNING("need card_count=~p, valid_card_count=~p", [ConsumeCardCount, ValidCardCount]),
				throw({custom, ?LOGIC_ERROR_CARD_NOT_ENOUGH});
			true -> ok
		end,
		RoomCfg = ss510k_room_cfg:new(RoomName, AA, DoubleDownScore, IsLaiziPlaymethod, IsOb, IsRandom, IsNotVoice, IsSafeMode, LockIdList),
		UserBasicData = user_basic_data:new(UserId, self(), Nickname, AvatarUrl),
		{success, {RoomId, RoomPid}} = room_mgr:create_room(UserBasicData, RoomCfg),
		success = RoomLogicMod:join_room(RoomPid, UserBasicData),
		From:reply({success, RoomId}),
		NewLockCardCount = LockCardCount + ConsumeCardCount,
		?FILE_LOG_DEBUG("user_id[~p] create_room_success, lock_card_count[~p=>~p] hall=>room, room_pid=~p", [
			UserId, LockCardCount, NewLockCardCount, RoomPid]),
		{next_state, room, State#state{lock_card_count = NewLockCardCount, room_pid = RoomPid}}
	catch
		throw:{custom, ErrorCode} when is_integer(ErrorCode) ->
			From:reply({failed, ErrorCode}),
			{next_state, hall, State};
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, hall, State}
	end;
hall({From, {ReqTokenData, {join_room, RoomId}}},
	#state{
		user_id = UserId,
		nickname = Nickname,
		avatar_url = AvatarUrl,

		token_data = TokenData,

		room_logic_mod = RoomLogicMod} = State) ->
	try
		compare_token(ReqTokenData, TokenData),

		UserBasicData = user_basic_data:new(UserId, self(), Nickname, AvatarUrl),
		RoomPid = get_room_pid(RoomId),

		success = RoomLogicMod:join_room(RoomPid, UserBasicData),
		From:reply(success),

		?FILE_LOG_DEBUG("user_id[~p] join_room_success", [UserId]),
		{next_state, room, State#state{room_pid = RoomPid}}
	catch
		throw:{custom, ErrorCode} when is_integer(ErrorCode) ->
			From:reply({failed, ErrorCode}),
			{next_state, hall, State};
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, hall, State}
	end;
hall(Event, State) ->
	?FILE_LOG_WARNING("room hall, event=~p", [Event]),
	{next_state, hall, State}.

room({From, {login, QpUserPid}}, #state{token_data = OldTokenData, card_count = CardCount, nickname = Nickname, avatar_url = AvatarUrl, room_pid = RoomPid} = State) ->
	try
		if
			OldTokenData =/= undefined ->
				%%之前有绑定qpuserid
				%%降之前的T下线
				?FILE_LOG_DEBUG("old_token_data=~p, kick", [OldTokenData]),
					catch OldTokenData:kick(),
				ok;
			true -> ok
		end,

		%%从房间获取数据下发
		PbRoomData = get_room_pbdata(RoomPid),

		NewTokenData = token_data:new(QpUserPid),
		?FILE_LOG_DEBUG("[room]connect success, [~p]=>[~p]", [OldTokenData, NewTokenData]),
		From:reply({success, {NewTokenData, {CardCount, Nickname, AvatarUrl, PbRoomData}}}),
		{next_state, room, State#state{token_data = NewTokenData}}
	catch
		throw:{custom, ErrorCode} when is_integer(ErrorCode) ->
			From:reply({failed, ErrorCode}),
			{next_state, room, State};
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, room, State}
	end;
room({From, {disconnect, ReqTokenData}}, #state{token_data = TokenData, user_id = UserId} = State) ->
	try
		compare_token(ReqTokenData, TokenData),
		From:reply(success),
		%%通知房间有人断开链接了
		%%%%%%%%%%%%%%%%%%%%%
		?FILE_LOG_DEBUG("[room]user_id=~p, ~p, disconnect", [UserId, TokenData]),
		{next_state, room, State#state{token_data = undefined}}
	catch
		throw:{custom, ErrorCode} when is_integer(ErrorCode) ->
			From:reply({failed, ErrorCode}),
			{next_state, room, State};
		What:Type ->
			?printSystemError(What, Type),
			From:reply({failed, ?SYSTEM_ERROR}),
			{next_state, room, State}
	end;
room({From, Event}, State) ->
	?FILE_LOG_WARNING("room_room, event=~p", [Event]),
	From:reply(ignore),
	{next_state, room, State}.

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
-spec(hall(Event :: term(), From :: {pid(), term()},
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

hall(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, hall, State}.


room(_Event, _From, State) ->
	Reply = ok,
	{reply, Reply, room, State}.

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
handle_info({room_msg, {bin, Bin}}, room, #state{token_data = TokenData} = State) ->
	%%只有在房间才接收room_bin
	TokenData:send_bin(Bin),
	{next_state, room, State};
handle_info(
	{room_msg, {dissmiss, {ExitMake, DissmissRoomPid, SeatNumber, RoomOwnerId, IsAA}}}, room,
	#state{user_id = UserId, room_pid = CurrentRoomPid, card_count = CardCount, lock_card_count = LockCardCount} = State) ->
	?FILE_LOG_DEBUG(
		"room_dissmiss, exitMake=~p, dissmissRoomPid=~p, seat_number=~p, roomOwnerId=~p, isAA=~p user[~p] room=>hall",
		[ExitMake, DissmissRoomPid, SeatNumber, RoomOwnerId, IsAA, UserId]),
	if
		DissmissRoomPid =/= CurrentRoomPid ->
			?FILE_LOG_WARNING("user_id=~p, room_diss_exception, currentRoomPid=~p", [UserId, CurrentRoomPid]),
			{next_state, room, State};
		true ->
			{NewCardCount, NewLockCardCount} =
				if
					SeatNumber =:= -1 andalso RoomOwnerId =/= UserId ->
						{CardCount, LockCardCount};
					true ->
						if
							IsAA =:= true ->
								if
									ExitMake =:= 0 -> {CardCount, LockCardCount - 1};
									true -> {CardCount - 1, LockCardCount - 1}
								end;
							true ->
								%%房主一个人包了
								if
									RoomOwnerId =:= UserId ->
										if
											ExitMake =:= 0 -> {CardCount, LockCardCount - 4};
											true -> {CardCount - 4, LockCardCount - 4}
										end;
									true ->
										{CardCount, LockCardCount}
								end
						end
				end,

			?FILE_LOG_DEBUG(
				"user_id=~p, room_owner_user_id=~p, card_count[~p=>~p] lock_card_count[~p=>~p]",
				[UserId, RoomOwnerId, CardCount, NewCardCount, LockCardCount, NewLockCardCount]),
			?FILE_LOG_DEBUG("user_id=~p room=>hall.", [UserId]),
			%%如果当前有qp_user的绑定
			{next_state, hall, State#state{room_pid = undefined, card_count = NewCardCount, lock_card_count = NewLockCardCount}}
	end;
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
valid_card_count(CardCount, LockCardCount) -> CardCount - LockCardCount.

compare_token(LTokenData, RTokenData) ->
	case LTokenData:compare(RTokenData) of
		true -> true;
		false ->
			?FILE_LOG_ERROR("token compare failed, ~p, ~p", [LTokenData, RTokenData]),
			throw({custom, ?SYSTEM_TOKEN_ERROR})
	end.


get_room_pid(RoomId) ->
	case room_mgr:get_room_pid(RoomId) of
		{success, RoomPid} -> RoomPid;
		failed -> throw({custom, ?LOGIC_ERROR_GET_ROOM_PID_FAILED})
	end.


get_room_pbdata(RoomPid) ->
	case catch ss510k_room:get_room_pbdata(RoomPid) of
		{success, PbRoomData} -> PbRoomData;
		Other ->
			?FILE_LOG_WARNING("get_room_info failed, ~p", [Other]),
			throw({custom, ?LOGIC_ERROR_GET_ROOM_INFO_FAILED})
	end.