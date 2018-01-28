%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. 十月 2016 19:49
%%%-------------------------------------------------------------------
-module(qp_user).
-author("yaohong").

-behaviour(gen_fsm).

-include("qp_type.hrl").
-include("../deps/file_log/include/file_log.hrl").
-include("./proto/qp_proto.hrl").
-include("../include/common_pb.hrl").
%% API

%% gen_fsm callbacks
-export([init/1,
	wait_login/2,              	%%等待登陆
	login_success/2,              %%登陆成功
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4]).
-export([head_len/2,
	closed/1,
	complete_packet/2,
	timer_callback/2, exit_callback/2
]).
-export([
	start/3,
	start_link/2
]).
-define(SERVER, ?MODULE).
-define(TIMER_SPACE, 10).
-define(EXIT_WAIT_TIME, 10).
-define(RECV_TIMEOUT, 60).

-record(state, {
	user_id=undefined,
	receiveMonitor,
	sockModule,
	sockData,
	last_recv_packet_time,

	token_data
}).

%%%===================================================================
%%% API
%%%===================================================================
head_len(HeadBin, _HeadLen) when is_binary(HeadBin) ->
	%%读取头
	<<PacketSize:?BIG_UINT32>> = HeadBin,
	PacketSize.

closed(Pid) when is_pid(Pid) ->
	Pid ! closed.

complete_packet(Pid, Bin) when is_pid(Pid) andalso is_binary(Bin) ->
	gen_fsm:send_all_state_event(Pid, {complete_packet, Bin}).

timer_callback(_Ref, Pid) ->
	Pid ! timeout_check.

exit_callback(_Ref, Pid) ->
	Pid ! exit.

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------


-spec start(UserSup :: atom(), SockModule :: module(), Socket :: tcp_socket:socket()) ->
	{ok, pid()}.
start(UserSup, SockModule, SocketData) ->
	supervisor:start_child(UserSup, [SockModule, SocketData]).

start_link(SockModule, Socket) ->
	gen_fsm:start_link(?MODULE, [SockModule, Socket], []).

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
init([SockModule, SocketData]) ->
	ReceiveMonitor = SockModule:monitor(SocketData),
	case catch SockModule:peername(SocketData) of
		{ok, _} ->
			CurrentTime = qp_util:timestamp(),
			timer_manager:addDelayTask(
				CurrentTime,
				CurrentTime + ?TIMER_SPACE,
				qp_user, timer_callback, [self()]),
			{ok,
				wait_login,
				#state{
					receiveMonitor = ReceiveMonitor,
					sockModule = SockModule,
					sockData = SocketData,
					last_recv_packet_time = CurrentTime,

					token_data = undefined
				}};
		Other ->
			?FILE_LOG_ERROR("socket init peername fail reason=[~p]", [Other]),
			{stop, normal}
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
-spec(wait_login(Event :: term(), State :: #state{}) ->
	{next_state, NextStateName :: atom(), NextState :: #state{}} |
	{next_state, NextStateName :: atom(), NextState :: #state{},
		timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
wait_login(Event, State) ->
	?FILE_LOG_WARNING("~p", [Event]),
	{next_state, wait_login, State}.


login_success(Event, State) ->
	?FILE_LOG_WARNING("~p ~p", [Event, State]),
	{next_state, hall, State}.

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
%%-spec(state_name(Event :: term(), From :: {pid(), term()},
%%                 State :: #state{}) ->
%%                    {next_state, NextStateName :: atom(), NextState :: #state{}} |
%%                    {next_state, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
%%                    {reply, Reply, NextStateName :: atom(), NextState :: #state{},
%%                     timeout() | hibernate} |
%%                    {stop, Reason :: normal | term(), NewState :: #state{}} |
%%                    {stop, Reason :: normal | term(), Reply :: term(),
%%                     NewState :: #state{}}).
%%state_name(_Event, _From, State) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, State}.

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
handle_event({complete_packet, Bin}, StateName, #state{last_recv_packet_time = OldLastRecvPacketTime} = State) ->
	try
		Request = qp_proto:decode_qp_packet(Bin),
		{NewStateName, NewState, IsUpdate} = packet_handle(Request, StateName, State),
		true = is_record(NewState, state),
		true = (IsUpdate =:= true orelse IsUpdate =:= false),
		state_name_check(NewStateName),

		NewLastRecvPacketTime =
			if
				IsUpdate =:= true -> qp_util:timestamp();
				true -> OldLastRecvPacketTime
			end,
		{next_state, NewStateName, NewState#state{last_recv_packet_time = NewLastRecvPacketTime}}
	catch
		throw:{custom, Error} ->
			?FILE_LOG_DEBUG("custom, error=~p", [Error]),
			{stop, normal, State};
		What:Type ->
			?FILE_LOG_ERROR("~p, ~p, ~p", [What, Type, erlang:get_stacktrace()]),
			{stop, normal, State}
	end;
handle_event(_Event, StateName, State) ->
	{next_state, StateName, State}.

state_name_check(wait_login) -> ok;
state_name_check(login_success) -> ok;
state_name_check(wait_exit) -> ok.

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
handle_info(closed, _StateName, State) ->
	?FILE_LOG_DEBUG("qp_user socket close", []),
	{stop, normal, State};
handle_info(timeout_check, StateName, #state{last_recv_packet_time = LastRecvPackTime} = State) ->
	CurrentTime = qp_util:timestamp(),
	SpacheTime = CurrentTime - LastRecvPackTime,
	if
		SpacheTime > ?RECV_TIMEOUT ->
			?FILE_LOG_DEBUG("timeout stat_name=~p", [StateName]),
			{stop,normal,State};
		true ->
			timer_manager:addDelayTask(
				CurrentTime,
				CurrentTime + ?TIMER_SPACE,
				qp_user, timer_callback, [self()]),
			{next_state, StateName, State}
	end;
handle_info({logic_msg, {bin, Bin}}, login_success, State) ->
	send_bin(Bin, State),
	{next_state, login_success, State};

handle_info({logic_msg, {room_dissmiss, {RoomId, ExitType}}}, login_success, #state{user_id = UserId} = State) ->
	?FILE_LOG_DEBUG("user_id=~p, room_dissmiss, room_id=~p, exit_type=~p", [UserId, RoomId, ExitType]),
	send_packet(#qp_room_dissmiss{room_id = RoomId, type = ExitType}, State),
	{next_state, login_success, State};

handle_info({logic_msg, {room_kick, {RoomId, KickType}}}, login_success, #state{user_id = UserId} = State) ->
	?FILE_LOG_DEBUG("user_id=~p, room_kick, room_id=~p, kick_type=~p", [UserId, RoomId, KickType]),
	send_packet(#qp_room_kick{room_id = RoomId, user_id = UserId, type = KickType}, State),
	{next_state, login_success, State};

handle_info(kick, login_success, State) ->
	?FILE_LOG_DEBUG("kick ~p ~p", [self(), login_success]),
	send_packet(#qp_kick{noop = 1}, State),
	CurrentTime = qp_util:timestamp(),
	timer_manager:addDelayTask(
		CurrentTime,
		CurrentTime + ?EXIT_WAIT_TIME,
		qp_user, exit_callback, [self()]),
	{next_state, wait_exit, State#state{token_data = undefined}};

handle_info(exit, wait_exit, #state{user_id = UserId} = State) ->
	?FILE_LOG_DEBUG("exit ~p", [UserId]),
	{stop, normal, State};
handle_info(_Info, StateName, State) ->
	?FILE_LOG_WARNING("unkown ino ~p, statName=~p", [_Info, StateName]),
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
| term(), StateName :: atom(), StateData :: term()) ->
	term()).
terminate(_Reason, _StateName, #state{user_id = UserId, token_data = TokenData} = State) ->
	?FILE_LOG_DEBUG("qp_user ~p, ~p terminate", [UserId, TokenData]),
	if
		TokenData =/= undefined ->
			Reply = room_user_mgr:request(UserId, {disconnect, TokenData}),
			?FILE_LOG_DEBUG("disconnect reply=~p", [Reply]),
			ok;
		true -> ok
	end,
	(State#state.sockModule):close(State#state.sockData),
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

send_packet(Packet, State) when is_record(State, state) ->
	RspBin = qp_proto:encode_qp_packet(Packet),
	send_bin(RspBin, State).
send_bin(Bin, State) when is_binary(Bin) andalso is_record(State, state)->
	(State#state.sockModule):send(State#state.sockData, Bin).



packet_handle(#qp_login_req{account = Acc, pwd = Pwd}, wait_login, State) ->
	?FILE_LOG_DEBUG("login_req acc=~p, pwd=~p", [Acc, Pwd]),
	case qp_db:account_verify(Acc, Pwd) of
		{success, UserId, IsCreate} ->
			?FILE_LOG_DEBUG("account_verify success, user_id=~p, is_create=~p", [UserId, IsCreate]),
			{success, {TokenData, {CardCount, Nickname, AvatarUrl, PbRoomData}}} = room_user_mgr:request(UserId, {login, self()}),
			?FILE_LOG_DEBUG("login success, token_data=~p", [TokenData]),
			PublicData = #pb_user_public_data{user_id = UserId, nick_name = Nickname, avatar_url = AvatarUrl},
			PrivateData = #pb_user_private_data{room_card_count = CardCount},
			Rsp = #qp_login_rsp{state = 0, public_data = PublicData, private_data = PrivateData, room_data = PbRoomData},
			send_packet(Rsp, State),
			{login_success, State#state{user_id = UserId, token_data = TokenData}, true};
		{failed, ErrorCode} ->
			?FILE_LOG_DEBUG("login_req failed, code=~p", [ErrorCode]),
			ErrorRsp = #qp_login_rsp{state = ErrorCode},
			send_packet(ErrorRsp, State),
			{wait_login, State, false}
	end;
packet_handle(Req, wait_login, _State) ->
	?FILE_LOG_DEBUG("wait_login, req=~p", [Req]),
	throw({custom, state_error});

packet_handle(#qp_create_room_req{cfg = RoomCfg}, login_success, #state{user_id = UserId, token_data = TokenData} = State) ->
	#pb_room_cfg{
		room_name = RoomName,
		is_aa = AA,
		double_down_score = DoubleDownScore,
		is_laizi_playmethod = IsLaiziPlaymethod,
		is_ob = IsOb,
		is_random = IsRandom,
		is_not_voice = IsNotVoice,
		is_safe_mode = IsSafeMode,
		lock_userid_list = LockUserIdList
	} = RoomCfg,

	?FILE_LOG_DEBUG("room_name=~ts, is_aa=~p, double_down_score=~p, is_laizi_playmethod=~p, is_ob=~p, is_random=~p, is_not_voice=~p, is_safe_mode=~p, lock_id_list=~p",
		[RoomName, AA, DoubleDownScore, IsLaiziPlaymethod, IsOb, IsRandom, IsNotVoice, IsSafeMode, undefined_to_empty(LockUserIdList)]),

	case room_user_mgr:request(
		UserId,
		{
			TokenData,
			{
				create_room,
				{RoomName, AA, DoubleDownScore, IsLaiziPlaymethod, IsOb, IsRandom, IsNotVoice, IsSafeMode, undefined_to_empty(LockUserIdList)}
			}
		}) of
		{success, RoomId} ->
			?FILE_LOG_DEBUG("user_id=~p, create_room success, room_id=~p", [UserId, RoomId]),
			send_packet(#qp_create_room_rsp{state = 0, room_id = RoomId}, State);
		{failed, ErrorCode} ->
			send_packet(#qp_create_room_rsp{state = ErrorCode}, State);
		ignore ->
			%%状态不对忽略
			%%不给客户端发送相应包
			?FILE_LOG_WARNING("user_id=~p, create_room ignore", [UserId]),
			ok
	end,
	{login_success, State, true};
packet_handle(#qp_join_room_req{room_id = RoomId}, login_success, #state{user_id = UserId, token_data = TokenData} = State) ->

	?FILE_LOG_DEBUG("user_id ~p join_room, room_id=~p", [UserId,RoomId]),
	case room_user_mgr:request(UserId, {TokenData, {join_room, RoomId}}) of
		success ->
			?FILE_LOG_DEBUG("user_id ~p join_room success", [UserId]),
			ok;      %%成功了不发包
		{failed, ErrorCode} ->
			%%失败了，发送返回
			?FILE_LOG_DEBUG("user_id ~p join_room failed, code=~p", [UserId, ErrorCode]),
			send_packet(#qp_join_room_rsp{result = ErrorCode}, State),
			ok;
		ignore ->
			?FILE_LOG_WARNING("user_id=~p, join_room[~p] ignore", [UserId, RoomId]),
			ok
	end,
	{login_success, State, true};
packet_handle(#qp_ping_req{}, login_success, #state{user_id = UserId} = State) ->

	?FILE_LOG_DEBUG("user_id ~p ping", [UserId]),
	send_packet(#qp_ping_rsp{noop = 0}, State),
	{login_success, State, true};
packet_handle(#qp_sitdown_req{seat_num = SeatNum}, login_success, #state{user_id = UserId, token_data = TokenData} = State) ->
	?FILE_LOG_DEBUG("user_id=~p sitdown seat_num=~p", [UserId, SeatNum]),
	case room_user_mgr:request(UserId, {TokenData, {sitdown, SeatNum}}) of
		{success, ServerSeatNum} ->
			?FILE_LOG_DEBUG("user_id ~p sitdown success, ServerSeatNum=~p", [UserId, ServerSeatNum]),
			send_packet(#qp_sitdown_rsp{result = 0, seat_num = ServerSeatNum}, State),
			ok;
		{failed, ErrorCode} ->
			?FILE_LOG_DEBUG("user_id ~p sitdown failed, code=~p", [UserId, ErrorCode]),
			send_packet(#qp_sitdown_rsp{result = ErrorCode}, State),
			ok;
		ignore ->
			?FILE_LOG_WARNING("user_id=~p, sitdown[~p] ignore", [UserId, SeatNum]),
			ok
	end,
	{login_success, State, true};
packet_handle(#qp_standup_req{seat_num = SeatNum}, login_success, #state{user_id = UserId, token_data = TokenData} = State) ->
	?FILE_LOG_DEBUG("user_id=~p standup seat_num=~p", [UserId, SeatNum]),
	case room_user_mgr:request(UserId, {TokenData, {standup, SeatNum}}) of
		success ->
			?FILE_LOG_DEBUG("user_id ~p standup success", [UserId]),
			send_packet(#qp_standup_rsp{state = 0}, State),
			ok;
		{failed, ErrorCode} ->
			?FILE_LOG_DEBUG("user_id ~p standup failed, code=~p", [UserId, ErrorCode]),
			send_packet(#qp_standup_rsp{state = ErrorCode}, State),
			ok;
		ignore ->
			?FILE_LOG_WARNING("user_id=~p, standup[~p] ignore", [UserId, SeatNum]),
			ok
	end,
	{login_success, State, true};
packet_handle(#qp_exit_room_req{seat_num = SeatNum}, login_success, #state{user_id = UserId, token_data = TokenData} = State) ->
	?FILE_LOG_DEBUG("user_id=~p exitroom seat_num=~p", [UserId, SeatNum]),
	case room_user_mgr:request(UserId, {TokenData, {exitroom, SeatNum}}) of
		success ->
			?FILE_LOG_DEBUG("user_id ~p exitroom success", [UserId]),
			send_packet(#qp_exit_room_rsp{result = 0}, State),
			ok;
		{failed, ErrorCode} ->
			?FILE_LOG_DEBUG("user_id ~p exitroom failed, code=~p", [UserId, ErrorCode]),
			send_packet(#qp_exit_room_rsp{result = ErrorCode}, State),
			ok;
		ignore ->
			?FILE_LOG_WARNING("user_id=~p, exitroom[~p] ignore", [UserId, SeatNum]),
			ok
	end,
	{login_success, State, true};
packet_handle(Event, login_success, #state{user_id = UserId}) ->
	?FILE_LOG_DEBUG("user_id[~p] unkown event ~p", [UserId, Event]),
	throw({custom, unknown_event});

packet_handle(Event, wait_exit, #state{user_id = UserId}) ->
	?FILE_LOG_DEBUG("user_id[~p] state[exit], unkown event ~p", [UserId, Event]),
	throw({custom, wait_exit}).

undefined_to_empty(undefined) -> [];
undefined_to_empty(L) -> L.


