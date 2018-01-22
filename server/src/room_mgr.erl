%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 21:49
%%%-------------------------------------------------------------------
-module(room_mgr).
-author("yaohong").

-behaviour(gen_server).
-include("../deps/file_log/include/file_log.hrl").
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).
-export([
	create_room/2,
	get_room_pid/1
]).
-export([destroy_room/1]).
-define(SERVER, ?MODULE).

-record(state, {
	current_id_list,
	backup_id_list,
	room_logic_mod
}).

-record(room_data, {room_id, room_pid, cfg, timestamp}).
%%%===================================================================
%%% API
%%%===================================================================
create_room(OwnerBasicData, RoomCfg) ->
	gen_server:call(?MODULE, {create_room, {OwnerBasicData, RoomCfg}}).

get_room_pid(RoomId) ->
	gen_server:call(?MODULE, {get_room_pid, RoomId}).


destroy_room(RoomId) ->
	gen_server:cast(?MODULE, {destroy_room, RoomId}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	L = qp_util:random_list(lists:seq(100000, 999999)),
	ets:new(all_room, [set, protected, named_table, {keypos, #room_data.room_id}]),
	{success, RoomLogicMod} = qp_config:get_cfg(room_logic_mod),
	{ok, #state{current_id_list = L, backup_id_list= [], room_logic_mod = RoomLogicMod}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
	State :: #state{}) ->
	{reply, Reply :: term(), NewState :: #state{}} |
	{reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_call({create_room, {OwnerBasicData, RoomCfg}}, _From, #state{room_logic_mod = RoomLogicMod, current_id_list = [RoomId|LastCurrentIdList], backup_id_list = BackupIdList} = State) ->
	Reply =
		case catch RoomLogicMod:start(RoomId, OwnerBasicData, RoomCfg) of
			{ok, RoomPid} ->
				?FILE_LOG_DEBUG("create_room room_id[~p], timestamp=~p", [RoomId, qp_util:timestamp()]),
				ets:insert(
					all_room,
					#room_data{
						room_id = RoomId,
						room_pid = RoomPid,
						cfg = RoomCfg,
						timestamp = qp_util:timestamp()}),
				{success, {RoomId, RoomPid}};
			Other ->
				?FILE_LOG_ERROR("basic_data:~p, room_cfg:~p, ~p", [OwnerBasicData, RoomCfg, Other]),
				failed
		end,
	NewState =
		if
			LastCurrentIdList =:= [] -> State#state{current_id_list = BackupIdList, backup_id_list = []};
			true -> State#state{current_id_list = LastCurrentIdList}
		end,
	{reply, Reply, NewState};
handle_call({get_room_pid, RoomId}, _From, State) ->
	Reply =
		case ets:lookup(all_room, RoomId) of
			[] -> failed;
			[#room_data{room_pid = RoomPid}] ->
				{success, RoomPid}
		end,
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast({destroy_room, RoomId}, #state{backup_id_list = BackupIdList}= State) ->
	?FILE_LOG_DEBUG("destory_room room_id[~p], timestamp=~p", [RoomId, qp_util:timestamp()]),
	ets:delete(all_room, RoomId),
	{noreply, State#state{backup_id_list = [RoomId|BackupIdList]}};
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
	State :: #state{}) -> term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
	Extra :: term()) ->
	{ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
