%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 一月 2018 13:23
%%%-------------------------------------------------------------------
-module(room_user_mgr).
-author("yaohong").
-include("../deps/file_log/include/file_log.hrl").
-include("qp_error.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).
-export([request/2]).
-define(SERVER, ?MODULE).

-record(state, {user_logic_mod}).
-record(game_user, {
	user_id,
	user_pid
}).


%%%===================================================================
%%% API
%%%===================================================================
request(UserId, Param) ->
	gen_server:call(?MODULE, {request, {UserId, Param}}).
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
	ets:new(all_game_user, [set, protected, named_table, {keypos, #game_user.user_id}]),
	{success, UserLogicMod} = qp_config:get_cfg(user_logic_mod),
	{ok, #state{user_logic_mod = UserLogicMod}}.

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
handle_call({request, {UserId, Param}}, From, #state{user_logic_mod = UserLogicMod} = State) ->
	case ets:lookup(all_game_user, UserId) of
		[] ->
			%%创建进程
			ets:insert(all_game_user, #game_user{user_id = UserId, user_pid = undefined}),
			spawn(
				fun() ->
					case catch UserLogicMod:start(UserId) of
						{ok, Pid} ->
							?MODULE ! {start_success, {UserId, Pid, Param, From}},
							ok;
						{error, ErrorCode} ->
							?FILE_LOG_ERROR("start new game_user failed, code=~p", [ErrorCode]),
							?MODULE ! {start_failed, {UserId, From}},
							ok;
						Other ->
							?FILE_LOG_ERROR("~p", [Other]),
							?MODULE ! {start_failed, {UserId, From}}
					end
				end),
			{noreply, State};
		[#game_user{user_pid = undefined}] ->
			%%正在加载中
			{reply, {failed, ?SYSTEM_TRY_AGAIN}, State};
		[#game_user{user_pid = UserPid}] when is_pid(UserPid) ->
			UserLogicMod:post(UserPid, from:new(From), Param),
			{noreply, State}
	end;
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
handle_info({start_success, {UserId, UserPid, Param, From}}, #state{user_logic_mod = UserLogicMod} = State) ->
	case ets:lookup(all_game_user, UserId) of
		[] ->
			?FILE_LOG_WARNING("create game_user failed, find_id_failed[~p]", [UserId]),
			gen_server:reply(From, {failed, ?SYSTEM_ERROR});
		[#game_user{user_pid = OldUserPid}] when is_pid(OldUserPid) ->
			?FILE_LOG_WARNING("create game_user failed, old_user_pid[~p]", [UserId]),
			gen_server:reply(From, {failed, ?SYSTEM_ERROR});
		[#game_user{user_pid = undefined}] ->
			?FILE_LOG_DEBUG("create game_user success pid=~p", [UserPid]),
			ets:update_element(all_game_user, UserId, [{#game_user.user_pid, UserPid}]),
			UserLogicMod:post(UserPid, from:new(From), Param)
	end,
	{noreply, State};
handle_info({start_failed, {UserId, From}}, State) ->
	case ets:lookup(all_game_user, UserId) of
		[] ->
			?FILE_LOG_DEBUG("cannot_delete user_id[~p] not exist", [UserId]),
			ok;
		[#game_user{user_pid = undefined}] ->
			ets:delete(all_game_user, UserId),
			ok;
		[#game_user{user_pid = OldUserPid}] when is_pid(OldUserPid) ->
			?FILE_LOG_DEBUG("cannot_delete old_pid=~p", [OldUserPid]),
			ok
	end,
	gen_server:reply(From, {failed, ?SYSTEM_ERROR}),
	{noreply, State};
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
