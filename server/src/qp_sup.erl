-module(qp_sup).
-author('yh@gmail.com').
-behaviour(supervisor).
-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.




init([]) ->
	{success, {DbAddr, DbPort, DbUser, DbPassword, DbName}} = qp_config:get_cfg(db_addr),
	{success, UserLogicMod} = qp_config:get_cfg(user_logic_mod),
	{success, RoomLogicMod} = qp_config:get_cfg(room_logic_mod),
	sql:start(DbAddr, DbPort, DbUser, DbPassword, DbName),

	TimerManager =
		{timer_manager,
		 {timer_manager, start_link, []},
		 permanent,
		 5000,
		 worker,
		 [timer_manager]},

	{success, {ListenIp, ListenPort}} = qp_config:get_cfg(listen_addr),

	QpReceiverSupervisor =
		{qp_receiver_sup,
		 {tmp_sup, start_link, [qp_receiver_sup, tcp_receiver]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [qp_receiver_sup]},
	QpUserSupervisor =
		{qp_user_sup,
		 {tmp_sup, start_link, [qp_user_sup, qp_user]},
		 transient,
		 brutal_kill,
		 supervisor,
		 [qp_user_sup]},


	RoomUserSupervisor =
		{room_user_sup,
			{tmp_sup, start_link, [room_user_sup, UserLogicMod]},
			transient,
			brutal_kill,
			supervisor,
			[room_user_sup]},

	RoomSupervisor =
		{room_sup,
			{tmp_sup, start_link, [room_sup, RoomLogicMod]},
			transient,
			brutal_kill,
			supervisor,
			[room_sup]},

	RoomUserMgr =
		{room_user_mgr,
			{room_user_mgr, start_link, []},
			permanent,
			5000,
			worker,
			[room_user_mgr]},

	RoomMgr =
		{room_mgr,
			{room_mgr, start_link, []},
			permanent,
			5000,
			worker,
			[room_mgr]},

	QpServer =
		{
			qp_server,
			{tcp_server, start_link, [qp_util:ipstr_to_v4(ListenIp), ListenPort, qp_server, qp_user, 4, qp_user_sup, qp_receiver_sup, 256 * 1024]},
			transient,
			brutal_kill,
			supervisor,
			[qp_server]
		},

    {ok,
    	{
    		{one_for_one, 10, 10},
			[
				TimerManager,
				QpReceiverSupervisor,
				QpUserSupervisor,
				RoomUserSupervisor,
				RoomSupervisor,
				RoomUserMgr,
				RoomMgr,
				QpServer
			]
    	}
    }.


%%web_specs(Mod, ListenIp, Port) ->
%%	WebConfig = [{ip, ListenIp},
%%		{port, Port},
%%		{nodelay, true},
%%		{docroot, http_deps:local_path(["priv", "www"])}],
%%	{Mod,
%%		{Mod, start, [WebConfig]},
%%		permanent, 5000, worker, dynamic}.
