%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 一月 2018 17:07
%%%-------------------------------------------------------------------
-module(from).
-author("yaohong").

%% API
-export([
	new/1,
	reply/2
]).


new(From) ->
	{?MODULE, [From]}.



reply(Value, {?MODULE, [From]}) ->
	gen_server:reply(From, Value).