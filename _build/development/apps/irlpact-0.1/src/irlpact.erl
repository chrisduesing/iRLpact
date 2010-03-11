%%%-------------------------------------------------------------------
%%% File    : irlpact.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : Shortcut module to be used for testing, allows starting of irlpact with erl -s irlpact.
%%%               For production use, should be included in a release as an app instead.
%%%
%%% Created : 23 Feb 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact).

%% API
-export([start/0, start_link/0, stop/0]).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
%%--------------------------------------------------------------------
start_link() ->
    ensure_started(crypto),
    irlpact_sup:start_link().

%%-------------------------------------------------------------------- 
%% @spec start() -> ok
%% @doc Start the application.
%%--------------------------------------------------------------------
start() ->
    ensure_started(crypto),
    application:start(irlpact).

%%--------------------------------------------------------------------
%% @spec stop() -> ok
%% @doc Stop the application.
%%--------------------------------------------------------------------
stop() ->
    Res = application:stop(irlpact),
    application:stop(crypto),
    Res.

%%====================================================================
%% Internal functions
%%====================================================================
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
