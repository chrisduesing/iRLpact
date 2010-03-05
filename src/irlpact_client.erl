%%%-------------------------------------------------------------------
%%% File    : irlpact_client.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : A sample client implementation
%%%
%%% Created :  4 Mar 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_client).

%% API
-export([start/0, run/0]).

%% records
-include_lib("irlpact.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

start() ->
    Pid = spawn(irlpact_client, run, []),
    irlpact_connector:register_listener(Pid),
    irlpact_connector:connect("63.247.113.163", 8000),
    irlpact_connector:login("ccx_pf","Starts123"),
    irlpact_connector:request_products_by_type([12,30,34,38,43]),
    irlpact_connector:subscribe_by_type([12,30,34,38,43]).



    

%%====================================================================
%% Internal functions
%%====================================================================

run() ->
    receive
	%{product_definition, #product_definition{market_id=MarketId}} ->
	    
	_ ->
	    ok
    end,
    run().
