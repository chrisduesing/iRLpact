%%%-------------------------------------------------------------------
%%% File    : irlpact_client.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : A sample client implementation
%%%
%%% Created :  4 Mar 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_client).

%% API
-export([start/0, run/1]).

%% records
-include_lib("irlpact.hrl").

-record(state, {order_books, product_definitions, market_snapshots}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

start() ->
    % initialize internal state
    OrderBooks = dict:new(),

    ProductDefinitions = dict:new(),
    MarketSnapshots = dict:new(),
    State = #state{order_books=OrderBooks, product_definitions=ProductDefinitions, market_snapshots=MarketSnapshots},

    % start connection
    Pid = spawn(irlpact_client, run, [State]),
    irlpact_connector:register_listener(Pid),
    irlpact_connector:connect("63.247.113.163", 8000),
    irlpact_connector:login("ccx_pf","Starts123"),
    irlpact_connector:request_products_by_type([12,30,34,38,43]),
    irlpact_connector:subscribe_by_type([12,30,34,38,43]).



    

%%====================================================================
%% Internal functions
%%====================================================================

run(#state{order_books=OrderBooks, product_definitions=ProductDefinitions, market_snapshots=MarketSnapshots} = State) ->

    receive
	{product_definition, #product_definition{market_id=MarketId} = ProductDefinition} ->
	    io:fwrite("received product definition response for market id: ~p~n", [MarketId]),
	    NewProductDefinitions = dict:append(MarketId, ProductDefinition, ProductDefinitions),
	    NewState = State#state{product_definitions=NewProductDefinitions},
	    run(NewState);

	{market_snapshot, #market_snapshot{market_id=MarketId } = MarketSnapshot} ->
	    io:fwrite("received market snapshot for market id: ~p~n", [MarketId]),
	    NewMarketSnapshots = dict:append(MarketId, MarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);
	    
	{market_statistics, #market_snapshot_update{market_id=MarketId } = MarketSnapshotUpdate} ->
	    io:fwrite("received market snapshot for market id: ~p~n", [MarketId]),
	    MarketSnapshot = dict:fetch(MarketId, MarketSnapshots),
	    NewMarketSnapshot = irlpact_util:apply_update(MarketSnapshot, MarketSnapshotUpdate),
	    NewMarketSnapshots = dict:append(MarketId, NewMarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);
	    

	_ ->
	    ok
    end.
