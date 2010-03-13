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
-record(order_book, {buy_book=[], sell_book=[]}).

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
    % api test environment
    irlpact_connector:connect("63.247.113.163", 8000),
    irlpact_connector:login("ccx_pf","Starts123"),
    % perf test environment
    %irlpact_connector:connect("63.247.113.214", 8000),
    %irlpact_connector:login("ccx_ps","Starts123"),
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
	    NewOrderBooks = dict:append(MarketId, #order_book{}, OrderBooks),
	    NewState = State#state{order_books=NewOrderBooks, product_definitions=NewProductDefinitions},
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
	    
	{open_interest, #open_interest{market_id=MarketId} = OpenInterest} ->
	    io:fwrite("received open interest for market id: ~p~n", [MarketId]),
	    MarketSnapshot = dict:fetch(MarketId, MarketSnapshots),
	    NewMarketSnapshot = irlpact_util:apply_update(MarketSnapshot, OpenInterest),
	    NewMarketSnapshots = dict:append(MarketId, NewMarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);
	    
	{order, #order{market_id=MarketId, order_id=OrderId} = Order} ->
	    io:fwrite("received an order with id: ~p for market: ~p~n", [OrderId, MarketId]),
	    OrderBook = dict:fetch(MarketId, OrderBooks),
	    #order_book{buy_book=BuyBook, sell_book=SellBook} = OrderBook,
	    F = fun(Order1, Order2) ->
			#order{price=Price1, sent_time=Time1} = Order1,
			#order{price=Price2, sent_time=Time2} = Order2,
			case Price1 /= Price2 of
			    true ->
				Price1 =< Price2;
			    false ->
				Time1 =< Time2
			end
		end,    
	    NewOrderBooks = case Order#order.side of
				<<1>> ->
				    NewBuyBook = [BuyBook | Order],
				    SortedBuyBook = lists:sort(F, NewBuyBook),
				    NewOrderBook = OrderBook#order_book{buy_book=SortedBuyBook},
				    dict:store(MarketId, NewOrderBook, OrderBooks);
				<<2>> ->   	    
				    NewSellBook = [SellBook | Order],
				    SortedSellBook = lists:sort(F, NewSellBook),
				    NewOrderBook = OrderBook#order_book{buy_book=SortedSellBook},
				    dict:store(MarketId, NewOrderBook, OrderBooks)
			     end,
	    NewState = State#state{order_books=NewOrderBooks},
	    run(NewState);
	    

	_ ->
	    run(State)
    end.
