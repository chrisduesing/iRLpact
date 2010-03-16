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

-record(state, {order_books, trades, product_definitions, market_snapshots}).
-record(order_book, {buy_book=[], sell_book=[]}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

start() ->
    %% initialize internal state
    OrderBooks = dict:new(),
    Trades = dict:new(),
    ProductDefinitions = dict:new(),
    MarketSnapshots = dict:new(),
    State = #state{order_books=OrderBooks, trades=Trades, product_definitions=ProductDefinitions, market_snapshots=MarketSnapshots},

    %% start connection
    Pid = spawn(irlpact_client, run, [State]),
    irlpact_connector:register_listener(Pid),
    
    %% api test environment
    irlpact_connector:connect("63.247.113.163", 8000),
    irlpact_connector:login("ccx_pf","Starts123"),

    %% perf test environment
    %%irlpact_connector:connect("63.247.113.214", 8000),
    %%irlpact_connector:login("ccx_ps","Starts123"),
    
    % request product definitions and market updates
    irlpact_connector:request_products_by_type([12,30,34,38,43]),
    irlpact_connector:subscribe_by_type([12,30,34,38,43]).





%%====================================================================
%% Internal functions
%%====================================================================

run(#state{order_books=OrderBooks, trades=Trades, product_definitions=ProductDefinitions, market_snapshots=MarketSnapshots} = State) ->

    receive
	{product_definition, #product_definition{market_id=MarketId} = ProductDefinition} ->
	    io:fwrite("received product definition response for market id: ~p~n", [MarketId]),
	    NewProductDefinitions = dict:store(MarketId, ProductDefinition, ProductDefinitions),
	    %% initialize empty data structures to hold orders/trades for this market
	    NewOrderBooks = dict:store(MarketId, #order_book{}, OrderBooks),
	    NewTrades = dict:store(MarketId, [], Trades),
	    NewState = State#state{order_books=NewOrderBooks, trades=NewTrades, product_definitions=NewProductDefinitions},
	    run(NewState);

	{market_snapshot, #market_snapshot{market_id=MarketId } = MarketSnapshot} ->
	    io:fwrite("received market snapshot for market id: ~p~n", [MarketId]),
	    NewMarketSnapshots = dict:store(MarketId, MarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);

	{market_statistics, #market_snapshot_update{market_id=MarketId } = MarketSnapshotUpdate} ->
	    io:fwrite("received market snapshot for market id: ~p~n", [MarketId]),
	    MarketSnapshot = dict:fetch(MarketId, MarketSnapshots),
	    NewMarketSnapshot = irlpact_util:apply_update(MarketSnapshot, MarketSnapshotUpdate),
	    NewMarketSnapshots = dict:store(MarketId, NewMarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);

	{open_interest, #open_interest{market_id=MarketId} = OpenInterest} ->
	    io:fwrite("received open interest for market id: ~p~n", [MarketId]),
	    MarketSnapshot = dict:fetch(MarketId, MarketSnapshots),
	    NewMarketSnapshot = irlpact_util:apply_update(MarketSnapshot, OpenInterest),
	    NewMarketSnapshots = dict:store(MarketId, NewMarketSnapshot, MarketSnapshots),
	    NewState = State#state{market_snapshots=NewMarketSnapshots},
	    run(NewState);

	{market_snapshot_order, #order{market_id=MarketId} = Order} ->
	    io:fwrite("received a market snapshot order for market: ~p~n", [MarketId]),
	    {ok, NewOrderBooks} = add_order(MarketId, Order, OrderBooks),
	    NewState = State#state{order_books=NewOrderBooks},
	    run(NewState);

	{add_modify_order, #order{market_id=MarketId, order_id=OrderId} = Order} ->
	    io:fwrite("received an order with id ~p for market: ~p~n", [OrderId, MarketId]),
	    {ok, NewOrderBooks} = add_order(MarketId, Order, OrderBooks),
	    NewState = State#state{order_books=NewOrderBooks},
	    run(NewState);

	{delete_order, #deleted_order{market_id=MarketId, order_id=OrderId} = _DeletedOrder} ->
	    io:fwrite("received deleted order with id ~p for market: ~p~n", [OrderId, MarketId]),
	    {ok, NewOrderBooks} = remove_order(MarketId, OrderId, OrderBooks),
	    NewState = State#state{order_books=NewOrderBooks},
	    run(NewState);

	{trade, #trade{market_id=MarketId, order_id=OrderId} = Trade} ->
	    io:fwrite("received trade for order id ~p in market: ~p~n", [OrderId, MarketId]),
	    {ok, NewOrderBooks} = remove_order(MarketId, OrderId, OrderBooks),
	    TradesForMarket = dict:fetch(MarketId, Trades),
	    NewTradesForMarket = [Trade | TradesForMarket],
	    NewTrades = dict:store(MarketId, NewTradesForMarket, Trades),
	    NewState = State#state{trades=NewTrades, order_books=NewOrderBooks},
	    run(NewState);

	{cancelled_trade, #cancelled_trade{market_id=MarketId, order_id=OrderId} = _CancelledTrade} ->
	    io:fwrite("received cancelled trade with order id ~p for market: ~p~n", [OrderId, MarketId]),
	    TradesForMarket = dict:fetch(MarketId, Trades),
	    case lists:keyfind(OrderId, 3, TradesForMarket) of
		false ->
		    io:fwrite("could not find trade with order id ~p in market ~p for cancellation", [OrderId, MarketId]),
		    run(State);
		Trade ->
		    NewTradesForMarket = lists:delete(Trade, TradesForMarket),
		    NewTrades = dict:store(MarketId, NewTradesForMarket, Trades),
		    NewState = State#state{trades=NewTrades},
		    run(NewState)
	    end;

	_ ->
	    run(State)
    end.


add_order(MarketId, Order, OrderBooks) ->
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
			<<"1">> ->
			    NewBuyBook = [Order | BuyBook],
			    SortedBuyBook = lists:sort(F, NewBuyBook),
			    NewOrderBook = OrderBook#order_book{buy_book=SortedBuyBook},
			    dict:store(MarketId, NewOrderBook, OrderBooks);
			<<"2">> ->   	    
			    NewSellBook = [Order | SellBook],
			    SortedSellBook = lists:sort(F, NewSellBook),
			    NewOrderBook = OrderBook#order_book{sell_book=SortedSellBook},
			    dict:store(MarketId, NewOrderBook, OrderBooks)
		    end,
    {ok, NewOrderBooks}.

remove_order(MarketId, OrderId, OrderBooks) ->
    OrderBook = dict:fetch(MarketId, OrderBooks),
    #order_book{buy_book=BuyBook, sell_book=SellBook} = OrderBook,
    %% delete order doesnt contain side, so we have to search both books and delete the order
    NewOrderBooks = case lists:keyfind(OrderId, 5, BuyBook) of
	false ->
	    case lists:keyfind(OrderId, 5, SellBook) of 
		false ->
		    io:fwrite("Did not find order id ~p in buy or sell book for market id ~p~n", [OrderId, MarketId]),
		    OrderBooks;
		SellOrder ->
		    NewSellBook = lists:delete(SellOrder, SellBook),
		    NewOrderBook = OrderBook#order_book{sell_book=NewSellBook},
		    dict:store(MarketId, NewOrderBook, OrderBooks)
	    end;
	BuyOrder ->
	    NewBuyBook = lists:delete(BuyOrder, BuyBook),
	    NewOrderBook = OrderBook#order_book{buy_book=NewBuyBook},
	    dict:store(MarketId, NewOrderBook, OrderBooks)
    end,
    {ok, NewOrderBooks}.

