%%%-------------------------------------------------------------------
%%% File    : irlpact_message.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : message parsing and building logic
%%%
%%% Created : 26 Feb 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_message).

%% API
-export([parse/1, build_login_message/3, build_product_definition_request/2, build_market_data_request/2]).

%% records
-include_lib("irlpact.hrl").

%% tests
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: parse(Data) -> [Message]
%%           Message -> {message, KnownMessageType}
%%                      {partial_message, RawMessageData}
%%
%% Description: parse takes a binary chunk and breaks it up in to a list of messages
%%--------------------------------------------------------------------
parse(Data) ->
    scan(Data, []).

%%--------------------------------------------------------------------
%% Function: build_login_message(Username, Password, Sequence) -> <<Message>>
%%
%% Description: takes the username and password and global seq # and builds
%%              a message ready to be passed back to the server for login.
%%--------------------------------------------------------------------
build_login_message(Username, Password, Sequence) ->
    UsernameField = list_to_binary(pad_right(<<0>>, 30, Username)), 
    PasswordField = list_to_binary(pad_right(<<0>>, 30, Password)),
    GetMarketStatsUpdate = <<"Y">>,
    MktDataBuffering = <<"Y">>, 
    Version = list_to_binary(pad_right(<<0>>, 12, "1.1.19")),
    GetMessageBundleMarker = <<"N">>,
    GetImpliedOrders = <<"N">>,
    GetUnofficialSettlementPrices = <<"N">>,
    GetQvMessages = <<"N">>,
    GetMarketEventMessages = <<"Y">>,
    GetStripInfoMessages = <<"N">>,
    ReservedField1 = 0,
    MessageBody = << Sequence:32, 
		     UsernameField/binary, 
		     PasswordField/binary, 
		     GetMarketStatsUpdate/binary, 
		     MktDataBuffering/binary,
		     Version/binary, 
		     GetMessageBundleMarker/binary, 
		     GetImpliedOrders/binary, 
		     GetUnofficialSettlementPrices/binary, 
		     GetQvMessages/binary,
		     GetMarketEventMessages/binary,
		     GetStripInfoMessages/binary,
		     ReservedField1:16
		  >>,
    Length = size(MessageBody),
    %io:fwrite("login message length: ~w~n", [Length]),
    <<"1", Length:16, MessageBody/binary>>.


%%--------------------------------------------------------------------
%% Function: build_product_definition_request(MarketTypeId, Sequence) -> <<Message>>
%%
%% Description: creates a message to request product definitions (one message per market type)
%%              
%%--------------------------------------------------------------------
build_product_definition_request(MarketTypeId, Sequence) ->
    MessageBody = << Sequence:32, MarketTypeId:16 >>,
    Length = size(MessageBody),
    <<"2", Length:16, MessageBody/binary>>.


build_market_data_request(MarketTypeId, Sequence) ->
    MarketDepth = 0,
    GetOptionMessages = <<"N">>,
    MessageBody = << Sequence:32, MarketTypeId:16, MarketDepth:16, GetOptionMessages:1/binary >>,
    Length = size(MessageBody),
    <<"3", Length:16, MessageBody/binary>>.



%%====================================================================
%% Internal functions
%%====================================================================
scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) == Length ->
    %io:fwrite("scan: single message: ", []),
    Message = parse_message(Type, Length, MessageBody),
    Messages ++ [{message, Message}];

scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) < Length ->
    PartialMessage = <<Type:1/binary, Length:16, MessageBody/binary>>,
    %io:fwrite("scan: partial message of type: ~p received: ~p of length: ~p~n", [Type, size(MessageBody), Length]),
    Messages ++ [{partial_message, PartialMessage}];

scan(<<Type:1/binary, Length:16, Rest/binary>>, Messages) when size(Rest) > Length ->
    %io:fwrite("scan: message with more: ", []),
    <<MessageBody:Length/binary, Next/binary>> = Rest,
    Message = parse_message(Type, Length, MessageBody),
    scan(Next, Messages ++ [{message, Message}]);

scan(BadFormatMessage, Messages) ->
    io:fwrite("scan received bad message: "),
    Messages ++ [{partial_message, BadFormatMessage}].

%%--------------------------------------------------------------------
%% Function: parse_message(MessageTypeIdentifier, Length, MessageBody) -> {Identifier, Message}
%%           Identifier = atom for each message response type
%%
%% Description: parse takes a binary chunk and breaks it up in to a list of messages
%%--------------------------------------------------------------------
parse_message(<<"A">>, 425, MessageBody) ->
    <<RequestSeqId:32, Code:1/binary, Text:120/binary, MarketTypesPermissioned:300/binary>> = MessageBody,
    {login_response, RequestSeqId, Code, Text, MarketTypesPermissioned};

parse_message(<<"B">>, 521, MessageBody) -> 
    %io:fwrite("received product definition response~n", []),
   << _ReqSeqId:32, RequestMarketType:16, NumMarkets:16, MarketId:32, ContractSymbol:35/binary, TradingStatus:1/binary, OrderPriceDenominator:1/binary, IncrementPrice:32, IncrementQty:32,
       LotSize:32, MarketDesc:120/binary, MaturityYear:16, MaturityMonth:16, MaturityDay:16, IsSpread:1/binary, IsCrackSpread:1/binary, PrimaryMarketId:32, SecondaryMarketId:32, 
       IsOptions:1/binary, OptionType:1/binary, StrikePrice:64, SecondStrike:64, DealPriceDenominator:1/binary, MinQty:32, UnitQty:32, Currency:20/binary, MinStrikePrice:64, MaxStrikePrice:64,
       IncrementStrikePrice:32, NumDecimalsStrikePrice:1/binary, MinOptionsPrice:64, MaxOptionsPrice:64, IncrementOptionsPrice:32, NumDecimalsOptionsPrice:1/binary, TickValue:64,
       AllowOptions:1/binary, ClearedAlias:15/binary, AllowImplied:1/binary, OptionsExpirationYear:16, OptionsExpirationMonth:16, OptionsExpirationDay:16, MinPrice:64, MaxPrice:64,
       ProductId:16, ProductName:62/binary, HubId:16, HubAlias:80/binary, StripId:16, StripName:39/binary, _ReservedField1:1/binary >> = MessageBody,
    PD = #product_definition{ 
      market_type=RequestMarketType, num_markets=NumMarkets, market_id=MarketId, contract_symbol=ContractSymbol, trading_status=TradingStatus, order_price_denominator=OrderPriceDenominator, 
      increment_price=IncrementPrice, increment_qty=IncrementQty, lot_size=LotSize, market_desc=MarketDesc, maturity_year=MaturityYear, marturity_month=MaturityMonth, maturity_day=MaturityDay, 
      is_spread=IsSpread, is_crack_spread=IsCrackSpread, primary_market_id=PrimaryMarketId, secondary_market_id=SecondaryMarketId, is_options=IsOptions, option_type=OptionType, 
      strike_price=StrikePrice, second_strike=SecondStrike, deal_price_denominator=DealPriceDenominator, min_qty=MinQty, unit_qty=UnitQty, currency=Currency, min_strike_price=MinStrikePrice, 
      max_strike_price=MaxStrikePrice, increment_strike_price=IncrementStrikePrice, num_decimals_strike_price=NumDecimalsStrikePrice, min_options_price=MinOptionsPrice, 
      max_options_price=MaxOptionsPrice, increment_options_price=IncrementOptionsPrice, num_decimals_options_price=NumDecimalsOptionsPrice, tick_value=TickValue, allow_options=AllowOptions, 
      cleared_alias=ClearedAlias, allow_implied=AllowImplied, options_expiration_year=OptionsExpirationYear, options_expiration_month=OptionsExpirationMonth, 
      options_expiration_day=OptionsExpirationDay, min_price=MinPrice, max_price=MaxPrice, product_id=ProductId, product_name=ProductName, hub_id=HubId, hub_alias=HubAlias, strip_id=StripId, 
      strip_name=StripName 
     },
    {product_definition, PD};

parse_message(<<"i">>, 84, MessageBody) ->
    {strip_info, MessageBody};

parse_message(<<"C">>, 105, MessageBody) ->
    %io:fwrite("parse market snapshot~n", []),
    << _ReqSeqId:32, MarketType:16, MarketId:32, TradingStatus:1/binary, Volume:32, BlockVolume:32, EFSVolume:32, EFPVolume:32, OpenInterest:32, OpeningPrice:64, SettlementPrice:64,
       High:64, Low:64, VWAP:64, NumOfOrderEntries:32, LastTradePrice:64, LastTradeQuantity:32, LastTradeDateTime:64, SettlePriceDateTime:64, _ReservedField1:2/binary >> = MessageBody,
    MS = #market_snapshot{
      market_type = MarketType, market_id = MarketId, trading_status = TradingStatus, volume = Volume, block_volume = BlockVolume, efs_volume = EFSVolume, 
      efp_volume = EFPVolume, open_interest = OpenInterest, opening_price = OpeningPrice, settlement_price = SettlementPrice, high = High, low = Low, vwap = VWAP, 
      num_of_order_entries = NumOfOrderEntries, last_trade_price = LastTradePrice, last_trade_quantity = LastTradeQuantity, last_trade_date_time = LastTradeDateTime, 
      settle_price_date_time = SettlePriceDateTime
     },
    {market_snapshot, MS};

parse_message(<<"D">>, 43, MessageBody) ->
    io:fwrite("received market snapshot order~n", []),
    << _ReqSeqId:32, MarketType:16, MarketId:32, OrderId:64, OrderSequenceId:16, Side:1/binary, Price:64, Quantity:32, IsImplied:1/binary, IsRFQ:1/binary, OrderEntryDateTime:64 >> = MessageBody,
    O = #order{
      market_type=MarketType, market_id=MarketId, order_id=OrderId, order_sequence_id=OrderSequenceId, side=Side, price=Price, quantity=Quantity, is_implied=IsImplied, is_rfq=IsRFQ, 
      order_entry_date_time=OrderEntryDateTime, is_snapshot=true
     },
    {market_snapshot_order, O};

parse_message(<<"E">>, 45, MessageBody) ->
    io:fwrite("received add/modify order~n", []),
    << MarketId:32, OrderId:64, OrderSequenceId:16, Side:1/binary, Price:64, Quantity:32, IsImplied:1/binary, IsRFQ:1/binary, OrderEntryDateTime:64, SentTime:64 >> = MessageBody,
    O = #order{
      market_id=MarketId, order_id=OrderId, order_sequence_id=OrderSequenceId, side=Side, price=Price, quantity=Quantity, is_implied=IsImplied, is_rfq=IsRFQ, 
      order_entry_date_time=OrderEntryDateTime, sent_time=SentTime, is_snapshot=false
     },
    {add_modify_order, O};

parse_message(<<"F">>, 21, MessageBody) ->
    io:fwrite("received add/modify order~n", []),
    << MarketId:32, OrderId:64, SentTime:64, SecurityType:1/binary >> = MessageBody,   
    D = #deleted_order{ market_id=MarketId, order_id=OrderId, sent_time=SentTime, security_type=SecurityType },
    {delete_order, D};

parse_message(<<"G">>, 43, MessageBody) ->
    io:fwrite("received trade~n", []),
    << MarketId:32, OrderId:64, IsSystemPricedLeg:1/binary, Price:64, Quantity:16, BlockTradeType:1/binary, TransactDateTime:64, SentTime:64, SystemPricedLegType:1/binary >> = MessageBody,    
    T = #trade{
      market_id=MarketId, order_id=OrderId, is_system_priced_leg=IsSystemPricedLeg, price=Price, quantity=Quantity, block_trade_type=BlockTradeType, transact_date_time=TransactDateTime, 
      sent_time=SentTime, system_priced_leg_type=SystemPricedLegType
     },
    {trade, T};

parse_message(<<"H">>, 34, MessageBody) ->
    {investigated_trade, MessageBody};

parse_message(<<"I">>, 33, MessageBody) ->
    io:fwrite("received cancelled trade~n", []),
    << MarketId:32, OrderId:64, Price:64, Quantity:16, BlockTradeType:1/binary, DateTime:64 >> = MessageBody,   
    CT = #cancelled_trade{ market_id=MarketId, order_id=OrderId, price=Price, quantity=Quantity, block_trade_type=BlockTradeType, date_time=DateTime },
    {cancelled_trade, CT};

parse_message(<<"J">>, 52, MessageBody) ->
    %io:fwrite("parse market snapshot~n", []),
    << MarketId:32, Volume:32, BlockVolume:32, EFSVolume:32, EFPVolume:32, High:64, Low:64, VWAP:64, DateTime:64 >> = MessageBody,
    MS = #market_snapshot_update{ market_id=MarketId, volume=Volume, block_volume=BlockVolume, efs_volume=EFSVolume, efp_volume=EFPVolume, high=High, low=Low, vwap=VWAP, date_time=DateTime },
    {market_statistics, MS};

parse_message(<<"K">>, 13, MessageBody) ->
    {market_state_change, MessageBody};

parse_message(<<"L">>, 1008, MessageBody) ->
    {system_text, MessageBody};

parse_message(<<"M">>, 20, MessageBody) ->
    {open_interest, MessageBody};

parse_message(<<"N">>, 20, MessageBody) ->
    {open_price, MessageBody};

parse_message(<<"O">>, 29, MessageBody) ->
    {settlement_price, MessageBody};

parse_message(<<"U">>, 89, MessageBody) ->
    {market_snapshot_option_order, MessageBody};

parse_message(<<"V">>, 88, MessageBody) ->
    {add_modify_option_order, MessageBody};

parse_message(<<"W">>, 85, MessageBody) ->
    {option_trade, MessageBody};

parse_message(<<"X">>, 78, MessageBody) ->
    {investigated_option_trade, MessageBody};

parse_message(<<"Y">>, 77, MessageBody) ->
    {cancelled_option_trade, MessageBody};

parse_message(<<"Z">>, 96, MessageBody) ->
    {option_market_statistics, MessageBody};

parse_message(<<"f">>, 13, MessageBody) ->
    {market_event, MessageBody};

parse_message(<<"P">>, 64, MessageBody) ->
    {debug_response, MessageBody};

parse_message(<<"Q">>, 8, DateTime) ->
    {heartbeat, DateTime};

parse_message(<<"S">>, 105, MessageBody) ->
    {error_response, MessageBody};

parse_message(<<"T">>, 1, MessageBody) ->
    {message_bundle_marker, MessageBody};

parse_message(Type, Length, MessageBody) ->
    io:fwrite("Cannot parse message of type: ~s and length: ~p, with value: ~s~n", [Type, Length, MessageBody]),
    {error, unhandled_message}.



pad_left(Pad, Length, List) ->
	   make_list(Pad, Length - length(List)) ++ List.

pad_right(Pad, Length, List) ->
	   List ++ make_list(Pad, Length - length(List)).

make_list(Of, Length) ->
    make_list(Of, Length, []).

make_list(_Of, 0, List) ->
    List;

make_list(Of, Length, List) ->
    make_list(Of, Length - 1, [Of] ++ List).

%%--------------------------------------------------------------------
%%% Unit tests
%%--------------------------------------------------------------------
message_test() ->    
    pad_left("a", 10, "bcd").
