%%%-------------------------------------------------------------------
%%% File    : irlpact_message.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : message parsing and building logic
%%%
%%% Created : 26 Feb 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_message).

%% API
-export([parse/1, build_login_message/3, build_product_definition_request/2]).

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





%%====================================================================
%% Internal functions
%%====================================================================
scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) == Length ->
    io:fwrite("scan: single message: ", []),
    Message = parse_message(Type, Length, MessageBody),
    Messages ++ [{message, Message}];

scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) < Length ->
    PartialMessage = <<Type:1/binary, Length:16, MessageBody/binary>>,
    io:fwrite("scan: partial message of type: ~p received: ~p of length: ~p~n", [Type, size(MessageBody), Length]),
    Messages ++ [{partial_message, PartialMessage}];

scan(<<Type:1/binary, Length:16, Rest/binary>>, Messages) when size(Rest) > Length ->
    io:fwrite("scan: message with more: ", []),
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

parse_message(<<"B">>, 521, MessageBody) when size(MessageBody) == 521 -> 
    io:fwrite("received product definition response~n", []),
    {product_definition_response, MessageBody};

parse_message(<<"i">>, 84, MessageBody) ->
    {strip_info, MessageBody};

parse_message(<<"C">>, 105, MessageBody) ->
    {market_snapshot, MessageBody};

parse_message(<<"D">>, 43, MessageBody) ->
    {market_snapshot_order, MessageBody};

parse_message(<<"E">>, 45, MessageBody) ->
    {add_modify_order, MessageBody};

parse_message(<<"F">>, 21, MessageBody) ->
    {delete_order, MessageBody};

parse_message(<<"G">>, 43, MessageBody) ->
    {trade, MessageBody};

parse_message(<<"H">>, 34, MessageBody) ->
    {investigated_trade, MessageBody};

parse_message(<<"I">>, 33, MessageBody) ->
    {cancelled_trade, MessageBody};

parse_message(<<"J">>, 52, MessageBody) ->
    {market_statistics, MessageBody};

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

parse_message(Type, Length, _MessageBody) ->
    io:fwrite("Cannot parse message of type: ~s, length: ~p~n", [Type, Length]),
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
