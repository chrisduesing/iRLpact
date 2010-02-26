%%%-------------------------------------------------------------------
%%% File    : irlpact_message.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : message parsing and building logic
%%%
%%% Created : 26 Feb 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_message).

%% API
-export([parse/1, build_login_message/3]).

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
    PaddedUsername = pad_right(" ", 30, Username),
    PaddedPassword = pad_right(" ", 30, Password),
    io:fwrite("after padding~n", []),
    UsernameField = list_to_binary(PaddedUsername), 
    PasswordField = list_to_binary(PaddedPassword),
    Version = list_to_binary(pad_left(" ", 12, "1.1.12")),
    io:fwrite("after list_to_binary~n", []),
    MessageBody = << Sequence:32, UsernameField/binary, PasswordField/binary, "Y", "Y", Version/binary, "N", "N", "N", "N" >>,
    io:fwrite("after message body~n", []),
    Length = size(MessageBody),
    <<"1", Length:16, MessageBody/binary>>.

%%====================================================================
%% Internal functions
%%====================================================================
scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) == Length ->
    io:fwrite("scan: single message~n", []),
    Message = parse_message(Type, Length, MessageBody),
    [{message, Message}] ++ Messages;

scan(<<Type:1/binary, Length:16, MessageBody/binary>>, Messages) when size(MessageBody) < Length ->
    io:fwrite("scan: partial message~n", []),
    PartialMessage = <<Type:1/binary, Length:16, MessageBody/binary>>,
    [{partial_message, PartialMessage}] ++ Messages;

scan(<<Type:1/binary, Length:16, Rest/binary>>, Messages) when size(Rest) > Length ->
    io:fwrite("scan: message with more~n", []),
    <<MessageBody:Length/binary, Next/binary>> = Rest,
    Message = parse_message(Type, Length, MessageBody),
    scan(Next, [{message, Message}] ++ Messages);

scan(BadFormatMessage, Messages) ->
    io:fwrite("scan received bad message~n"),
    [{partial_message, BadFormatMessage}] ++ Messages.


parse_message(<<"Q">>, 8, DateTime) ->
    {heartbeat, DateTime};

parse_message(<<"A">>, 425, MessageBody) ->
    <<Sequence:32, Code:1/binary, Message:120/binary, Markets:300/binary>> = MessageBody,
    {login_response, Sequence, Code, Message, Markets};

parse_message(Type, Length, MessageBody) ->
    io:fwrite("Cannot parse message of type: ~s, length: ~p, message: ~p~n", [Type, Length, MessageBody]),
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
