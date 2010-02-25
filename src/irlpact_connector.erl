%%%-------------------------------------------------------------------
%%% File    : irlpact_connector.erl
%%% Author  : Chris Duesing <chris.duesing@gmail.com>
%%% Description : Establish connection to ICE's iMpact Data Feed
%%%
%%% Created : 22 Feb 2010 by Chris Duesing <chris.duesing@gmail.com>
%%%-------------------------------------------------------------------
-module(irlpact_connector).

-behaviour(gen_server).

%% API
-export([start_link/0, connect/2, login/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket}).

%% tests
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect(IP, Port) ->
    gen_server:call(?MODULE, {connect, IP, Port}).

login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, IP, Port}, _From, State) ->
    case gen_tcp:connect(IP, Port, [binary, {active, true}]) of
	{ok, Socket} ->
	    %gen_tcp:controlling_process(Socket, ?MODULE),
	    NewState = State#state{socket=Socket},
	    {reply, ok, NewState};
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;

handle_call({login, Username, Password}, _From, State) ->
    Socket = State#state.socket,
    io:fwrite("entering handle_call for login~n", []),
    Sequence = 1,
    io:fwrite("after sequence~n", []),
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
    LoginMessage = <<"1", Length:16, MessageBody/binary>>, 
    io:fwrite("login message: ~p~n", [LoginMessage]),
    case gen_tcp:send(Socket, LoginMessage) of
	ok ->
	   % case gen_tcp:recv(Socket, 3424) of
		%{ok, Packet} ->
		%    io:fwrite("Login response: ~p~n", [Packet]),
		    {reply, ok, State};
		%{error, Reason} ->
		%    io:fwrite("gen_tcp:recv failed in login: ~p~n", [Reason]),
		%    {reply, {error, Reason}, State}
	   % end;	   
	{error, Reason} ->
	    {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, State) ->
    io:fwrite("irlpact_connector:handle_info received ~s~n", [Data]),
    <<Type:8/binary, Length:16, Rest/binary>> = Data,
    <<MessageBody:(Length * 8)/binary, Next/binary>> = Rest,
    Message = parse_message(Type, Length, MessageBody),
    io:fwrite("irlpact_connector:handle_info received ~w~n", [Message]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    io:fwrite("irlpact_connector:handle_info tcp_close~n", []),
    {noreply, State};

handle_info({tcp_error, Socket, Reason}, State) ->
    io:fwrite("irlpact_connector:handle_info tcp_error : ~s~n", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

parse_message(<<"Q">>, 64, DateTime) ->
    {ok, heartbeat};

parse_message(<<"A">>, 3400, MessageBody) ->
    <<Sequence:32, Code:8/binary, Message:960/binary, Markets:2400>> = MessageBody,
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

connect_test() ->
    irlpact_connector:connect("63.247.113.163", 8000),
    irlpact_connector:login("ccx_ps","Starts123").
    
message_test() ->    
    pad_left("a", 10, "bcd").
