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
-export([start_link/0, connect/2, login/2, register_listener/1, request_products_by_type/1, subscribe_by_type/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
-record(state, {socket, listener, sequence=0, leftover=nill}).

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

%%--------------------------------------------------------------------
%% Function: register_listener(Pid, MessageTypes) -> ok | {error,Error}
%%           MessageTypes = heartbeat | login_response | ...
%% Description: register a client to be notified of message types
%%--------------------------------------------------------------------
register_listener(Pid) ->
    gen_server:cast(?MODULE, {register, Pid}).

%%--------------------------------------------------------------------
%% Function: connect(IP, Port) -> {ok,Pid} | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
connect(IP, Port) ->
    gen_server:call(?MODULE, {connect, IP, Port}).

%%--------------------------------------------------------------------
%% Function: login -> ok | {error,Error}
%% Description: authenticate the user
%%--------------------------------------------------------------------
login(Username, Password) ->
    gen_server:call(?MODULE, {login, Username, Password}).

%%--------------------------------------------------------------------
%% Function: request_products_by_type() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
request_products_by_type(TypeList) when is_list(TypeList) ->
    gen_server:cast(?MODULE, {request_products_by_type, TypeList}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
subscribe_by_type(TypeList) when is_list(TypeList) ->
    gen_server:cast(?MODULE, {subscribe_by_type, TypeList}).


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
    case gen_tcp:connect(IP, Port, [binary, {active, once}]) of
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
    Sequence = State#state.sequence + 1,
    NewState = State#state{sequence=Sequence},
    LoginMessage = irlpact_message:build_login_message(Username, Password, Sequence),
    io:fwrite("login message: ~s~n", [LoginMessage]),
    case gen_tcp:send(Socket, LoginMessage) of
	ok ->
	    {reply, ok, NewState};
	{error, Reason} ->
	    {reply, {error, Reason}, NewState}
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
handle_cast({register, Pid}, State) ->
    NewState = State#state{listener=Pid},
    {noreply, NewState};

handle_cast({request_products_by_type, TypeList}, State) ->
    {ok, NewState} = request_products(TypeList, State),
    {noreply, NewState};

handle_cast({subscribe_by_type, TypeList}, State) ->
    {ok, NewState} = request_market_data(TypeList, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data}, #state{leftover=Prepend} = State) when Prepend /= nill ->
    %io:fwrite("prepend data: ", []),
    {ok, NewState} = handle_streaming_data(<<Prepend/binary, Data/binary>>, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};

handle_info({tcp, Socket, Data}, State) ->
    %io:fwrite("irlpact_connector:handle_info received ~s~n", [Data]),
    {ok, NewState} = handle_streaming_data(Data, State),
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    io:fwrite("irlpact_connector:handle_info tcp_close~n", []),
    {noreply, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
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

handle_streaming_data(Data, State) ->
    Messages = irlpact_message:parse(Data),
    %io:fwrite("irlpact_connector:handle_streaming_data parsed messages: ~w~n", [Messages]),
    case notify(State#state.listener, Messages) of
	{partial_message, Leftover} ->
	    %io:fwrite(" adding left over data to state~n", []),
	    NewState = State#state{leftover=Leftover},
	    {ok, NewState};
	_ ->
	    NewState = State#state{leftover=nill},
	    {ok, NewState}
    end.


notify(Listener, [{message, Message} | Messages]) ->
    if is_pid(Listener) ->
	    Listener ! Message;
       true ->
	    ok
    end,
    notify(Listener, Messages);

notify(_Listener, [{partial_message, Message}]) ->
    {partial_message, Message};

% empty list, and/or no listener
notify(_Listener, _) ->
    ok.

request_products([MarketTypeId | T], #state{socket=Socket, sequence=Sequence} = State) ->
    NextSequence = Sequence + 1,
    Message = irlpact_message:build_product_definition_request(MarketTypeId, NextSequence),
    io:fwrite("sending product definition request: ~p~n", [Message]),
    gen_tcp:send(Socket, Message),
    NewState = State#state{sequence=NextSequence},
    request_products(T, NewState);

request_products([], State) ->
    {ok, State}.

request_market_data([MarketTypeId | T], #state{socket=Socket, sequence=Sequence} = State) ->
    NextSequence = Sequence + 1,
    Message = irlpact_message:build_market_data_request(MarketTypeId, NextSequence),
    io:fwrite("sending market subscription request ~p~n", [MarketTypeId]),
    gen_tcp:send(Socket, Message),
    NewState = State#state{sequence=NextSequence},
    request_market_data(T, NewState);

request_market_data([], State) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Unit tests
%%--------------------------------------------------------------------

%connect_test() ->
