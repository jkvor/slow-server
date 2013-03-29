-module(slow_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT, 2000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
init([]) ->
    Port = list_to_integer(os:getenv("PORT")),
    io:format("start web server on port ~p~n", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    {ok, LSock, 0}.

%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
handle_info(timeout, LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("[~w] accepted ~p~n", [time_to_ms(os:timestamp()), Sock]),
    inet:setopts(Sock,[{active,once}]),
    {noreply, LSock};

handle_info({tcp,Sock,Data}, State) ->
    io:format("[~w] recv'd ~p~n", [time_to_ms(os:timestamp()), Data]),
    inet:setopts(Sock,[{active,once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp_closed,_Sock}, State) ->
    io:format("[~w] closed~n", [time_to_ms(os:timestamp())]),
    {noreply, State, 0};

handle_info({tcp_error,_Sock,Reason}, State) ->
    io:format("[~w] error: ~p~n", [time_to_ms(os:timestamp()), Reason]),
    {noreply, State, 0};

handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
time_to_ms(Time) ->
  time_to_microseconds(Time) div 1000.

time_to_microseconds(Time) ->
  timer:now_diff(Time,{0,0,0}).
