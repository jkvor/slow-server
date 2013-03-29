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
    io:format("accepted ~p~n", [Sock]),
    inet:setopts(Sock,[{active,once}]),
    {noreply, LSock};

handle_info({tcp,Sock,Data}, State) ->
    io:format("recv'd ~p~n", [Data]),
    inet:setopts(Sock,[{active,once}]),
    {noreply, State, ?TIMEOUT};

handle_info({tcp_closed,_Sock}, State) ->
    io:format("closed~n"),
    {noreply, State, 0};

handle_info({tcp_error,_Sock,Reason}, State) ->
    io:format("error: ~p~n", [Reason]),
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
