-module(slow_server_app).
-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(slow_server).

start(_StartType, _StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 1000, 3600}, [
    {slow_server, {slow_server, start_link, []}, permanent, 2000, worker, [slow_server]}
  ]}}.
