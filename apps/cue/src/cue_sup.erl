%%%-------------------------------------------------------------------
%% @doc cue top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cue_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  {ok, {
    #{strategy => one_for_one, intensity => 3, period => 30},
    [
      #{
        id => cue_handler_server,
        start => {cue_handler_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cue_handler_server]
      },
      
      #{
        id => cue_listener_server,
        start => {cue_listener_server, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [cue_listener_server]
      }
    ]
  }}.


%%====================================================================
%% Internal functions
%%====================================================================
