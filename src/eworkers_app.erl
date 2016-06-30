-module(eworkers_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(term(), term()) -> pid().
start(_StartType, _StartArgs) ->
    eworkers_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
