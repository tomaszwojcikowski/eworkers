-module(eworkers_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([get_worker/0]).
-export([get_worker/1]).
-export([add_pool/1]).
-export([add_pool/2]).
-export([get_pool/0]).

-define(POOL_SIZE, 32).
-define(POOL_NAME, eworker_pool).

-include("config.hrl").

get_pool() ->
    ?POOL_NAME.

get_pool_size() ->
    case application:get_env(?APP, pool_size) of
        undefined ->
            ?POOL_SIZE;
        {ok, C} -> 
            C
    end.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1000, 3600}, [pool_spec(get_pool(), get_pool_size())]}}.


get_worker() ->
    get_worker(?POOL_NAME).
    
get_worker(PoolName) ->
    cuesport:get_worker(PoolName).


-spec pool_spec(atom(), non_neg_integer()) -> supervisor:child_spec().
pool_spec(PoolName, PoolSize) ->
    ChildMods = [eworkers],
    ChildMFA = {eworkers, start_link, []},
    {PoolName,
     {cuesport, start_link,
      [PoolName, PoolSize, ChildMods, ChildMFA]},
     transient, 2000, supervisor, [cuesport | ChildMods]}.

add_pool(PoolName) ->
    add_pool(PoolName, get_pool_size()).

add_pool(PoolName, PoolSize) ->
    {ok, _} = supervisor:start_child(?MODULE, pool_spec(PoolName, PoolSize)),
    ok.