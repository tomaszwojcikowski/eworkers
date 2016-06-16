-module(eworkers).

-behaviour(gen_server).

-export([start_link/0]).
-export([add_pool/1]).
-export([add_pool/2]).
-export([call/1]).
-export([cast/1]).


%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type cmd() :: {module(), function(), list()} | {function(), list()}.
-type pool_name() :: atom().

-spec get_worker(pool_name()) -> pid().
get_worker(PoolName) ->
    eworkers_sup:get_worker(PoolName).

-spec call(cmd()) -> term().
call(Msg) ->
    call(eworkers_sup:get_pool(), Msg).
-spec call(pool_name(), cmd()) -> term().
call(PoolName, Msg) ->
    gen_server:call(get_worker(PoolName), {execute, Msg}). 

-spec cast(cmd()) -> ok.
cast(Msg) ->
    cast(eworkers_sup:get_pool(), Msg).
-spec cast(pool_name(), cmd()) -> ok.
cast(PoolName, Msg) ->
    gen_server:cast(get_worker(PoolName), {execute, Msg}). 


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call({execute, Cmd}, _From, State) ->
    Result = do_execute(Cmd),
    {reply, Result, State}.

handle_cast({execute, Cmd}, State) ->
    do_execute(Cmd),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
-spec do_execute(cmd()) -> term().
do_execute(Cmd) ->
    try
        tc_op(Cmd)
    catch _:Error ->
        error_loger:error_msg("Error during worker execution ~p, Reason: ~p. Stacktrace: ~p", [Cmd, Error, erlang:get_stacktrace()]),
        exometer:update([eworkers, errors], 1),
        {error, Error}
    end.

tc_op({Mod, Fun, Args}) ->
    {Time, Result} = timer:tc(Mod, Fun, Args),
    update_metrics(Mod, Fun, Time),
    Result;
tc_op({Fun, Args}) ->
    {Time, Result} = timer:tc(Fun, Args),
    update_metrics(global, anon, Time),
    Result.

-spec add_pool(pool_name()) -> ok.
add_pool(PoolName) ->
    eworkers_sup:add_pool(PoolName).
-spec add_pool(pool_name(), pos_integer()) -> ok.
add_pool(PoolName, PoolSize) when PoolSize > 0 ->
    eworkers_sup:add_pool(PoolName, PoolSize).


msg_queue_len() ->
    {message_queue_len, L} = process_info(self(), message_queue_len),
    L.

update_metrics(_Mod, _Fun, Time) ->
    exometer:update([eworkers, times], Time),
    exometer:update([eworkers, queues], msg_queue_len()),
    ok.