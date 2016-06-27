-module(eworkers_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("config.hrl").

-compile(export_all).

 
all() -> basic().

basic() ->
    [simple, async].
    
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(?APP),
    Config.


end_per_group(_, Config) ->

    Config.

end_per_testcase(_, Config) ->
    Config.

simple(_) ->
    ok = eworkers:call({?MODULE, test, []}).

async(_) ->
    Msgs = [asd, qwe],
    [eworkers:cast({?MODULE, test_async, [Msg, self()]}) || Msg <- Msgs],
    ok = recv(Msgs).

recv([]) ->
    ok;
recv([Msg|Rest]) ->
    receive 
        Msg ->
            recv(Rest)
    after 100 ->
        error
    end.

test() ->
    ok.

test_async(Msg, Pid) ->
    Pid ! Msg.