-module(eworkers_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("config.hrl").

-compile(export_all).

 
all() -> basic().

basic() ->
    [simple].
    
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(?APP),
    Config.


end_per_group(_, Config) ->

    Config.

end_per_testcase(_, Config) ->
    Config.

simple(_) ->
    ok = eworkers:call({?MODULE, test, []}).


test() ->
    ok.
