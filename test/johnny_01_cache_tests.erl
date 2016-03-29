% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(johnny_01_cache_tests).


-include_lib("eunit/include/eunit.hrl").


basic_test() ->
    ?assertMatch({johnny_cache, _}, johnny:cache(foo)).


key_test() ->
    C = johnny:cache(foo),
    ?assertEqual(foo, johnny:key(C)),
    ?assertEqual({foo, undefined, undefined}, johnny:unwrap(C)).


val_test() ->
    C = johnny:cache(foo, bar),
    ?assertEqual(bar, johnny:val(C)),
    ?assertEqual({foo, bar, undefined}, johnny:unwrap(C)).


opt_test() ->
    C = johnny:cache(foo, bar, baz),
    ?assertEqual(baz, johnny:opt(C)),
    ?assertEqual({foo, bar, baz}, johnny:unwrap(C)).


tuple1_test() ->
    C = johnny:cache_tuple({foo}),
    ?assertEqual({foo, undefined, undefined}, johnny:unwrap(C)).


tuple2_test() ->
    C = johnny:cache_tuple({foo, bar}),
    ?assertEqual({foo, bar, undefined}, johnny:unwrap(C)).


tuple3_test() ->
    C = johnny:cache_tuple({foo, bar, baz}),
    ?assertEqual({foo, bar, baz}, johnny:unwrap(C)).


silly_leak_test() ->
    {Pid, Ref} = erlang:spawn_monitor(fun() ->
        {memory, Before} = process_info(self(), memory),
        lists:foreach(fun(I) ->
            johnny:cache(I)
        end, lists:seq(1, 10000)),
        erlang:garbage_collect(),
        {memory, After} = process_info(self(), memory),
        exit(After - Before)
    end),
    receive
        {'DOWN', Ref, process, Pid, Diff} ->
            ?assert(Diff < 100)
    end.

