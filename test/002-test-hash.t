#! /usr/bin/env escript
% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

num_cycles() -> 10000.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(10),

    test_basic(),
    test_random(),

    etap:end_tests().


test_basic() ->
    {ok, C} = johnny_hash:new(),
    etap:is(
        johnny_hash:get(C, <<"foo">>), {error, not_found}, "Get missing is ok"),
    etap:is(
        johnny_hash:del(C, <<"foo">>), {error, not_found}, "Del missing is ok"),
    etap:is(johnny_hash:put(C, <<"foo">>, bar), ok, "Stored a key"),
    etap:is(johnny_hash:get(C, <<"foo">>), {ok, bar}, "Retrieved a key"),
    etap:is(johnny_hash:put(C, <<"bar">>, foo), ok, "Stored a key"),
    etap:is(johnny_hash:size(C), {ok, 2}, "Correct size for hash"),
    etap:is(johnny_hash:del(C, <<"foo">>), ok, "Deleted a key"),
    etap:is(johnny_hash:size(C), {ok, 1}, "Correct size after delete"),
    etap:is(johnny_hash:clear(C), ok, "Cleared the hash"),
    etap:is(johnny_hash:size(C), {ok, 0}, "Correct size after clear").


test_random() ->
    D = dict:new(),
    {ok, H} = johnny_hash:new(),

    Actions = [
        {0.1, fun(S) -> run_clear(S) end},
        {1.0, fun(S) -> run_get2(S) end},
        {1.0, fun(S) -> run_get3(S) end},
        {1.0, fun(S) -> run_put(S) end},
        {1.0, fun(S) -> run_del(S) end},
        {0.5, fun(S) -> run_size(S) end},
        {0.3, fun(S) -> run_keys(S) end},
        {0.3, fun(S) -> run_to_list(S) end}
    ],
    
    run(Actions, num_cycles(), {D, H}).


run(_, N, S) when N =< 0 ->
    ok;
run(Actions, N, S0) ->
    Action = weighted_choice(Actions),
    S1 = Action(S0),
    run(Actions, N-1, check_state(S1)).


run_clear({_D0, H}) ->
    ok = johnny_hash:clear(H),
    {dict:new(), H}.


run_get2({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, _} = Resp ->
            Resp = johnny_hash:get(H, K);
        error ->
            {error, not_found} = johnny_hash:get(H, K)
    end,
    {D, H}.


run_get3({D, H}) ->
    K = random_key(D),
    case dict:find(K, D) of
        {ok, _} = Resp ->
            Resp = johnny_hash:get(H, K);
        error ->
            Val = random_val(),
            {ok, Val} = johnny_hash:get(H, K, Val)
    end,
    {D, H}.


run_put({D0, H}) ->
    K = random_key(D0),
    V = random_val(),
    D1 = dict:store(K, V, D0),
    ok = johnny_hash:put(H, K, V),
    {D1, H}.


run_del({D0, H}) ->
    K = random_key(D0),
    D1 = case dict:is_key(K, D0) of
        true ->
            ok = johnny_hash:del(H, K),
            dict:erase(K, D0);
        false ->
            {error, not_found} = johnny_hash:del(H, K),
            D0
    end,
    {D1, H}.


run_size({D, H}) ->
    S = dict:size(D),
    {ok, S} = johnny_hash:size(H),
    {D, H}.


run_keys({D, H}) ->
    DKeys = lists:sort(dict:fetch_keys(D)),
    {ok, HKeys0} = johnny_hash:keys(H),
    HKeys = lists:sort(HKeys0),
    DKeys = HKeys,
    {D, H}.


run_to_list({D, H}) ->
    DKVs = lists:sort(dict:to_list(D)),
    {ok, HKVs0} = johnny_hash:to_list(H),
    HKVs = lists:sort(HKVs0),
    DKVs = HKVs,
    {D, H}.


check_state({D, H}) ->
    DKVs = lists:sort(dict:to_list(D)),
    {ok, HKVs0} = johnny_hash:to_list(H),
    HKVs = lists:sort(HKVs0),
    case DKVs == HKVs of
        true ->
            {D, H};
        false ->
            etap:bail("Bad hash state")
    end.


weighted_choice(Items0) ->
    Items = lists:sort(Items0),
    Sum = lists:sum([W || {W, _} <- Items]),
    Choice = random:uniform() * Sum,
    weighted_choice(Items, 0.0, Choice).


weighted_choice([], _, _) ->
    throw(bad_choice);
weighted_choice([{W, _} | Rest], S, C) when W + S < C ->
    weighted_choice(Rest, W+S, C);
weighted_choice([{_, I} | _], _, _) ->
    I.


random_key(D) ->
    Keys = lists:usort(dict:fetch_keys(D) ++ [foo]),
    lists:nth(random:uniform(length(Keys)), Keys).


random_val() ->
    gen_term:any(0).
