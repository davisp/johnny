-module(johnny_hash_tests).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
    prop_hash_any/0,
    prop_hash_kvs/0
]).

-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3,
    random_key/1
]).


-record(st, {h, d}).

proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 10},
        {numtests, 500}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_hash_any() ->
    ?FORALL({K, V}, kvs(), begin
        {ok, C} = johnny:hash(),
        case (catch johnny:put(C, K, V)) of
            ok -> johnny:get(C, K) == {ok, V};
            _ -> false
        end
    end).


prop_hash_kvs() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, S, R} = run_commands(?MODULE, Cmds),
            cleanup(S),
            Fmt = "History: ~p~nState: ~p~nRes: ~p~nCommands: ~p~n",
            ?WHENFAIL(
                io:format(standard_error, Fmt, [H,S,R,Cmds]),
                R =:= ok
            )
        end
    ).

initial_state() ->
    {ok, H} = johnny:hash(),
    #st{h=H, d=dict:new()}.


command(S) ->
    Key = {call, johnny_hash_tests, random_key, [S]},
    frequency([
        {1, {call, johnny, clear, [S#st.h]}},
        {9, {call, johnny, get, [S#st.h, Key]}},
        {2, {call, johnny, get, [S#st.h, key()]}},
        {9, {call, johnny, put, [S#st.h, Key, val()]}},
        {2, {call, johnny, put, [S#st.h, key(), val()]}},
        {2, {call, johnny, del, [S#st.h, Key]}},
        {2, {call, johnny, del, [S#st.h, key()]}},
        {1, {call, johnny, keys, [S#st.h]}},
        {1, {call, johnny, size, [S#st.h]}}
    ]).


precondition(_, _) ->
    true.


postcondition(S, {call, _, clear, [_C]}, ok) ->
    true;
postcondition(S, {call, _, get, [_C, Key]}, {ok, Val}) ->
    {ok, Val} == dict:find(Key, S#st.d);
postcondition(S, {call, _, get, [_C, Key]}, {error, not_found}) ->
    dict:is_key(Key, S#st.d) == false;
postcondition(_S, {call, _, put, [_C, _Key, _Val]}, ok) ->
    true;
postcondition(_S, {call, _, del, [_C, _Key]}, ok) ->
    true;
postcondition(_S, {call, _, del, [_C, _Key]}, {error, not_found}) ->
    true;
postcondition(S, {call, _, keys, [_C]}, {ok, HKeys}) ->
    DKeys = dict:fetch_keys(S#st.d),
    lists:sort(HKeys) == lists:sort(DKeys);
postcondition(S, {call, _, size, [_C]}, {ok, Size}) ->
    case dict:size(S#st.d) of
        Size -> true;
        _ -> false
    end;
postcondition(_S, _A, _R) ->
    false.


next_state(S, _V, {call, _, clear, [_C]}) ->
    D = {call, dict, new, []},
    S#st{d=D};
next_state(S, _V, {call, _, get, [_C, _Key]}) ->
    S;
next_state(S, _V, {call, _, put, [_C, Key, Val]}) ->
    D = {call, dict, store, [Key, Val, S#st.d]},
    S#st{d=D};
next_state(S, _V, {call, _, del, [_C, Key]}) ->
    D = {call, dict, erase, [Key, S#st.d]},
    S#st{d=D};
next_state(S, _V, {call, _, keys, [_C]}) ->
    S;
next_state(S, _V, {call, _, size, [_C]}) ->
    S.


random_key(#st{d=D}) ->
    Keys0 = dict:fetch_keys(D),
    Keys = lists:append(Keys0, [foo]),
    NumKeys = erlang:length(Keys),
    KeyPos = random:uniform(NumKeys),
    lists:nth(KeyPos, Keys).


cleanup(_S) ->
    ok.


% Generators

key() -> any().
val() -> any().
kvs() -> {any(), any()}.
