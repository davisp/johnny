-module(johnny_hash_tests).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


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
        {numtests, 1000}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_hash_any() ->
    ?FORALL({K, V}, kvs(), begin
        {ok, C} = johnny:new(),
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
            ?WHENFAIL(
                io:format("History: ~p\nState: ~p\nRes: ~p\n", [H,S,R]),
                R =:= ok
            )
        end
    ).

initial_state() ->
    {ok, H} = johnny:new(),
    #st{h=H, d=dict:new()}.


command(S) ->
    Key = {call, johnny_hash_tests, random_key, [S]},
    frequency([
        {9, {call, johnny, get, [S#st.h, Key]}},
        {1, {call, johnny, get, [S#st.h, key()]}},
        {9, {call, johnny, put, [S#st.h, Key, val()]}},
        {1, {call, johnny, put, [S#st.h, key(), val()]}},
        {2, {call, johnny, del, [S#st.h, Key]}},
        {1, {call, johnny, del, [S#st.h, key()]}},
        {1, {call, johnny, size, [S#st.h]}}
    ]).


precondition(_, _) ->
    true.


postcondition(S, {call, _, get, [_C, Key]}, Val) ->
    case dict:is_key(Key, S#st.d) of
        true ->
            case dict:find(Key, S#st.d) of
                Val -> true;
                _ -> false
            end;
        false ->
            case Val of
                {error, not_found} -> true;
                _ -> false
            end
    end;
postcondition(_S, {call, _, put, [_C, _Key, _Val]}, ok) ->
    true;
postcondition(_S, {call, _, del, [_C, _Key]}, {ok, _Val}) ->
    true;
postcondition(_S, {call, _, del, [_C, _Key]}, {error, not_found}) ->
    true;
postcondition(S, {call, _, size, [_C]}, {ok, Size}) ->
    case dict:size(S#st.d) of
        Size -> true;
        _ -> false
    end;
postcondition(_S, _A, _R) ->
    false.


next_state(S, _V, {call, _, get, [_C, _Key]}) ->
    S;
next_state(S, _V, {call, _, put, [_C, Key, Val]}) ->
    S#st{
        d={call, dict, store, [Key, Val, S#st.d]}
    };
next_state(S, _V, {call, _, del, [_C, Key]}) ->
    S#st{
        d={call, dict, erase, [Key, S#st.d]}
    };
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
