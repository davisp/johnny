% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(johnny).

-export([
    info/0,
    info/1,

    hash/0,
    hash/1,
    hash/2,

    clear/1,
    get/2,
    get/3,
    put/3,
    del/2,
    keys/1,
    to_list/1,
    size/1
]).


-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).


info() ->
    ?NOT_LOADED.


info(_Object) ->
    ?NOT_LOADED.


hash() ->
    hash([], []).

hash(KVs) ->
    hash(KVs, []).

hash(KVs, Opts) ->
    case hash_new(Opts) of
        {ok, Handle} ->
            lists:foreach(fun({K, V}) ->
                hash_put(Handle, term_to_binary(K), V)
            end, KVs),
            {ok, {johnny_hash, Handle}};
        Else ->
            Else
    end.


clear({johnny_hash, Handle}) ->
    hash_clear(Handle).


get({johnny_hash, Handle}, Key) ->
    hash_get(Handle, term_to_binary(Key)).


get({johnny_hash, Handle}, Key, Default) ->
    hash_get(Handle, term_to_binary(Key), Default).


put({johnny_hash, Handle}, Key, Value) ->
    hash_put(Handle, term_to_binary(Key), Value).


del({johnny_hash, Handle}, Key) ->
    hash_del(Handle, term_to_binary(Key)).


keys({johnny_hash, Handle}) ->
    case hash_keys(Handle) of
        {ok, List} ->
            {ok, lists:map(fun erlang:binary_to_term/1, List)};
        Else ->
            Else
    end.


to_list({johnny_hash, Handle}) ->
    case hash_to_list(Handle) of
        {ok, List} ->
            Conv = fun({K, V}) -> {binary_to_term(K), V} end,
            {ok, lists:map(Conv, List)};
        Else ->
            Else
    end.


size({johnny_hash, Handle}) ->
    hash_size(Handle).


init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "johnny"), 0).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).


% Hash NIF functions

hash_new(_Opts) ->
    ?NOT_LOADED.

hash_clear(_Hash) ->
    ?NOT_LOADED.

hash_get(_Hash, _Key) ->
    ?NOT_LOADED.

hash_get(_Hash, _Key, _Default) ->
    ?NOT_LOADED.

hash_put(_Hash, _Key, _Val) ->
    ?NOT_LOADED.

hash_del(_Hash, _Key) ->
    ?NOT_LOADED.

hash_keys(_Hash) ->
    ?NOT_LOADED.

hash_to_list(_Hash) ->
    ?NOT_LOADED.

hash_size(_Hash) ->
    ?NOT_LOADED.
