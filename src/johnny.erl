% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(johnny).
-export([new/0, new/1, get/2, put/3, del/2]).
-define(NOT_LOADED, not_loaded(?LINE)).


-on_load(init/0).


new() ->
    new([]).


new(_Options) ->
    ?NOT_LOADED.


get(Cache, Key) when is_binary(Key) ->
    nif_get(Cache, Key);
get(Cache, Key) ->
    nif_get(Cache, term_to_binary(Key)).


put(Cache, Key, Val) when is_binary(Key) ->
    nif_put(Cache, Key, Val);
put(Cache, Key, Val) ->
    nif_put(Cache, term_to_binary(Key), Val).


del(Cache, Key) when is_binary(Key) ->
    nif_del(Cache, Key);
del(Cache, Key) ->
    nif_del(Cache, term_to_binary(Key)).


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


nif_get(Cache, Key) ->
    ?NOT_LOADED.


nif_put(Cache, Key, Val) ->
    ?NOT_LOADED.


nif_del(Cache, Key) ->
    ?NOT_LOADED.
