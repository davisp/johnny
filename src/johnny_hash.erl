% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(johnny_hash).

-on_load(init/0).

-export([
    new/0, new/1, new/2,
    get/2, get/3,
    put/3,
    del/2,
    clear/1,
    size/1,
    keys/1,
    to_list/1
]).

-include("johnny.hrl").


init() ->
    ?NIF_INIT(?MODULE, "johnny_hash", 0).


new() ->
    new([], []).

new(KVs) ->
    new(KVs, []).

new(KVs, Opts) ->
    case new_nif(Opts) of
        {ok, Hash} ->
            lists:foreach(fun({K, V}) -> ?MODULE:put(Hash, K, V) end, KVs),
            {ok, Hash};
        Else ->
            Else
    end.


new_nif(_Options) ->
    ?NOT_LOADED.


get(_Hash, _Key) ->
    ?NOT_LOADED.


get(_Hash, _Key, _Val) ->
    ?NOT_LOADED.


put(_Hash, _Key, _Val) ->
    ?NOT_LOADED.


del(_Hash, _Key) ->
    ?NOT_LOADED.


clear(_Hash) ->
    ?NOT_LOADED.


size(_Hash) ->
    ?NOT_LOADED.


keys(_Hash) ->
    ?NOT_LOADED.


to_list(_Hash) ->
    ?NOT_LOADED.
