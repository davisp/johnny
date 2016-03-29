% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(johnny).
-on_load(init/0).


-export([
    cache/1,
    cache/2,
    cache/3,
    cache_tuple/1,
    unwrap/1,
    key/1,
    val/1,
    opt/1
]).


-define(NOT_LOADED,
        erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).


cache(Key) ->
    {johnny_cache, nif_cache_alloc(Key, undefined, undefined)}.


cache(Key, Value) ->
    {johnny_cache, nif_cache_alloc(Key, Value, undefined)}.


cache(Key, Value, Opt) ->
    {johnny_cache, nif_cache_alloc(Key, Value, Opt)}.


cache_tuple({K}) ->
    {johnny_cache, nif_cache_alloc(K, undefined, undefined)};

cache_tuple({K, V}) ->
    {johnny_cache, nif_cache_alloc(K, V, undefined)};

cache_tuple({K, V, O}) ->
    {johnny_cache, nif_cache_alloc(K, V, O)}.


unwrap({johnny_cache, Cache}) ->
    nif_cache_unwrap(Cache).


key(Cache) ->
    element(1, unwrap(Cache)).


val(Cache) ->
    element(2, unwrap(Cache)).


opt(Cache) ->
    element(3, unwrap(Cache)).


init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).


nif_cache_alloc(_K, _V, _O) ->
    ?NOT_LOADED.


nif_cache_unwrap(_Cache) ->
    ?NOT_LOADED.
