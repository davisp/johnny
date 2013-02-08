% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

-module(gen_term).

-export([
    any/0,
    any/1,
    
    gen_atom/1,
    gen_integer/1,
    gen_float/1,
    gen_reference/1,
    gen_port/1,
    gen_pid/1,
    gen_tuple/1,
    gen_list/1,
    gen_short_string/1,
    gen_string/1,
    gen_binary/1,
    gen_bitstring/1,
    gen_bignum/1,
    gen_function/1
]).


any() ->
    any(16).


any(MaxSize) when MaxSize =< 0 ->
    Fun = choice(value_types()),
    ?MODULE:Fun(MaxSize);
any(MaxSize) ->
    Fun = choice(all_types()),
    ?MODULE:Fun(MaxSize).


gen_atom(MaxSize) ->
    list_to_atom(gen_short_string(MaxSize)).


gen_integer(_) ->
    Value = case random:uniform() < 0.5 of
        true -> random:uniform(127);
        false -> random:uniform(16#FFFFFFFF)
    end,
    case random:uniform() < 0.5 of
        true -> -1 * Value;
        false -> Value
    end.


gen_float(_) ->
    random:uniform() * float(16#FFFFFFFF).


gen_reference(_) ->
    erlang:make_ref().


gen_port(_) ->
    Ports = erlang:ports(),
    lists:nth(random:uniform(length(Ports)), Ports).


gen_pid(_) ->
    Pids = erlang:processes(),
    lists:nth(random:uniform(length(Pids)), Pids).


gen_tuple(MaxSize) ->
    list_to_tuple(gen_list(MaxSize)).


gen_list(MaxSize) ->
    Width = random:uniform(MaxSize),
    [any(MaxSize-Width) || _ <- lists:seq(1, Width)].


gen_short_string(_) ->
    Size = random:uniform(255),
    [random:uniform(127) || _ <- lists:seq(1, Size)].


gen_string(_) ->
    Size = random:uniform(4096),
    [random:uniform(127) || _ <- lists:seq(1, Size)].


gen_binary(MaxSize) ->
    list_to_binary(gen_string(MaxSize)).


gen_bitstring(MaxSize) ->
    B = gen_binary(MaxSize),
    <<2:4/integer, B/binary>>.


gen_bignum(_) ->
    16#FFFFFFFFFFFFFFFF + random:uniform(16#FFFFFFFF).


gen_function(_) ->
    choice(all_types()).


choice(Options) ->
    lists:nth(random:uniform(length(Options)), Options).


value_types() ->
    [
        gen_atom,
        gen_integer,
        gen_float,
        gen_reference,
        gen_port,
        gen_pid,
        gen_short_string,
        gen_string,
        gen_binary,
        gen_bitstring,
        gen_bignum,
        gen_function
    ].


all_types() ->
    value_types() ++ [gen_tuple, gen_list].

    