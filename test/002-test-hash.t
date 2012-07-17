#! /usr/bin/env escript
% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(10),

    {ok, C} = johnny:hash(),
    etap:is(johnny:get(C, <<"foo">>), {error, not_found}, "Get missing is ok"),
    etap:is(johnny:del(C, <<"foo">>), {error, not_found}, "Del missing is ok"),
    etap:is(johnny:put(C, <<"foo">>, bar), ok, "Stored a key"),
    etap:is(johnny:get(C, <<"foo">>), {ok, bar}, "Retrieved a key"),
    etap:is(johnny:put(C, <<"bar">>, foo), ok, "Stored a key"),
    etap:is(johnny:size(C), {ok, 2}, "Correct size for hash"),
    etap:is(johnny:del(C, <<"foo">>), ok, "Deleted a key"),
    etap:is(johnny:size(C), {ok, 1}, "Correct size after delete"),
    etap:is(johnny:clear(C), ok, "Cleared the hash"),
    etap:is(johnny:size(C), {ok, 0}, "Correct size after clear"),

    etap:end_tests().


