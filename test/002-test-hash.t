#! /usr/bin/env escript
% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(3),

    {ok, C} = johnny:new(),
    etap:is(johnny:put(C, <<"foo">>, bar), ok, "Stored a key"),
    etap:is(johnny:get(C, <<"foo">>), {ok, bar}, "Retrieved a key"),
    etap:is(johnny:del(C, <<"foo">>), {ok, bar}, "Deleted a key"),

    etap:end_tests().


