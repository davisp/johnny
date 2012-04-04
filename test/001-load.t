#! /usr/bin/env escript
% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    etap:plan(1),
    etap:loaded_ok(johnny, "Loaded johnny"),
    etap:end_tests().

