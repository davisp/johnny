% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.


-define(NIF_INIT(Module, Nif, Extra),
    PrivDir = case code:priv_dir(Module) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(Module)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, Nif), Extra)).


-define(NOT_LOADED,
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).
