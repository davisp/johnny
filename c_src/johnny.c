// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.


#include "erl_nif.h"


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}


static ErlNifFunc nif_funcs[] = {
};


ERL_NIF_INIT(johnny, nif_funcs, &load, NULL, NULL, NULL);
