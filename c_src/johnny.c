// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.


#include "johnny.h"


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    johnny_st_t* st = (johnny_st_t*) enif_alloc(sizeof(johnny_st_t));

    if(st == NULL) {
        return 1;
    }

    johnny_init_atoms(env);

    st->res_cache = johnny_cache_init_resource(env);
    if(st->res_cache == NULL) {
        enif_free(st);
        return 1;
    }

    *priv = st;

    return 0;
}


static ErlNifFunc nif_funcs[] = {
    {"nif_cache_alloc", 3, johnny_cache_alloc},
    {"nif_cache_unwrap", 1, johnny_cache_unwrap}
};


ERL_NIF_INIT(johnny, nif_funcs, &load, NULL, NULL, NULL);
