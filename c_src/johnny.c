// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "johnny.h"
#include "atoms.h"
#include "nif_hash.h"
#include "nif_info.h"

static int
load(ErlNifEnv* env, void** priv, ENTERM info)
{
    johnny_init_atoms(env);

    if(!johnny_nif_hash_init(env))
        return 1;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ENTERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ENTERM info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


static ErlNifFunc nif_funcs[] = {
    {"hash_new", 1, johnny_nif_hash_new},
    {"hash_clear", 1, johnny_nif_hash_clear},
    {"hash_get", 2, johnny_nif_hash_get},
    {"hash_get", 3, johnny_nif_hash_get},
    {"hash_put", 3, johnny_nif_hash_put},
    {"hash_del", 2, johnny_nif_hash_del},
    {"hash_size", 1, johnny_nif_hash_size},
    {"hash_keys", 1, johnny_nif_hash_keys},
    {"hash_to_list", 1, johnny_nif_hash_to_list},

    {"info", 0, johnny_nif_info},
    {"info", 1, johnny_nif_info}
};


ERL_NIF_INIT(johnny, nif_funcs, &load, &reload, &upgrade, &unload);
