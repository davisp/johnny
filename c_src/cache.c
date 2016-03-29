// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "johnny.h"


ERL_NIF_TERM
johnny_cache_alloc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    johnny_st_t* st = (johnny_st_t*) enif_priv_data(env);
    johnny_cache_t* res;
    ERL_NIF_TERM ret;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    res = enif_alloc_resource(st->res_cache, sizeof(johnny_cache_t));
    if(res == NULL) {
        return enif_make_badarg(env);
    }

    res->env = enif_alloc_env();
    if(res->env == NULL) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    res->key = enif_make_copy(res->env, argv[0]);
    res->val = enif_make_copy(res->env, argv[1]);
    res->opt = enif_make_copy(res->env, argv[2]);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    return ret;
}


static void
johnny_cache_free(ErlNifEnv* env, void* obj)
{
    johnny_cache_t* cache = (johnny_cache_t*) obj;
    if(cache->env != NULL) {
        enif_free_env(cache->env);
    }
}


ERL_NIF_TERM
johnny_cache_unwrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    johnny_st_t* st = (johnny_st_t*) enif_priv_data(env);
    johnny_cache_t* res;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM opt;
    void* obj;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], st->res_cache, &obj)) {
        return enif_make_badarg(env);
    }

    res = (johnny_cache_t*) obj;

    key = enif_make_copy(env, res->key);
    val = enif_make_copy(env, res->val);
    opt = enif_make_copy(env, res->opt);

    return enif_make_tuple3(env, key, val, opt);
}


ErlNifResourceType*
johnny_cache_init_resource(ErlNifEnv* env)
{
    return enif_open_resource_type(
            env,
            NULL,
            "johnny_cache",
            johnny_cache_free,
            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
            NULL
        );
}
