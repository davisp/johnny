
#include "atoms.h"
#include "util.h"

ENTERM
johnny_make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

ENTERM
johnny_make_ok(ErlNifEnv* env, ENTERM data)
{
    return enif_make_tuple2(env, JOHNNY_ATOM_OK, data);
}

ENTERM
johnny_make_error(ErlNifEnv* env, ENTERM data)
{
    return enif_make_tuple2(env, JOHNNY_ATOM_ERROR, data);
}

johnny_item_t*
johnny_item_create(ENTERM key, ENTERM val)
{
    johnny_item_t* ret = (johnny_item_t*) malloc(sizeof(johnny_item_t));
    if(!ret) return NULL;

    ret->env = enif_alloc_env();
    if(!ret->env) {
        free(ret);
        return NULL;
    }

    ret->key = enif_make_copy(ret->env, key);
    ret->val = enif_make_copy(ret->env, val);

    return ret;
}

void
johnny_item_destroy(johnny_item_t* item)
{
    if(!item) return;
    if(item->env) enif_free_env(item->env);
    free(item);
}


ErlNifResourceType*
johnny_init_res(ErlNifEnv* env, const char* name, johnny_nif_dtor_t* dtor)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    return enif_open_resource_type(env, NULL, name, dtor, flags, NULL);
}
