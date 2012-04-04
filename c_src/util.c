
#include "johnny.h"

ENTERM
johnny_make_ok(ErlNifEnv* env, ENTERM data)
{
    return enif_make_tuple2(env, johnny_ok_a, data);
}

ENTERM
johnny_make_error(ErlNifEnv* env, ENTERM data)
{
    return enif_make_tuple2(env, johnny_error_a, data);
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
