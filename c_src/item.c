// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "johnny.h"

johnny_item_t*
johnny_item_create1(ErlNifEnv* env, ENTERM key)
{
    johnny_item_t* ret = (johnny_item_t*) malloc(sizeof(johnny_item_t));
    if(!ret) return NULL;
    ret->env = env;
    ret->key = key;
    return ret;
}

johnny_item_t*
johnny_item_create2(ErlNifEnv* env, ENTERM key, ENTERM val)
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
