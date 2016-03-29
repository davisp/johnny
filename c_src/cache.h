// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.


#ifndef JOHNNY_CACHE_H
#define JOHNNY_CACHE_H


#include "erl_nif.h"


typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM key;
    ERL_NIF_TERM val;
    ERL_NIF_TERM opt;
} johnny_cache_t;


ErlNifResourceType* johnny_cache_init_resource(ErlNifEnv* env);

ERL_NIF_TERM johnny_cache_alloc(
        ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
    );

ERL_NIF_TERM johnny_cache_unwrap(
        ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
    );


#endif // Included johnny_cache.h