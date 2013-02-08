// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_NIF_HASH_H
#define JOHNNY_NIF_HASH_H

#include "johnny.h"
#include "util.h"


int johnny_nif_hash_init(ErlNifEnv* env);


ENTERM johnny_nif_hash_new(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_clear(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_get(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_put(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_del(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_size(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_keys(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_hash_to_list(ErlNifEnv* env, int argc, const ENTERM argv[]);

// Hash functions

int johnny_jenkins_single(ErlNifEnv* env, ENTERM key, unsigned int* ret);

#endif // Included nif_hash.h
