// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_NIF_LIST_H
#define JOHNNY_NIF_LIST_H

#include "johnny.h"
#include "util.h"


int johnny_nif_list_init(ErlNifEnv* env);

ENTERM johnny_nif_list_new(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_clear(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_size(ErlNifEnv* env, int argc, const ENTERM argv[]);

ENTERM johnny_nif_list_get(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_put(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_del(ErlNifEnv* env, int argc, const ENTERM argv[]);

ENTERM johnny_nif_list_unshift(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_shift(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_push(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_pop(ErlNifEnv* env, int argc, const ENTERM argv[]);

ENTERM johnny_nif_list_extend(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_splice(ErlNifEnv* env, int argc, const ENTERM argv[]);

ENTERM johnny_nif_list_count(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_index(ErlNifEnv* env, int argc, const ENTERM argv[]);
ENTERM johnny_nif_list_remove(ErlNifEnv* env, int argc, const ENTERM argv[]);

ENTERM johnny_nif_list_to_list(ErlNifEnv* env, int argc, const ENTERM argv[]);

#endif // Included nif_hash.h
