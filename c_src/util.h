// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_UTIL_H
#define JOHNNY_UTIL_H

#include "johnny.h"
#include "jtypes.h"


typedef struct _johnny_item_t
{
    ErlNifEnv*      env;
    ENTERM          key;
    ENTERM          val;
} johnny_item_t;


johnny_item_t* johnny_item_create(ENTERM key, ENTERM val);
void johnny_item_destroy(johnny_item_t* item);


ENTERM johnny_make_atom(ErlNifEnv* env, const char* name);
ENTERM johnny_make_ok(ErlNifEnv* env, ENTERM data);
ENTERM johnny_make_error(ErlNifEnv* env, ENTERM data);


ErlNifResourceType* johnny_init_res(
    ErlNifEnv* env, const char* name, johnny_nif_dtor_t* dtor);


#endif // Included util.h
