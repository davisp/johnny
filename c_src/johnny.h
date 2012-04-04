// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_H
#define JOHNNY_H

#include "erl_nif.h"

typedef ERL_NIF_TERM ENTERM;

typedef struct _johnny_item_t
{
    ErlNifEnv*      env;
    ENTERM          key;
    ENTERM          val;
} johnny_val_t;

typedef struct _johnny_t
{
    void*           data;
    int             finalized;
    int             (*get) (johnny_t*, johnny_item_t* item);
    int             (*put) (johnny_t*, johnny_item_t* item);
    int             (*del) (johnny_t*, johnny_item_t* item);
    int             (*size) (johnny_t*);
    void            (*dtor) (johnny_t*);
} johnny_t;

ERL_NIF_TERM johnny_make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM johnny_make_ok(johnny_ct* c, ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM johnny_make_error(johnny_ct* c, ErlNifEnv* env, const char* error);

int johnny_hash_init(johnny_t* ctx, ENTERM opts);

johnny_item_t* johnny_item_create1(ErlNifEnv* env, ENTERM key);
johnny_item_t* johnny_item_create2(ErlNifEnv* env, ENTERM key, ENTERM val);
void johnny_item_destroy(johnny_item_t* item);

#endif // Included johnny.h
