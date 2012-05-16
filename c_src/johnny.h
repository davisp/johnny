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
} johnny_item_t;

typedef struct _johnny_t
{
    ErlNifMutex*    lock;
    void*           data;
    int             finalized;
    int             (*get) (void*, johnny_item_t* item);
    int             (*put) (void*, johnny_item_t* item);
    int             (*del) (void*, johnny_item_t* item);
    int             (*size) (void*);
    void            (*dtor) (void*);
} johnny_t;

ENTERM johnny_make_ok(ErlNifEnv* env, ENTERM data);
ENTERM johnny_make_error(ErlNifEnv* env, ENTERM data);

johnny_item_t* johnny_item_create(ENTERM key, ENTERM val);
void johnny_item_destroy(johnny_item_t* item);

extern ENTERM johnny_ok_a;
extern ENTERM johnny_error_a;
extern ENTERM johnny_internal_error_a;
extern ENTERM johnny_already_finalized_a;
extern ENTERM johnny_not_found_a;

#endif // Included johnny.h
