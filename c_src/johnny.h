// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_H
#define JOHNNY_H


#include "erl_nif.h"

#include "atoms.h"
#include "cache.h"


typedef struct {
    ErlNifResourceType* res_cache;
} johnny_st_t;


#endif // Included johnny.h
