// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_HASH_H
#define JOHNNY_HASH_H

#include "jtypes.h"

typedef struct _johnny_hash_bucket_t johnny_hash_bucket_t;
typedef struct _johnny_hash_t johnny_hash_t;

typedef struct {
    johnny_hash_func_t*     hash;
    johnny_cmp_func_t*      cmp;
    johnny_dtor_t*          dtor;
} johnny_hash_opts_t;


johnny_hash_t* johnny_hash_create(johnny_hash_opts_t* opts);
void johnny_hash_destroy(johnny_hash_t* h);
void johnny_hash_clear(johnny_hash_t* h);
int johnny_hash_get(johnny_hash_t* h, void* i, void** ret);
int johnny_hash_put(johnny_hash_t* h, void* i);
int johnny_hash_del(johnny_hash_t* h, void* i, void** ret);
int johnny_hash_iter(johnny_hash_t* h, johnny_iter_func_t* iterfunc, void* ctx);
int johnny_hash_size(johnny_hash_t* h);
int johnny_hash_resize(johnny_hash_t* h);


#endif // Included hash.h
