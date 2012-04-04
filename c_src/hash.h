// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_HASH_H
#define JOHNNY_HASH_H

#include "johnny.h"

typedef struct _johnny_hash_bucket_t johnny_hash_bucket;
typedef struct _johnny_hash_t johnny_hash_t;
typedef int (johnny_hash_func_t*) (ErlNifEnv*, ENTERM, unsigned int*);

johnny_hash_t* johnny_hash_create(ENTERM opts);
void johnny_hash_destroy(johnny_hash_t* h);
void johnny_hash_clear(johnny_hash_t* h);
int johnny_hash_get(johnny_hash_t* h, johnny_item_t* i);
int johnny_hash_put(johnny_hash_t* h, johnny_item_t* i);
int johnny_hash_del(johnny_hash_t* h, johnny_item_t* i);
int johnny_hash_size(johnny_hash_t* h);
int johnny_hash_resize(johnny_hash_t* h);

// Hash Functions
int johnny_jenkins_single(ErlNifEnv* env, ENTERM key, unsigned int* hash);

#endif // Included hash.h
