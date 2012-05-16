// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_LRU_H
#define JOHNNY_LRU_H

#include "johnny.h"

typedef struct _johnny_lru_t johnny_lru_t;

johnny_lru_t* johnny_lru_create(ENTERM opts);
void johnny_lru_destroy(johnny_lru_t* l);
void johnny_lru_clear(johnny_lru_t* l);
int johnny_lru_get(johnny_lru_t* l, johnny_item_t* i);
int johnny_lru_put(johnny_lru_t* l, johnny_item_t* i);
int johnny_lru_del(johnny_lru_t* l, johnny_item_t* i);
int johnny_lru_size(johnny_lru_t* l);

// Resource init
int johnny_lru_res_init(johnny_t* res, ENTERM opts);


#endif // Included lru.h
