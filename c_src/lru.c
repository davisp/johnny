// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "hash.h"
#include "lru.h"

typedef struct _johnny_lru_entry_t johnny_lru_entry_t;

typedef struct _johnny_lru_entry_t {
    johnny_lru_entry_t*     prev;
    johnny_lru_entry_t*     next;
    johnny_lru_item_t*      item;
};

typedef struct _johnny_lru_t {
    johnny_hash_t*          hash;
    johnny_lru_entry_t*     head;
    johnny_lru_entry_t*     tail;
};


johnny_lru_t*
johnny_lru_create(ENTERM opts)
{
    johnny_lru_t* ret = (johnny_lru_t*) malloc(sizeof(johnny_lru_t));
    if(!ret) return NULL;

    ret->hash = johnny_hash_create(opts);
    if(!ret->hash) {
        free(ret);
        return ret;
    }

    ret->head = NULL;
    ret->tail = NULL;

    return ret;
}

void
johnny_lru_destroy(johnny_lru_t* l)
{
    if(!l) return;

    johnny_lru_clear(l);
    johnny_hash_destroy(l);
    free(l);
}

void
johnny_lru_clear(johnny_lru_t* l)
{
    // ok
}

int
johnny_lru_get(johnny_lru_t* l, johnny_item_t* i)
{
    // ok
}

int
johnny_lru_put(johnny_lru_t* l, johnny_item_t* i)
{
    // ok
}

int
johnny_lru_del(johnny_lru_t* l, johnny_item_t* i)
{
    // ok
}

int
johnny_lru_size(johnny_lru_t* l)
{
    return johnny_hash_size(l->hash);
}

int
johnny_lru_res_init(johnny_t* res, ENTERM opts)
{
    return 0;
}
