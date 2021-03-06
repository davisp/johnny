// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.
//
// First draft of the implementation was cribbed from khash.h which
// is also released under the MIT license. khash.h can be found here:
//
//     http://attractivechaos.awardspace.com/khash.h.html
//
//  The original khash.h is:
//
//  Copyright (c) 2008, by Attractive Chaos <attractivechaos@aol.co.uk>


#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "hash.h"


#define JOHNNY_HASH_INIT_BUCKETS 1024
#define JOHNNY_HASH_MAX_LOAD 0.75
#define JOHNNY_HASH_MIN_LOAD 0.25


struct _johnny_hash_bucket_t {
    unsigned int                    hval;
    void*                           item;
    struct _johnny_hash_bucket_t*   next;
    struct _johnny_hash_bucket_t*   tail;
};

struct _johnny_hash_t {
    johnny_hash_func_t*             hashfunc;
    johnny_cmp_func_t*              cmpfunc;
    johnny_dtor_t*                  dtor;

    johnny_hash_bucket_t**          buckets;
    int                             num_buckets;
    int                             num_occupied;
    int                             upper_bound;
    int                             size;
};


johnny_hash_t*
johnny_hash_create(johnny_hash_opts_t* opts)
{
    johnny_hash_t* ret;
    size_t bucket_bytes;

    ret = (johnny_hash_t*) malloc(sizeof(johnny_hash_t));
    if(!ret) return NULL;

    bucket_bytes = sizeof(johnny_hash_t*) * JOHNNY_HASH_INIT_BUCKETS;
    ret->buckets = (johnny_hash_bucket_t**) malloc(bucket_bytes);
    if(!ret->buckets) {
        free(ret);
        return NULL;
    }
    memset(ret->buckets, 0, bucket_bytes);

    ret->hashfunc = opts->hash;
    ret->cmpfunc = opts->cmp;
    ret->num_buckets = JOHNNY_HASH_INIT_BUCKETS;
    ret->num_occupied = 0;
    ret->upper_bound = (int) (ret->num_buckets * JOHNNY_HASH_MAX_LOAD);
    ret->size = 0;
    ret->dtor = opts->dtor;

    return ret;
}

void
johnny_hash_destroy(johnny_hash_t* h)
{
    if(!h)
        return;
    johnny_hash_clear(h);
    free(h);
}

void
johnny_hash_clear(johnny_hash_t* h)
{
    johnny_hash_bucket_t* iter;
    johnny_hash_bucket_t* next;
    size_t i;

    assert(h && "hash is null");
    assert(h->buckets && "hash has no buckets");

    for(i = 0; i < h->num_buckets; i++) {
        iter = h->buckets[i];
        while(iter) {
            next = iter->next;
            if(h->dtor)
                h->dtor(iter->item);
            free(iter);
            iter = next;
        }
        h->buckets[i] = NULL;
    }
    h->num_occupied = 0;
    h->size = 0;
}

int
johnny_hash_get(johnny_hash_t* h, void* item, void** ret)
{
    johnny_hash_bucket_t* iter;
    unsigned int hval;

    assert(h != NULL && "hash is null");
    assert(h->buckets != NULL && "hash has no buckets");

    *ret = NULL;

    if(!h->hashfunc(item, &hval))
        return 0;

    iter = h->buckets[hval % h->num_buckets];
    while(iter) {
        if(iter->hval != hval) {
            iter = iter->next;
            continue;
        }
        if(h->cmpfunc(item, iter->item) == 0) {
            *ret = iter->item;
            return 1;
        }
        iter = iter->next;
    }

    return 1;
}

int
johnny_hash_put(johnny_hash_t* h, void* i)
{
    johnny_hash_bucket_t* iter;
    johnny_hash_bucket_t* next;
    unsigned int hval;

    assert(h != NULL && "hash is null");
    assert(h->buckets != NULL && "hash has no buckets");

    if(!h->hashfunc(i, &hval))
        return 0;

    iter = h->buckets[hval % h->num_buckets];
    while(iter) {
        if(iter->hval != hval) {
            iter = iter->next;
            continue;
        }
        if(h->cmpfunc(i, iter->item) == 0) {
            h->dtor(iter->item);
            iter->item = i;
            return 1;
        }
        iter = iter->next;
    }

    next = (johnny_hash_bucket_t*) malloc(sizeof(johnny_hash_bucket_t));
    if(!next)
        return 0;
    next->hval = hval;
    next->item = i;
    next->next = NULL;
    next->tail = NULL;

    iter = h->buckets[hval % h->num_buckets];

    if(iter) {
        if(iter->tail) iter->tail->next = next;
        iter->tail = next;
    } else {
        h->buckets[hval % h->num_buckets] = next;
        h->num_occupied++;
    }
    h->size++;

    return 1;
}

int
johnny_hash_del(johnny_hash_t* h, void* item, void** ret)
{
    johnny_hash_bucket_t* iter;
    johnny_hash_bucket_t* prev;
    unsigned int hval;

    *ret = NULL;

    assert(h != NULL && "hash is null");
    assert(h->buckets != NULL && "hash has no buckets");

    if(!h->hashfunc(item, &hval))
        return 0;

    iter = h->buckets[hval % h->num_buckets];
    prev = NULL;
    while(iter) {
        if(iter->hval != hval) {
            prev = iter;
            iter = iter->next;
            continue;
        }
        if(h->cmpfunc(item, iter->item) == 0) {
            break;
        }
        prev = iter;
        iter = iter->next;
    }

    if(!iter) {
        *ret = NULL;
        return 1;
    }

    if(prev == NULL) {
        h->buckets[hval % h->num_buckets] = iter->next;
        if(iter->next)
            iter->next->tail = iter->tail;
        if(iter->next == NULL)
            h->num_occupied--;
    } else {
        prev->next = iter->next;
    }
    h->size--;

    *ret = iter->item;
    free(iter);

    return 1;
}

int
johnny_hash_iter(johnny_hash_t* h, johnny_iter_func_t* iterfunc, void* ctx)
{
    johnny_hash_bucket_t* iter;
    size_t i;

    assert(h && "hash is null");
    assert(h->buckets && "hash has no buckets");

    for(i = 0; i < h->num_buckets; i++) {
        iter = h->buckets[i];
        while(iter) {
            if(!iterfunc(ctx, iter->item))
                return 0;
            iter = iter->next;
        }
    }

    return 1;
}

int
johnny_hash_size(johnny_hash_t* h)
{
    assert(h->size >= 0 && "invalid hash size");
    return h->size;
}

int
johnny_hash_resize(johnny_hash_t* h)
{
    johnny_hash_bucket_t** new_buckets;
    johnny_hash_bucket_t* iter;
    johnny_hash_bucket_t* next;
    unsigned int new_num_buckets;
    unsigned int new_occupied;
    unsigned int new_size;
    size_t bucket_bytes;
    size_t i;

    if(h->num_occupied < h->upper_bound) return 1;

    new_num_buckets = (int) (h->num_occupied / JOHNNY_HASH_MIN_LOAD);
    new_occupied = 0;
    new_size = 0;

    bucket_bytes = sizeof(johnny_hash_bucket_t*) * new_num_buckets;
    new_buckets = (johnny_hash_bucket_t**) malloc(bucket_bytes);
    if(new_buckets == NULL)
        return 0;

    for(i = 0; i < h->num_buckets; i++) {
        while(h->buckets[i]) {
            new_size++;
            next = h->buckets[i];
            h->buckets[i] = next->next;
            next->next = NULL;
            next->tail = NULL;
            iter = new_buckets[next->hval % new_size];
            if(iter == NULL) {
                new_buckets[next->hval % new_size] = next;
                new_occupied++;
                continue;
            }
            iter->tail->next = next;
            iter->tail = next;
        }
    }

    free(h->buckets);
    h->buckets = new_buckets;
    h->num_buckets = new_size;
    h->num_occupied = new_occupied;
    assert(new_size == h->size && "missing item after hash resize");
    h->upper_bound = (int) (JOHNNY_HASH_MAX_LOAD * new_num_buckets);

    return 1;
}
