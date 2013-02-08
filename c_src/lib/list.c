#include <stdlib.h>

#include "list.h"


typedef struct _johnny_list_item_t
{
    void*                           item;
    struct _johnny_list_item_t*     next;
    struct _johnny_list_item_t*     prev;
} johnny_list_item_t;

struct _johnny_list_t
{
    johnny_dtor_t*                  dtor;
    johnny_list_item_t*             head;
    johnny_list_item_t*             tail;
    int                             size;
};


johnny_list_t*
johnny_list_create(johnny_dtor_t* dtor)
{
    johnny_list_t* ret = (johnny_list_t*) malloc(sizeof(johnny_list_t));
    if(!ret)
        goto error;

    ret->dtor = dtor;
    ret->head = NULL;
    ret->tail = NULL;
    ret->size = 0;

    return ret;

error:
    if(ret != NULL)
        free(ret);
    return NULL;
}


void
johnny_list_destroy(johnny_list_t* l)
{
    if(l == NULL)
        return;
    johnny_list_clear(l);
    free(l);
}


void
johnny_list_clear(johnny_list_t* l)
{
    johnny_list_item_t* iter;
    johnny_list_item_t* next;
    if(l == NULL)
        return;

    iter = l->head;
    while(iter) {
        if(l->dtor)
            l->dtor(iter->item);
        next = iter->next;
        free(iter);
        iter = next;
    }
    l->head = NULL;
    l->tail = NULL;
    l->size = 0;
}


int
johnny_list_size(johnny_list_t* l)
{
    return l->size;
}


int
johnny_list_get(johnny_list_t* l, int i, void** ret)
{
    johnny_list_item_t* iter;
    int p;

    if(i < 0)
        i = l->size + i
    if(i < 0 || i >= l->size)
        return 0;

    if(i == 0) {
        iter = l->head;
    } else if(i == l->size - 1) {
        iter = l->tail;
    } else {
        iter = l->head;
        for(p = 0; p < i; p++) {
            iter = iter->next;
        }
    }

    *ret = iter->item;
    return 1;
}


int
johnny_list_put(johnny_list_t* l, int i, void* v)
{
    johnny_list_item_t* iter;
    johnny_list_item_t* item =
            (johnny_list_item_t*) malloc(sizeof(johnny_list_item_t));
    int p;

    if(item == NULL)
        return 0;

    item->item = v;

    if(i < 0)
        i = l->size + i
    if(i < 0 || i > l->size)
        return 0;

    if(i == 0) {
        iter = l->head;
    } else if(i == l->size - 1) {
        iter = l->tail;
    } else {
        iter = l->head;
        for(p = 0; p < i; p++) {
            iter = iter->next;
        }
    }

    if(iter == NULL) {
        // Appending an item
        item->prev = l->tail;
        item->prev->next = item;
        item->next = NULL;
        l->tail = item;
    } else {
        item->prev = iter->prev;
        item->prev->next = item;
        item->next = iter;
        item->next->prev = item;
        if(item->prev) {
            item->prev->next = item;
        } else {
            l->head = item;
        }
    }

    l->size++;

    return 1;
}


int
johnny_list_del(johnny_list_t* l, int i, void** ret)
{
    johnny_list_item_t* iter;
    int p;

    if(i < 0)
        i = l->size + i
    if(i < 0 || i >= l->size)
        return 0;

    if(i == 0) {
        iter = l->head;
    } else if(i == l->size - 1) {
        iter = l->tail;
    } else {
        iter = l->head;
        for(p = 0; p < i; p++) {
            iter = iter->next;
        }
    }

    if(iter->prev) {
        iter->prev->next = iter->next;
    } else {
        l->head = iter->next;
    }

    if(iter->next) {
        iter->next->prev = iter->prev;
    } else {
        l->tail = iter->prev;
    }

    *ret = iter->item;
    free(iter);
    l->size--;

    return 1;
}


int
johnny_list_splice(johnny_list_t* l1, johnny_list_t* l2, int i)
{
    johnny_list_item_t* iter1;
    johnny_list_item_t* iter2;
    johnny_list_item_t* item;
    int p;

    if(i < 0 || i >= l1->size)
        return 0;

    if(i == 0) {
        iter1 = l1->head;
    } else if(i == l1->size - 1) {
        iter1 = l1->tail;
    } else {
        iter1 = l1->head;
        for(p = 0; p < i; p++) {
            iter1 = iter1->next;
        }
    }

    iter2 = l2->head;
    while(iter2) {
        item = (johnny_list_item_t*) malloc(sizeof(johnny_list_item_t));
        if(!item)
            goto error;

        item->prev = iter1->prev;
        item->next = iter1;

        if(item->prev)
            item->prev->next = item;
        if(item->next)
            item->next->prev = item;

        l1->size++;
    }

    return 1;

error:
    if(item != NULL)
        free(item);
    return 0;
}


int
johnny_list_reverse(johnny_list_t* l)
{
    johnny_list_t tmp;
    johnny_list_item_t* iter;
    johnny_list_item_t* next;

    if(l->size < 2)
        return 1;

    tmp.head = l->head;
    tmp.tail = l->head;
    iter = l->head->next;
    tmp.head->next = NULL;
    tmp.head->prev = NULL;

    while(iter) {
        next = iter->next;
        iter->next = tmp.head;
        iter->prev = NULL;
        tmp.head->prev = iter;
        tmp.head = iter;
        iter = next;
    }

    l->head = tmp.head;
    l->tail = tmp.tail;

    return 1;
}


int
johnny_list_iter(johnny_list_t* l, johnny_iter_func_t* iterfunc, void* ctx)
{
    johnny_list_item_t* iter;

    iter = l->head;
    while(iter) {
        if(!iterfunc(ctx, iter->item))
            return 0;
        iter = iter->next;
    }

    return 1;
}
