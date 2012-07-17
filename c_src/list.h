// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_LIST_H
#define JOHNNY_LIST_H


typedef struct _johnny_list_t johnny_list_t;


johnny_list_t* johnny_list_create();
void johnny_list_destroy(johnny_list_t* h);
void johnny_list_clear(johnny_list_t* h);
int johnny_list_size(johnny_list_t* h);

int johnny_list_get(johnny_list_t* h, size_t i, void** ret);
int johnny_list_put(johnny_list_t* h, size_t i, void* i);
int johnny_list_del(johnny_list_t* h, size_t i, void** ret);

int johnny_list_splice(johnny_list_t* l1, johnny_list_t* l2, size_t pos);
int johnny_list_reverse(johnny_list_t* h);
int johnny_list_iter(johnny_list_t* h, johnny_iter_func_t* iter, void* ctx);

#endif // Included list.h
