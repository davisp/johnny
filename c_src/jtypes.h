// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_TYPES_H
#define JOHNNY_TYPES_H

typedef void (johnny_dtor_t) (void*);
typedef int (johnny_hash_func_t) (void*, unsigned int*);
typedef int (johnny_cmp_func_t) (void*, void*);
typedef int (johnny_iter_func_t) (void*, void*);

#endif // Included jtypes.h