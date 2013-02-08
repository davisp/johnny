// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_ATOMS_H
#define JOHNNY_ATOMS_H


#include "erl_nif.h"


#define JOHNNY_ATOM_TABLE_MAP(XX)                       \
    XX(ALREADY_FINALIZED,   "already_finalized")        \
    XX(ERROR,               "error")                    \
    XX(INTERNAL_ERROR,      "internal_error")           \
    XX(NOT_FOUND,           "not_found")                \
    XX(OK,                  "ok")


#define JOHNNY_ATOM_EXTERN(n, v) extern ERL_NIF_TERM JOHNNY_ATOM_##n;
JOHNNY_ATOM_TABLE_MAP(JOHNNY_ATOM_EXTERN)
#undef JOHNNY_ATOM_EXTERN


void johnny_init_atoms(ErlNifEnv* env);


#endif // Included atoms.h