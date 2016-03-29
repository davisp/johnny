// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#ifndef JOHNNY_ATOMS_H
#define JOHNNY_ATOMS_H


#define ATOM_MAP(defname, atomname) extern ERL_NIF_TERM ATOM_##defname;
#include "atoms.list"
#undef ATOM_MAP


void johnny_init_atoms(ErlNifEnv* env);


#endif // Included atoms.h