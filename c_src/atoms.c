// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.


#include "johnny.h"


#define ATOM_MAP(defname, atomname) ERL_NIF_TERM ATOM_##defname;
#include "atoms.list"
#undef ATOM_MAP


#define ATOM_MAP(defname, atomname) \
    ATOM_##defname = enif_make_atom(env, #atomname);

void
johnny_init_atoms(ErlNifEnv* env)
{
#include "atoms.list"
}

#undef ATOM_MAP
