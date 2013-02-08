
#include "atoms.h"
#include "util.h"


#define JOHNNY_ATOM_DECL(n, v) ERL_NIF_TERM JOHNNY_ATOM_##n;
JOHNNY_ATOM_TABLE_MAP(JOHNNY_ATOM_DECL)
#undef JOHNNY_ATOM_DECL


#define JOHNNY_ATOM_INST(n, v) JOHNNY_ATOM_##n = johnny_make_atom(env, v);
void
johnny_init_atoms(ErlNifEnv* env)
{
    JOHNNY_ATOM_TABLE_MAP(JOHNNY_ATOM_INST);
}
#undef JOHNNY_ATOM_INST