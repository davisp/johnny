// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "phash2.h"

// make_hash2 is actually an internal Erlang VM
// function. We're being a bit hackish here using
// it even though its not actually part of the
// NIF API.
//
// I've submitted a patch to the Erlang team to
// get this into the public API which hopefully
// goes through.

unsigned int make_hash2(ENTERM term);


unsigned int johnny_phash2(ENTERM term)
{
    return make_hash2(term);
}