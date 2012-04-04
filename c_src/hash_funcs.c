
#include "johnny.h"

int
johnny_jenkins_single(ErlNifEnv* env, ENTERM key, unsigned int* ret)
{
    ErlNifBinary bin;
    unsigned int hash;
    unsigned int i;

    *ret = 0;

    if(!enif_inspect_binary(env, key, &bin))
        return 0;

    for(hash = i = 0; i < bin.size; ++i) {
        hash += bin.data[i];
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }

    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);
    *ret = hash;

    return 1;
}
