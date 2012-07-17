
#include "atoms.h"
#include "hash.h"
#include "nif_hash.h"


typedef struct {
    johnny_hash_t*  hash;
    ErlNifMutex*    lock;
} johnny_nif_hash_t;


typedef struct {
    ErlNifEnv*      env;
    ENTERM          val;
} johnny_nif_hash_iter_t;


ErlNifResourceType* JOHNNY_NIF_HASH_RES;


int
johnny_nif_hash_hash(void* obj, unsigned int* ret)
{
    johnny_item_t* item = (johnny_item_t*) obj;
    return johnny_jenkins_single(item->env, item->key, ret);
}


int
johnny_nif_hash_cmp(void* a, void* b)
{
    johnny_item_t* ia = (johnny_item_t*) a;
    johnny_item_t* ib = (johnny_item_t*) b;
    return enif_compare(ia->key, ib->key);
}


void
johnny_nif_hash_item_dtor(void* obj)
{
    johnny_item_t* i = (johnny_item_t*) obj;
    johnny_item_destroy(i);
}


void
johnny_nif_hash_destroy(ErlNifEnv* env, void* obj)
{
    johnny_nif_hash_t* nh = (johnny_nif_hash_t*) obj;
    if(nh->hash != NULL)
        johnny_hash_destroy(nh->hash);
    if(nh->lock != NULL)
        enif_mutex_destroy(nh->lock);
    return;
}


int
johnny_nif_hash_init(ErlNifEnv* env)
{
    JOHNNY_NIF_HASH_RES = johnny_init_res(
            env, "johnny_hash", &johnny_nif_hash_destroy
        );
    if(JOHNNY_NIF_HASH_RES == NULL)
        return 0;
    return 1;
}


ENTERM
johnny_nif_hash_new(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_hash_opts_t hopts;
    ENTERM ret;

    nh = (johnny_nif_hash_t*) enif_alloc_resource(
            JOHNNY_NIF_HASH_RES, sizeof(johnny_nif_hash_t)
        );

    if(nh == NULL)
        goto error;

    nh->hash = NULL;
    nh->lock = NULL;

    hopts.hash = &johnny_nif_hash_hash;
    hopts.cmp = &johnny_nif_hash_cmp;
    hopts.dtor = &johnny_nif_hash_item_dtor;
    nh->hash = johnny_hash_create(&hopts);
    if(nh->hash == NULL)
        goto error;

    nh->lock = enif_mutex_create("hash_lock");
    if(nh->lock == NULL)
        goto error;

    ret = enif_make_resource(env, nh);
    enif_release_resource(nh);
    return johnny_make_ok(env, ret);

error:
    if(nh != NULL) {
        enif_release_resource(nh);
    }
    return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
}


ENTERM
johnny_nif_hash_clear(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);

    enif_mutex_lock(nh->lock);
    johnny_hash_clear(nh->hash);
    enif_mutex_unlock(nh->lock);
    
    return JOHNNY_ATOM_OK;
}


ENTERM
johnny_nif_hash_get(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_item_t key;
    johnny_item_t* val;
    int ret;

    if(argc != 2 && argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);
    if(!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    key.env = env;
    key.key = argv[1];
    
    enif_mutex_lock(nh->lock);
    ret = johnny_hash_get(nh->hash, (void*) &key, (void**) &val);
    enif_mutex_unlock(nh->lock);

    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    if(val == NULL && argc == 2) {
        return johnny_make_error(env, JOHNNY_ATOM_NOT_FOUND);
    } else if(val == NULL && argc == 3) {
        return johnny_make_ok(env, argv[2]);
    } else {
        return johnny_make_ok(env, enif_make_copy(env, val->val));
    }
}


ENTERM
johnny_nif_hash_put(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_item_t* item;
    int ret;

    if(argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);
    if(!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    item = johnny_item_create(argv[1], argv[2]);
    if(!item)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    enif_mutex_lock(nh->lock);
    ret = johnny_hash_put(nh->hash, item);
    enif_mutex_unlock(nh->lock);
    
    if(ret) {
        return JOHNNY_ATOM_OK;
    } else {
        johnny_item_destroy(item);
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);        
    }
}


ENTERM
johnny_nif_hash_del(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_item_t key;
    johnny_item_t* val;
    int ret;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);
    if(!enif_is_binary(env, argv[1]))
        return enif_make_badarg(env);

    key.env = env;
    key.key = argv[1];

    enif_mutex_lock(nh->lock);
    ret = johnny_hash_del(nh->hash, (void*) &key, (void**) &val);
    enif_mutex_unlock(nh->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    if(val != NULL) {
        johnny_item_destroy(val);
        return JOHNNY_ATOM_OK;
    } else {
        return johnny_make_error(env, JOHNNY_ATOM_NOT_FOUND);   
    }
}


int
johnny_nif_hash_key_iter(void* ctx, void* obj)
{
    johnny_nif_hash_iter_t* iter = (johnny_nif_hash_iter_t*) ctx;
    johnny_item_t* item = (johnny_item_t*) obj;
    ENTERM cp = enif_make_copy(iter->env, item->key);
    iter->val = enif_make_list_cell(iter->env, cp, iter->val);
    return 1;
}


ENTERM
johnny_nif_hash_keys(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_nif_hash_iter_t iter;
    int ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);

    iter.env = env;
    iter.val = enif_make_list(env, 0);

    enif_mutex_lock(nh->lock);
    ret = johnny_hash_iter(nh->hash, &johnny_nif_hash_key_iter, &iter);
    enif_mutex_unlock(nh->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    return johnny_make_ok(env, iter.val);
}


int
johnny_nif_hash_list_iter(void* ctx, void* obj)
{
    johnny_nif_hash_iter_t* iter = (johnny_nif_hash_iter_t*) ctx;
    johnny_item_t* item = (johnny_item_t*) obj;
    ENTERM key = enif_make_copy(iter->env, item->key);
    ENTERM val = enif_make_copy(iter->env, item->val);
    val = enif_make_tuple2(iter->env, key, val);
    iter->val = enif_make_list_cell(iter->env, val, iter->val);
    return 1;
}


ENTERM
johnny_nif_hash_to_list(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    johnny_nif_hash_iter_t iter;
    int ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);

    iter.env = env;
    iter.val = enif_make_list(env, 0);

    enif_mutex_lock(nh->lock);
    ret = johnny_hash_iter(nh->hash, &johnny_nif_hash_list_iter, &iter);
    enif_mutex_unlock(nh->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    return johnny_make_ok(env, iter.val);
}


ENTERM
johnny_nif_hash_size(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_hash_t* nh;
    int size;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_HASH_RES, (void**) &nh))
        return enif_make_badarg(env);

    enif_mutex_lock(nh->lock);
    size = johnny_hash_size(nh->hash);
    enif_mutex_unlock(nh->lock);

    return johnny_make_ok(env, enif_make_int(env, size));
}
