#include "atoms.h"
#include "list.h"
#include "nif_list.h"


typedef struct {
    johnny_list_t*  list;
    ErlNifMutex*    lock;
} johnny_nif_list_t;

typedef struct {
    ErlNifEnv*      env;
    ENTERM          val;
} johnny_nif_list_iter_t;

typedef struct {
    ErlNifEnv*      env;
    ENTERM          val;
    int             count;
} johnny_nif_list_iter_t;


ErlNifResourceType* JOHNNY_NIF_LIST_RES;


void
johnny_nif_list_destroy(ErlNifEnv* env, void* obj)
{
    johnny_nif_list_t* nh = (johnny_nif_list_t*) obj;
    if(nl->list != NULL)
        johnny_hash_destroy(nl->list);
    if(nl->lock != NULL)
        enif_mutex_destroy(nl->lock);
    return;
}


int
johnny_nif_list_init(ErlNifEnv* env)
{
    JOHNNY_NIF_HASH_RES = johnny_init_res(
            env, "johnny_hash", &johnny_nif_hash_destroy
        );
    if(JOHNNY_NIF_HASH_RES == NULL)
        return 0;
    return 1;
}


ENTERM
johnny_nif_list_new(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    ENTERM ret;

    nl = (johnny_nif_list_t*) enif_alloc_resource(
            JOHNNY_NIF_LIST_RES, sizeof(johnny_nif_list_t)
        );

    if(nl == NULL)
        goto error;

    nl->list = NULL;
    nl->lock = NULL;

    nl->list = johnny_list_create(&johnny_item_destroy);
    if(nl->list == NULL)
        goto error;

    nl->lock = enif_mutex_create("hash_lock");
    if(nl->lock == NULL)
        goto error;

    ret = enif_make_resource(env, nl);
    enif_release_resource(nl);
    return johnny_make_ok(env, ret);

error:
    if(nl != NULL)
        enif_release_resource(nl);
    return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
}


ENTERM
johnny_nif_list_clear(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    johnny_list_clear(nl->list);
    enif_mutex_unlock(nl->lock);
    
    return JOHNNY_ATOM_OK;
}


ENTERM
johnny_nif_list_size(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    int size;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    size = johnny_list_size(nl->list);
    enif_mutex_unlock(nl->lock);
    
    return johnny_make_ok(env, enif_make_int(env, size));
}


ENTERM
johnny_nif_list_get(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    int ret;
    int pos;
    
    if(argc != 2 && argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &pos))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_get(nl->list, pos, &val);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
    
    if(val == NULL && argc == 3) {
        return johnny_make_ok(env, argv[2]);
    } else if(val == NULL) {
        return johnny_make_error(env, JOHNNY_ATOM_NOT_FOUND);
    } else {
        return johnny_make_ok(env, enif_make_copy(env, val->val));
    }
}


ENTERM
johnny_nif_list_put(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    ENTERM val;
    int ret;
    int pos;
    
    if(argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &pos))
        return enif_make_badarg(env);

    val = johnny_make_item(0, argv[2]);
    if(!val)
        return enif_make_error(env, JOHNNY_ATOM_INTERNAL ERROR);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_put(nl->list, pos, &val);
    enif_mutex_unlock(nl->lock);
    
    if(ret) {
        return JOHNNY_ATOM_OK;
    } else {
        johnny_item_destroy(val);
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
    }
}


ENTERM
johnny_nif_list_del(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    int ret;
    int pos;
    
    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[1], &pos))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_del(nl->list, pos, &val);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
    
    johnny_item_destroy(val);
    return JOHNNY_ATOM_OK;
}

ENTERM
johnny_nif_list_unshift(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    int ret;
    
    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    val = johnny_make_item(0, argv[1]);
    if(!val)
        return enif_make_error(env, JOHNNY_ATOM_INTERNAL ERROR);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_put(nl->list, 0, &val);
    enif_mutex_unlock(nl->lock);
    
    if(ret) {
        return JOHNNY_ATOM_OK;
    } else {
        johnny_item_destroy(val);
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
    }
}


ENTERM
johnny_nif_list_shift(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    ENTERM rval;
    int count;
    int ret;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_del(nl->list, 0, &val);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    rval = johnny_make_ok(env, enif_make_copy(env, val->val));
    johnny_item_destroy(val);
    return rval;
}


ENTERM
johnny_nif_list_push(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    int ret;
    
    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    val = johnny_make_item(0, argv[1]);
    if(!val)
        return enif_make_error(env, JOHNNY_ATOM_INTERNAL ERROR);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_size(nl->list);
    ret = johnny_list_put(nl->list, ret, &val);
    enif_mutex_unlock(nl->lock);
    
    if(ret) {
        return JOHNNY_ATOM_OK;
    } else {
        johnny_item_destroy(val);
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);
    }    
}


ENTERM
johnny_nif_list_pop(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_item_t* val;
    ENTERM rval;
    int count;
    int ret;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_size(nl->list);
    ret = johnny_list_del(nl->list, ret-1, &val);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    rval = johnny_make_ok(env, enif_make_copy(env, val->val));
    johnny_item_destroy(val);
    return rval;
}


ENTERM
johnny_nif_list_splice(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl1;
    johnny_nif_list_t* nl2;
    int pos;
    int ret;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl1))
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[1], JOHNNY_NIF_LIST_RES, (void**) &nl2))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[2], &pos))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_splice(nl1->list, nl2->list, pos);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    return JOHNNY_ATOM_OK;
}


ENTERM
johnny_nif_list_extend(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl1;
    johnny_nif_list_t* nl2;
    int pos;
    int ret;
    
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl1))
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[1], JOHNNY_NIF_LIST_RES, (void**) &nl2))
        return enif_make_badarg(env);

    enif_mutex_lock(nl->lock);
    ret = johnny_list_size(nl1->list);
    ret = johnny_list_splice(nl1->list, nl2->list, ret);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    return JOHNNY_ATOM_OK;
}


int
johnny_nif_list_count_iter(void* ctx, void* obj)
{
    johnny_nif_list_iter_t* iter = (johnny_nif_list_iter*) ctx;
    johnny_list_item_t* item = (johnny_list_item_t*) obj;

    if(enif_compare(iter->val, item->val) == 0)
        iter->count++;
    
    return 1;
}


ENTERM
johnny_nif_list_count(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_nif_list_t* nl;
    johnny_nif_list_iter_t iterval;
    int pos;
    int ret;
    
    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], JOHNNY_NIF_LIST_RES, (void**) &nl1))
        return enif_make_badarg(env);

    iterval.env = env;
    iterval.val = argv[1];
    iterval.count = 0;

    enif_mutex_lock(nl->lock);
    ret = johnny_list_splice(nl->list, &johnny_nif_list_count_iter, &iterval);
    enif_mutex_unlock(nl->lock);
    
    if(!ret)
        return johnny_make_error(env, JOHNNY_ATOM_INTERNAL_ERROR);

    return johnny_make_ok(env, enif_make_int(env, iterval.count));
}


ENTERM
johnny_nif_list_index(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    
}


ENTERM
johnny_nif_list_remove(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    
}


ENTERM
johnny_nif_list_to_list(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    
}
