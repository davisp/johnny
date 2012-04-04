// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "johnny.h"
#include "hash.h"

ErlNifResourceType* johnny_res;
ENTERM johnny_ok_a;
ENTERM johnny_error_a;
ENTERM johnny_internal_error_a;
ENTERM johnny_already_finalized_a;
ENTERM johnny_not_found_a;

void
johnny_res_dtor(ErlNifEnv* env, void* obj)
{
    johnny_t* ctx = (johnny_t*) obj;
    ctx->dtor(ctx->data);
}

static int
load(ErlNifEnv* env, void** priv, ENTERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    johnny_res = enif_open_resource_type(
            env, NULL, "johnny_res", johnny_res_dtor, flags, NULL
        );
    if(johnny_res == NULL) return 1;

    johnny_ok_a = enif_make_atom(env, "ok");
    johnny_error_a = enif_make_atom(env, "error");
    johnny_internal_error_a = enif_make_atom(env, "internal_error");
    johnny_already_finalized_a = enif_make_atom(env, "already_finalized");
    johnny_not_found_a = enif_make_atom(env, "not_found");

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ENTERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ENTERM info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

ENTERM
nif_new(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx = (johnny_t*) enif_alloc_resource(
            johnny_res, sizeof(johnny_t)
        );
    ENTERM ret;

    if(argc != 1)
        return enif_make_badarg(env);

    // Single type for now. Eventually iterate the
    // options and look for a specified type.
    if(!johnny_hash_res_init(ctx, argv[0])) {
        ctx->finalized = 1;
        enif_release_resource(ctx);
        return johnny_make_error(env, johnny_internal_error_a);
    }

    ctx->finalized = 0;
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return johnny_make_ok(env, ret);
}

ENTERM
nif_get(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    johnny_item_t item;
    int ret;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return johnny_make_error(env, johnny_already_finalized_a);

    item.env = env;
    item.key = argv[1];
    ret = ctx->get(ctx->data, &item);
    if(ret == 0)
        return johnny_make_ok(env, item.val);
    if(ret > 0)
        return johnny_make_error(env, item.val);

    return johnny_make_error(env, johnny_internal_error_a);
}

ENTERM
nif_put(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    johnny_item_t* item;
    int ret;

    if(argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return johnny_make_error(env, johnny_already_finalized_a);

    item = johnny_item_create(argv[1], argv[2]);
    if(!item)
        return johnny_make_error(env, johnny_internal_error_a);

    ret = ctx->put(ctx->data, item);
    if(ret == 0)
        return johnny_ok_a;

    johnny_item_destroy(item);
    return johnny_make_error(env, johnny_internal_error_a);
}

ENTERM
nif_del(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    johnny_item_t item;
    int ret;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return johnny_make_error(env, johnny_already_finalized_a);

    item.env = env;
    item.key = argv[1];
    ret = ctx->del(ctx->data, &item);
    if(ret == 0)
        return johnny_make_ok(env, item.val);
    if(ret > 0)
        return johnny_make_error(env, item.val);

    return johnny_make_error(env, johnny_internal_error_a);
}

ENTERM
nif_size(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    int size;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return johnny_make_error(env, johnny_already_finalized_a);

    size = ctx->size(ctx->data);
    if(size < 0) {
        return johnny_make_error(env, johnny_internal_error_a);
    } else {
        return johnny_make_ok(env, enif_make_int(env, size));
    }
}

static ErlNifFunc funcs[] =
{
    {"new", 1, nif_new},
    {"nif_get", 2, nif_get},
    {"nif_put", 3, nif_put},
    {"nif_del", 2, nif_del},
    {"size", 1, nif_size}
};

ERL_NIF_INIT(johnny, funcs, &load, &reload, &upgrade, &unload);
