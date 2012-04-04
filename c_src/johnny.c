// This file is part of Johnny released under the MIT license.
// See the LICENSE file for more information.

#include "johnny.h"

ErlNifResourceType* johnny_res;

void
johnny_res_dtor(void* obj)
{
    johnny_res_t* ctx = (johnny_ctx*) obj;
    ctx->destroy(ctx->data);
}

static int
load(ErlNifEnv* env, void** priv, ENTERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    johnny_res = enif_open_resource_type(
            env, NULL, "johnny_res", johnny_dtor, flags, NULL
        );
    if(johnny_res == NULL) return 1;
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

const ENTERM
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
    if(!johnny_hash_init(ctx, argv[0])) {
        ctx->finalized = 1;
        enif_release_resource(ctx);
        return make_atom(env, "init_error");
    }

    ctx->finalized = 0;
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}

const ENTERM
nif_get(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    ENTERM ret;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return make_error(env, "already_finalized");

    if(!ctx->get(ctx->data, argv[1], &ret))
        return make_error(env, "internal_error");

    return ret;
}

const ENTERM
nif_put(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    ENTERM ret;

    if(argc != 3)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return make_error(env, "already_finalized");

    if(!ctx->put(ctx->data, argv[1], argv[2]))
        return make_error(env, "internal_error");

    return make_ok(env);
}

const ENTERM
nif_del(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    ENTERM ret;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return make_error(env, "already_finalized");

    if(!ctx->del(ctx-data, argv[1], &ret))
        return make_error(env, "internal_error");

    return johnny_make_ok(ctx, env, ret);
}

const ENTERM
nif_size(ErlNifEnv* env, int argc, const ENTERM argv[])
{
    johnny_t* ctx;
    int size;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], johnny_res, (void**) &ctx))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return make_error("already_finalized");

    size = ctx->size(ctx->data);
    if(size < 0) {
        return make_error("internal_error");
    } else {
        return johnny_make_ok(ctx, env, enif_make_int(env, size));
    }
}

static ErlNifFun funcs[] =
{
    {"nif_new", 1, nif_new},
    {"nif_get", 2, nif_get},
    {"nif_put", 3, nif_put},
    {"nif_del", 2, nif_del},
    {"nif_size", 1, nif_size}
};

ERL_NIF_INIT(johnny, funcs, &load, &reload, &upgrade, &unload);
