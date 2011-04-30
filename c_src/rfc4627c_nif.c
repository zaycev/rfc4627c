/*
 * Name        : rfc4627c.c
 * Author      : Vladimir Zaytsev
 * Version     : 0.1
 * Copyright   : (C) Vladimir Zaytsev 2011
 * Description : Fast Erlang JSON encoder-decoder
 */

#include "erl_nif.h"
#include "rfc4627c_encoder.h"

 /*
  * NIF API
  */

static ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	
	JsonEncoder* enc = enc_new(env);
	
	encode_term(env, argv[0], enc);
	
	ErlNifBinary bin;
	
	enc_to_binary(&bin, enc);
	
	enc_free(enc);
	
	return enif_make_binary(env, &bin);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

static ErlNifFunc nif_funcs[] = {
    {"encode", 1, encode_nif}
};

ERL_NIF_INIT(rfc4627c, nif_funcs, load, reload, upgrade, unload)