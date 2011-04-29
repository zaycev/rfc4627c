/*
 ============================================================================
 Name        : feje.c
 Author      : Vladimir Zaytsev
 Version     : 0.1
 Copyright   : (C) Vladimir Zaytsev 2011
 Description : Fast Erlang JSON encoder-decoder
 ============================================================================
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "erl_nif.h"
#include "erl_nif.h"

#define DEFAULT_OUTPUT_BUFF_SIZE 1024
#define MAX_ATOM_SIZE 64
#define MAX_NUMBER_LENGTH 64


/*
 * Context - simple encoder state container
 */
typedef struct {
	/*
	 * Buffers
	 */
	unsigned char*	 		buff_output;
	ErlNifBinary 			buff_binary;
	int 					buff_int;
	ErlNifSInt64 			buff_int64;
	double 					buff_double;
	unsigned char 			buff_atom[MAX_ATOM_SIZE];
	unsigned char 			buff_number[MAX_NUMBER_LENGTH];
	const ERL_NIF_TERM*		buff_tuple;
	
	/*
	 * Buffers params
	 */
	size_t 				size_buff_output;
	size_t 				size_data_output;
	size_t 				size_buff_atom;
	int					size_buff_tuple;
	
	/*
	 * Special values
	 */
	ERL_NIF_TERM 		val_atom_true;
	ERL_NIF_TERM 		val_atom_false;
	ERL_NIF_TERM 		val_atom_null;
	ERL_NIF_TERM 		val_atom_obj;
	
} ctx;


/*
 * Construct a new context
 */
ctx*
ctx_new(ErlNifEnv* env) {
	ctx* ctx = enif_alloc(sizeof(ctx));
	ctx->buff_output = enif_alloc(DEFAULT_OUTPUT_BUFF_SIZE * sizeof(unsigned char));
	ctx->size_buff_output = DEFAULT_OUTPUT_BUFF_SIZE;
	ctx->size_data_output = 0;
	ctx->val_atom_true = enif_make_atom(env, "true");
	ctx->val_atom_false = enif_make_atom(env, "false");
	ctx->val_atom_null = enif_make_atom(env, "null");
	ctx->val_atom_obj = enif_make_atom(env, "obj");
	return ctx;
};


/*
 * Extend output buffer size
 */
void
ctx_extend(ctx* ctx) {
	enif_realloc(ctx->buff_output, ctx->size_buff_output * 2);
	ctx->size_buff_output *= 2;
}


/*
 * Destroy context structure
 */
void
ctx_free(ctx* ctx) {
	enif_free(ctx->buff_output);
	enif_free(ctx);
};


/*
 * Write string into output buffer
 */
void
ctx_write_str(ctx* ctx, unsigned char* str) {
	size_t size = strlen((char*)str);
	if(ctx->size_data_output + size < ctx->size_buff_output) {
		memcpy(&(ctx->buff_output[ctx->size_data_output]), str, size);
		ctx->size_data_output += size;
	} else {
		ctx_extend(ctx);
		ctx_write_str(ctx, str);
	}
}


/*
 * Write memory block into output buffer
 */
void
ctx_write_mem(ctx* ctx, unsigned char* mem, size_t size) {
	if(ctx->size_data_output + size < ctx->size_buff_output) {
		memcpy(&(ctx->buff_output[ctx->size_data_output]), mem, size);
		ctx->size_data_output += size;
	} else {
		ctx_extend(ctx);
		ctx_write_mem(ctx, mem, size);
	}
}


/*
 * Write char into output buffer
 */
void
ctx_write_ch(ctx* ctx, unsigned char ch) {
	if(ctx->size_data_output + 1 >= ctx->size_buff_output) ctx_extend(ctx);
	ctx->buff_output[ctx->size_data_output] = ch;
	++ ctx->size_data_output;
}


/*
 * Set value in output buffer
 */
void
ctx_set_ch(ctx* ctx, unsigned char ch) {
	ctx->buff_output[ctx->size_data_output - 1] = ch;
}


void
ctx_put_int(ctx* ctx) {
	sprintf((char*)ctx->buff_number,"%d", ctx->buff_int);
	ctx_write_str(ctx, ctx->buff_number);	
}

void
ctx_put_int64(ctx* ctx) {
	sprintf((char*)ctx->buff_number,"%lld", ctx->buff_int64);
	ctx_write_str(ctx, ctx->buff_number);	
}

void
ctx_put_double(ctx* ctx) {
	sprintf((char*)ctx->buff_number,"%f", ctx->buff_double);
	ctx_write_str(ctx, ctx->buff_number);	
}

void
ctx_put_binary(ctx* ctx) {
	ctx_write_mem(ctx, ctx->buff_binary.data, ctx->buff_binary.size);
}

void
ctx_put_binary_string(ctx* ctx) {
	ctx_write_ch(ctx, '"');
	ctx_put_binary(ctx);
	ctx_write_ch(ctx, '"');	
}

void ctx_put_true(ctx* ctx) { ctx_write_str(ctx, (unsigned char *)"true"); }
void ctx_put_false(ctx* ctx) { ctx_write_str(ctx, (unsigned char *)"false"); }
void ctx_put_null(ctx* ctx) { ctx_write_str(ctx, (unsigned char *)"null"); }

void
ctx_to_binary(ErlNifBinary* bin, ctx* ctx) {
	enif_alloc_binary(ctx->size_data_output, bin);
	memcpy(bin->data, ctx->buff_output, ctx->size_data_output);
}

void
ctx_put_atom(ctx* ctx) {
	ctx_write_ch(ctx, '"');
	ctx_write_mem(ctx, ctx->buff_atom, ctx->size_buff_atom - 1);
	ctx_write_ch(ctx, '"');	
}

/*
 ============================================================================
 Internal functions
 ============================================================================
 */
void encode_term(ErlNifEnv* env, ERL_NIF_TERM term, ctx* ctx);
void encode_list(ErlNifEnv* env, ERL_NIF_TERM list, ctx* ctx);
void encode_obj(ErlNifEnv* env, ctx* ctx);
void encode_field(ErlNifEnv* env, ctx* ctx, ERL_NIF_TERM field_tuple);

/*
 ============================================================================
 Internal functions
 ============================================================================
 */
void
encode_list(ErlNifEnv* env, ERL_NIF_TERM list, ctx* ctx) {
	ERL_NIF_TERM head, tail;
	ctx_write_ch(ctx, '[');	
	while(enif_get_list_cell(env, list, &head, &tail)) {
		encode_term(env, head, ctx);
		ctx_write_ch(ctx, ',');	
		list = tail;
	};
	ctx_set_ch(ctx, ']');	
}

void
encode_obj(ErlNifEnv* env, ctx* ctx) {
	ERL_NIF_TERM fields = ctx->buff_tuple[1];
	ERL_NIF_TERM h_pair, t_pairs;
	ctx_write_ch(ctx, '{');	
	while(enif_get_list_cell(env, fields, &h_pair, &t_pairs)) {
		encode_field(env, ctx, h_pair);
		ctx_write_ch(ctx, ',');	
		fields = t_pairs;
	};
	ctx_set_ch(ctx, '}');	
}

void
encode_field(ErlNifEnv* env, ctx* ctx, ERL_NIF_TERM field_tuple) {
	const ERL_NIF_TERM * pair;
	int size;
	enif_get_tuple(env, field_tuple, &size, &pair);
	ERL_NIF_TERM name = pair[0];
	ERL_NIF_TERM value = pair[1];
	encode_term(env, name, ctx);
	ctx_write_ch(ctx, ':');
	encode_term(env, value, ctx);
}

void
encode_tuple(ErlNifEnv* env, ctx* ctx) {
	encode_obj(env, ctx);
}

void
encode_term(ErlNifEnv* env, ERL_NIF_TERM term, ctx* ctx) {
	
	if(enif_is_list(env, term)) encode_list(env, term, ctx);
	else if(enif_get_tuple(env, term, &ctx->size_buff_tuple, &ctx->buff_tuple)) encode_tuple(env, ctx);
	else {
		
		if(enif_get_double(env, term, &ctx->buff_double)) ctx_put_double(ctx);
		else if(enif_get_int(env, term, &ctx->buff_int)) ctx_put_int(ctx);
		else if(enif_get_int64(env, term, &ctx->buff_int64)) ctx_put_int64(ctx);
		
		else if(enif_inspect_binary(env, term, &ctx->buff_binary)) ctx_put_binary_string(ctx);
		
		else if(enif_is_identical(term, ctx->val_atom_true)) ctx_put_true(ctx);
		else if(enif_is_identical(term, ctx->val_atom_false)) ctx_put_false(ctx);
		else if(enif_is_identical(term, ctx->val_atom_null)) ctx_put_null(ctx);
		
		else {
			ctx->size_buff_atom = enif_get_atom(env, term, (char*)ctx->buff_atom, MAX_ATOM_SIZE, ERL_NIF_LATIN1);
			if(ctx->size_buff_atom) ctx_put_atom(ctx);
		}
	}
}

/*
 ============================================================================
 NIF API
 ============================================================================
 */

static ERL_NIF_TERM encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	
	ctx * ctx = ctx_new(env);
	
	encode_term(env, argv[0], ctx);
	
	ErlNifBinary bin;
	
	ctx_to_binary(&bin, ctx);
	
	ctx_free(ctx);
	
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

ERL_NIF_INIT(feje, nif_funcs, load, reload, upgrade, unload)