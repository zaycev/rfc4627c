#ifndef	ENCODER_H
#define	ENCODER_H

#include "erl_nif.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define DEFAULT_OUTPUT_BUFF_SIZE 512 // must be >= 2
#define OUTPUT_BUFFER_CAPACITY_KOEFFICIENT 1.6
#define MAX_ATOM_NAME_LENGTH 64
#define MAX_NUMBER_LENGTH 64

#define MALLOC enif_alloc
#define REALLOC enif_realloc
#define FREE enif_free

/*
 * JsonEncoder - simple encoder state container
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
	unsigned char 			buff_atom[MAX_ATOM_NAME_LENGTH];
	unsigned char 			buff_number[MAX_NUMBER_LENGTH];
	const ERL_NIF_TERM*		buff_tuple;
	
	/*
	 * Buffers params
	 */
	size_t 					size_buff_output;
	size_t 					size_data_output;
	size_t 					size_buff_atom;
	int						size_buff_tuple;
	
	/*
	 * Special values
	 */
	ERL_NIF_TERM 			val_atom_true;
	ERL_NIF_TERM 			val_atom_false;
	ERL_NIF_TERM 			val_atom_null;
	ERL_NIF_TERM 			val_atom_obj;
	
} JsonEncoder;

JsonEncoder* enc_new(ErlNifEnv* env);
void enc_free(JsonEncoder* enc);
void enc_extend(JsonEncoder* enc);
void enc_write_str(JsonEncoder* enc, unsigned char* str);
void enc_write_mem(JsonEncoder* enc, unsigned char* mem, size_t size);
void enc_write_mem_escape(JsonEncoder* enc, unsigned char* mem, size_t size);
void enc_write_ch(JsonEncoder* enc, unsigned char ch);
void enc_set_ch(JsonEncoder* enc, unsigned char ch);
void enc_put_int(JsonEncoder* enc);
void enc_put_int64(JsonEncoder* enc);
void enc_put_double(JsonEncoder* enc);
void enc_put_binary(JsonEncoder* enc);
void enc_put_binary_string(JsonEncoder* enc);
void enc_put_true(JsonEncoder* enc);
void enc_put_false(JsonEncoder* enc);
void enc_put_null(JsonEncoder* enc);
void enc_to_binary(ErlNifBinary* bin, JsonEncoder* enc);
void enc_put_atom(JsonEncoder* enc);

void encode_term(ErlNifEnv* env, ERL_NIF_TERM term, JsonEncoder* enc);
void encode_list(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc);
void encode_obj(ErlNifEnv* env, JsonEncoder* enc);
void encode_field(ErlNifEnv* env, JsonEncoder* enc, ERL_NIF_TERM field_tuple);
void encode_tuple(ErlNifEnv* env, JsonEncoder* enc);

#endif	/* ENCODER_H */