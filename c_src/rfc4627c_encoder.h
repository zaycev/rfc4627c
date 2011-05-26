/* rfc4627c_encoder.h - Erlang JSON encoder
 * Author Vladimir Zaytsev <vladimir@zvm.me>
 * Copyright (C) 2011 Vladimir Zaytsev
 * License
 * Permission is hereby granted, free of charge, to any author
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit authors to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef	ENCODER_H
#define	ENCODER_H

#include "erl_nif.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// NOTE(vladimir@zvm.me): INIT_OUTPUT_BUFF_SIZE * OUTPUT_BUFF_CAP_KOEFF must be >= 2
#define INIT_OUTPUT_BUFF_SIZE			512
#define OUTPUT_BUFF_CAP_KOEFF			1.6
#define MAX_ATOM_NAME_LENGTH 			64
#define MAX_NUMBER_LENGTH 				64

#define MALLOC							enif_alloc
#define REALLOC							enif_realloc
#define FREE							enif_free

#define IS_ESCAPE_CHAR(ch)				ch == '\\' || ch == '\"'

// JsonEncoder - simple encoder state container
typedef struct {
	unsigned char*	 		buff_output;
	ErlNifBinary 			buff_binary;
	int 					buff_int;
	ErlNifSInt64 			buff_int64;
	double 					buff_double;
	unsigned char 			buff_atom[MAX_ATOM_NAME_LENGTH];
	unsigned char 			buff_number[MAX_NUMBER_LENGTH];
	const ERL_NIF_TERM*		buff_tuple;
	
	size_t 					size_buff_output;
	size_t 					size_data_output;
	size_t 					size_buff_atom;
	int						size_buff_tuple;
	
	ERL_NIF_TERM 			val_atom_true;
	ERL_NIF_TERM 			val_atom_false;
	ERL_NIF_TERM 			val_atom_null;
	ERL_NIF_TERM 			val_atom_obj;
	
} JsonEncoder;

//////////////////////////////////////////////////////////////////////////////
/// JsonEncoder struct manipulation funcitons
//////////////////////////////////////////////////////////////////////////////
JsonEncoder*	encoder_init(ErlNifEnv* env, JsonEncoder* enc);
void			encoder_release(JsonEncoder* enc);
void			encoder_to_binary(ErlNifBinary* bin, JsonEncoder* enc);

//////////////////////////////////////////////////////////////////////////////
/// JsonEncoder 'public' routine functions
//////////////////////////////////////////////////////////////////////////////
int encode_term(ErlNifEnv* env, ERL_NIF_TERM term, JsonEncoder* enc);
int encode_list_as_array(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc);
int encode_list_as_string(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc);
int encode_atom(ErlNifEnv* env, ERL_NIF_TERM atom, JsonEncoder* enc);
int encode_atom_str(ErlNifEnv* env, ERL_NIF_TERM atom, JsonEncoder* enc);
int encode_tuple(ErlNifEnv* env, ERL_NIF_TERM tuple, JsonEncoder* enc);
int encode_binary(ErlNifEnv* env, ERL_NIF_TERM binary, JsonEncoder* enc);
int encode_number(ErlNifEnv* env, ERL_NIF_TERM number, JsonEncoder* enc);

#endif	/* ENCODER_H */