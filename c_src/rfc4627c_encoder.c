/* rfc4627c_encoder.c - Erlang JSON encoder
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

#include "rfc4627c_encoder.h"

static void enc_realloc_buff(JsonEncoder* enc);

static void enc_buff_write_string(JsonEncoder* enc, unsigned char* str);
static void enc_buff_write(JsonEncoder* enc, unsigned char* mem, size_t size);
static void enc_buff_write_with_escape(JsonEncoder* enc, unsigned char* mem, size_t size);
static void enc_buff_write_ch(JsonEncoder* enc, unsigned char ch);
static void enc_buff_set_last(JsonEncoder* enc, unsigned char ch);

static void enc_flush_buff_int(JsonEncoder* enc);
static void enc_flush_buff_int64(JsonEncoder* enc);
static void enc_flush_buff_double(JsonEncoder* enc);
static void enc_flush_buff_binary(JsonEncoder* enc);
static void enc_flush_buff_binary_string(JsonEncoder* enc);
static void enc_flush_buff_true(JsonEncoder* enc);
static void enc_flush_buff_false(JsonEncoder* enc);
static void enc_flush_buff_null(JsonEncoder* enc);
static void enc_flush_buff_atom(JsonEncoder* enc);

// TODO(vladimir@zvm.me): write error checking where it is appropriate

//////////////////////////////////////////////////////////////////////////////
/// JsonEncoder struct manipulation funcitons
//////////////////////////////////////////////////////////////////////////////

// Initialize JsonEncoder
JsonEncoder*
encoder_init(ErlNifEnv* env, JsonEncoder* enc) {
	enc->size_buff_output = INIT_OUTPUT_BUFF_SIZE;
	enc->buff_output = MALLOC(enc->size_buff_output * sizeof(unsigned char));
	enc->size_data_output = 0;
	enc->val_atom_true = enif_make_atom(env, "true");
	enc->val_atom_false = enif_make_atom(env, "false");
	enc->val_atom_null = enif_make_atom(env, "null");
	enc->val_atom_obj = enif_make_atom(env, "obj");
	return enc;
};

// Release JsonEncoder
void
encoder_release(JsonEncoder* enc) {
	FREE(enc->buff_output);
}

// Copy JsonEncoder output buffer to ErlNifBinary
void
encoder_to_binary(ErlNifBinary* bin, JsonEncoder* enc) {
	enif_alloc_binary(enc->size_data_output, bin);
	memcpy(bin->data, enc->buff_output, enc->size_data_output);
}

//////////////////////////////////////////////////////////////////////////////
/// JsonEncoder 'public' routine functions
//////////////////////////////////////////////////////////////////////////////

// Encode Erlang term to JSON term
// returns 0 if an error occured
int
encode_term(ErlNifEnv* env, ERL_NIF_TERM term, JsonEncoder* enc) {
	if(enif_is_list(env, term))
		return encode_list_as_array(env, term, enc);
	if(enif_is_tuple(env, term))
		return encode_tuple(env, term, enc);
	if(enif_is_atom(env, term))
		return encode_atom(env, term, enc);
	if(enif_inspect_binary(env, term, &enc->buff_binary))
		return encode_binary(env, term, enc);
	return encode_number(env, term, enc);
}

// Encode Erlang integer or float to JSON number
// returns 0 if an error occured
int
encode_number(ErlNifEnv* env, ERL_NIF_TERM number, JsonEncoder* enc) {
	if(enif_get_int(env, number, &enc->buff_int)) {
		enc_flush_buff_int(enc);
		return 1;
	}
	if(enif_get_double(env, number, &enc->buff_double)) {
		enc_flush_buff_double(enc);
		return 1;
	}
	if(enif_get_int64(env, number, &enc->buff_int64)) {
		enc_flush_buff_int64(enc);
		return 1;
	}
	return 0;
}

// Encode Erlang atom to JSON
// 		true 		->		true
// 		false 		->		false
// 		null 		->		null
// 		otherwise 	->		"atom name"
// returns 0 if an error occured
int
encode_atom(ErlNifEnv* env, ERL_NIF_TERM atom, JsonEncoder* enc) {
	if(enif_is_identical(atom, enc->val_atom_true)) {
		enc_flush_buff_true(enc);
		return 1;
	}
	if(enif_is_identical(atom, enc->val_atom_false)) {
		enc_flush_buff_false(enc);
		return 1;
	}
	if(enif_is_identical(atom, enc->val_atom_null)) {
		enc_flush_buff_null(enc);
		return 1;
	}
	return encode_atom_str(env, atom, enc);
}

// Encode Erlang atom to JSON string
int
encode_atom_str(ErlNifEnv* env, ERL_NIF_TERM atom, JsonEncoder* enc) {
	enc->size_buff_atom = enif_get_atom(env,
		atom, (char*)enc->buff_atom,
		MAX_NUMBER_LENGTH, ERL_NIF_LATIN1);
	if(!enc->size_buff_atom) return 0;
	enc_flush_buff_atom(enc);
	return 1;
}

// Encode Erlang tuple(must be {obj, [Term]}) to JSON object
// returns 0 if error occured
int
encode_tuple(ErlNifEnv* env, ERL_NIF_TERM tuple, JsonEncoder* enc) {
	enif_get_tuple(env, tuple, &enc->size_buff_tuple, &enc->buff_tuple);
	if(!enif_is_identical(enc->buff_tuple[0], enc->val_atom_obj)) return 0;
	if(enc->size_buff_tuple < 2) return 0;
	
	const ERL_NIF_TERM * head_content;
	ERL_NIF_TERM head;
	ERL_NIF_TERM tail;
	ERL_NIF_TERM obj_fields;
	obj_fields = enc->buff_tuple[1];
	// cell_counter is the counter of fields in the object
	size_t cell_counter = 0;

	enc_buff_write_ch(enc, '{');	
	while(enif_get_list_cell(env, obj_fields, &head, &tail)) {

		int head_size;

		// head is the current field of the object
		if(!enif_get_tuple(env, head, &head_size, &head_content)) return 0;
		if(head_size != 2) return 0;
		ERL_NIF_TERM key = head_content[0];
		ERL_NIF_TERM value = head_content[1];
		
		// key must be an atom, list or binary
		if(enif_is_list(env, key))
			encode_list_as_string(env, key, enc);
		else if(enif_inspect_binary(env, key, &enc->buff_binary))
			enc_flush_buff_binary_string(enc);
		else if(enif_is_atom(env, key))
			encode_atom_str(env, key, enc);
		else return 0;
				
		enc_buff_write_ch(enc, ':');
		encode_term(env, value, enc);
		enc_buff_write_ch(enc, ',');	
		obj_fields = tail;
		++ cell_counter;
	}
	(cell_counter ? enc_buff_set_last : enc_buff_write_ch)(enc, '}');
	return 1;
}

// Encode Erlang list of terms to JSON array,
// returns 0 if error occured
int
encode_list_as_array(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc) {
	ERL_NIF_TERM head, tail;
	size_t list_length = 0;
	enc_buff_write_ch(enc, '[');
	while(enif_get_list_cell(env, list, &head, &tail)) {
		if(!encode_term(env, head, enc)) return 0;
		enc_buff_write_ch(enc, ',');	
		list = tail;
		++ list_length;
	}
	(list_length ? enc_buff_set_last : enc_buff_write_ch)(enc, ']');
	return 1;
}

// Encode Erlang list of integers to JSON string,
// returns 0 if error occured
int
encode_list_as_string(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc) {
	ERL_NIF_TERM head, tail;
	enc_buff_write_ch(enc, '\"');
	while(enif_get_list_cell(env, list, &head, &tail)) {
		enif_get_int(env, head, &enc->buff_int);
		enc_buff_write_ch(enc, (unsigned char)enc->buff_int);
		list = tail;
	}
	enc_buff_write_ch(enc, '\"');
	return 1;
}

// Encode Erlang binary to JSON string,
// returns 0 if error occured
int
encode_binary(ErlNifEnv* env, ERL_NIF_TERM binary, JsonEncoder* enc) {
	enc_flush_buff_binary_string(enc);
	return 1;
}


//////////////////////////////////////////////////////////////////////////////
/// JsonEncoder internal functions
//////////////////////////////////////////////////////////////////////////////

static void
enc_realloc_buff(JsonEncoder* enc) {
	enc->size_buff_output *= OUTPUT_BUFF_CAP_KOEFF;
	enc->buff_output = REALLOC(enc->buff_output, enc->size_buff_output * sizeof(unsigned char));
}

static void
enc_buff_write_string(JsonEncoder* enc, unsigned char* str) {
	size_t size = strlen((char*)str);
	if(enc->size_data_output + size < enc->size_buff_output) {
		memcpy(&(enc->buff_output[enc->size_data_output]), str, size);
		enc->size_data_output += size;
	} else {
		enc_realloc_buff(enc);
		enc_buff_write_string(enc, str);
	}
}

static void
enc_buff_write(JsonEncoder* enc, unsigned char* mem, size_t size) {
	if(enc->size_data_output + size < enc->size_buff_output) {
		memcpy(&(enc->buff_output[enc->size_data_output]), mem, size);
		enc->size_data_output += size;
	} else {
		enc_realloc_buff(enc);
		enc_buff_write(enc, mem, size);
	}
}

static void
enc_buff_write_with_escape(JsonEncoder* enc, unsigned char* mem, size_t size) {
	unsigned char * src_head = mem;
	size_t esc_block_size = 0;
	int i;
	for(i = 0; i < size; ++i) {
		++ esc_block_size;
		if(IS_ESCAPE_CHAR(mem[i])) {
			enc_buff_write(enc, src_head, esc_block_size - 1);
			enc_buff_write_ch(enc, '\\');
			enc_buff_write_ch(enc, mem[i]);
			src_head = &(mem[i + 1]);
			esc_block_size = 0;
		}
	}
	enc_buff_write(enc, src_head, esc_block_size);
}

static void
enc_buff_write_ch(JsonEncoder* enc, unsigned char ch) {
	if(enc->size_data_output + 1 >= enc->size_buff_output)
		enc_realloc_buff(enc);
	enc->buff_output[enc->size_data_output] = ch;
	++ enc->size_data_output;
}

static void
enc_buff_set_last(JsonEncoder* enc, unsigned char ch) {
	enc->buff_output[enc->size_data_output - 1] = ch;
}

static void
enc_flush_buff_int(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%d", enc->buff_int);
	enc_buff_write_string(enc, enc->buff_number);
}

static void
enc_flush_buff_int64(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%lld", enc->buff_int64);
	enc_buff_write_string(enc, enc->buff_number);
}

static void
enc_flush_buff_double(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%.20e", enc->buff_double);
	enc_buff_write_string(enc, enc->buff_number);
}

static void
enc_flush_buff_binary(JsonEncoder* enc) {
	enc_buff_write_with_escape(enc, enc->buff_binary.data, enc->buff_binary.size);
}

static void
enc_flush_buff_binary_string(JsonEncoder* enc) {
	enc_buff_write_ch(enc, '"');
	enc_flush_buff_binary(enc);
	enc_buff_write_ch(enc, '"');	
}

static void
enc_flush_buff_true(JsonEncoder* enc) {
	enc_buff_write_string(enc, (unsigned char *)"true");
}

static void
enc_flush_buff_false(JsonEncoder* enc) {
	enc_buff_write_string(enc, (unsigned char *)"false");
}

static void
enc_flush_buff_null(JsonEncoder* enc) {
	enc_buff_write_string(enc, (unsigned char *)"null");
}

static void
enc_flush_buff_atom(JsonEncoder* enc) {
	enc_buff_write_ch(enc, '"');
	enc_buff_write(enc, enc->buff_atom, enc->size_buff_atom - 1);
	enc_buff_write_ch(enc, '"');
}