#include "rfc4627c_encoder.h"

/*
 * Construct a new context
 */
JsonEncoder*
enc_new(ErlNifEnv* env) {
	JsonEncoder* enc = MALLOC(sizeof(JsonEncoder));
	enc->size_buff_output = DEFAULT_OUTPUT_BUFF_SIZE;
	enc->buff_output = MALLOC(DEFAULT_OUTPUT_BUFF_SIZE * sizeof(unsigned char));
	enc->size_data_output = 0;
	enc->val_atom_true = enif_make_atom(env, "true");
	enc->val_atom_false = enif_make_atom(env, "false");
	enc->val_atom_null = enif_make_atom(env, "null");
	enc->val_atom_obj = enif_make_atom(env, "obj");
	return enc;
};


/*
 * Extend output buffer size
 */
void
enc_extend(JsonEncoder* enc) {
	enc->size_buff_output *= OUTPUT_BUFFER_CAPACITY_KOEFFICIENT;
	enc->buff_output = REALLOC(enc->buff_output, enc->size_buff_output * sizeof(unsigned char));
}


/*
 * Destroy context structure
 */
void
enc_free(JsonEncoder* enc) {
	FREE(enc->buff_output);
	FREE(enc);
};


void
encode_term(ErlNifEnv* env, ERL_NIF_TERM term, JsonEncoder* enc) {
	if(enif_is_list(env, term))
		encode_list(env, term, enc);
	else
		if(enif_get_tuple(env, term, &enc->size_buff_tuple, &enc->buff_tuple))
			encode_tuple(env, enc);
				else {
					if(enif_get_double(env, term, &enc->buff_double)) enc_put_double(enc);
					else if(enif_get_int(env, term, &enc->buff_int)) enc_put_int(enc);
					else if(enif_get_int64(env, term, &enc->buff_int64)) enc_put_int64(enc);
					else if(enif_inspect_binary(env, term, &enc->buff_binary)) enc_put_binary_string(enc);
					else if(enif_is_identical(term, enc->val_atom_true)) enc_put_true(enc);
					else if(enif_is_identical(term, enc->val_atom_false)) enc_put_false(enc);
					else if(enif_is_identical(term, enc->val_atom_null)) enc_put_null(enc);
					else {
						enc->size_buff_atom = enif_get_atom(env,
												term,
												(char*)enc->buff_atom,
												MAX_NUMBER_LENGTH,
												ERL_NIF_LATIN1);
												
						if(enc->size_buff_atom) enc_put_atom(enc);
					}
				}
}

void
encode_list(ErlNifEnv* env, ERL_NIF_TERM list, JsonEncoder* enc) {
	ERL_NIF_TERM head, tail;
	enc_write_ch(enc, '[');	
	while(enif_get_list_cell(env, list, &head, &tail)) {
		encode_term(env, head, enc);
		enc_write_ch(enc, ',');	
		list = tail;
	}
	enc_set_ch(enc, ']');	
}

void
encode_obj(ErlNifEnv* env, JsonEncoder* enc) {
	ERL_NIF_TERM fields = enc->buff_tuple[1];
	ERL_NIF_TERM h_pair, t_pairs;
	enc_write_ch(enc, '{');	
	while(enif_get_list_cell(env, fields, &h_pair, &t_pairs)) {
		encode_field(env, enc, h_pair);
		enc_write_ch(enc, ',');	
		fields = t_pairs;
	}
	enc_set_ch(enc, '}');	
}

void
encode_field(ErlNifEnv* env, JsonEncoder* enc, ERL_NIF_TERM field_tuple) {
	const ERL_NIF_TERM * pair;
	int size;
	enif_get_tuple(env, field_tuple, &size, &pair);
	ERL_NIF_TERM name = pair[0];
	ERL_NIF_TERM value = pair[1];
	encode_term(env, name, enc);
	enc_write_ch(enc, ':');
	encode_term(env, value, enc);
}

void
encode_tuple(ErlNifEnv* env, JsonEncoder* enc) {
	encode_obj(env, enc);
}





/*
 * Write string into output buffer
 */
void
enc_write_str(JsonEncoder* enc, unsigned char* str) {
	size_t size = strlen((char*)str);
	if(enc->size_data_output + size < enc->size_buff_output) {
		memcpy(&(enc->buff_output[enc->size_data_output]), str, size);
		enc->size_data_output += size;
	} else {
		enc_extend(enc);
		enc_write_str(enc, str);
	}
}


/*
 * Write memory block into output buffer
 */
void
enc_write_mem(JsonEncoder* enc, unsigned char* mem, size_t size) {
	if(enc->size_data_output + size < enc->size_buff_output) {
		memcpy(&(enc->buff_output[enc->size_data_output]), mem, size);
		enc->size_data_output += size;
	} else {
		enc_extend(enc);
		enc_write_mem(enc, mem, size);
	}
}


/*
 * Write char into output buffer
 */
void
enc_write_ch(JsonEncoder* enc, unsigned char ch) {
	if(enc->size_data_output + 1 >= enc->size_buff_output) enc_extend(enc);
	enc->buff_output[enc->size_data_output] = ch;
	++ enc->size_data_output;
}


/*
 * Set value in output buffer
 */
void
enc_set_ch(JsonEncoder* enc, unsigned char ch) {
	enc->buff_output[enc->size_data_output - 1] = ch;
}


void
enc_put_int(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%d", enc->buff_int);
	enc_write_str(enc, enc->buff_number);	
}

void
enc_put_int64(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%lld", enc->buff_int64);
	enc_write_str(enc, enc->buff_number);	
}

void
enc_put_double(JsonEncoder* enc) {
	sprintf((char*)enc->buff_number,"%f", enc->buff_double);
	enc_write_str(enc, enc->buff_number);	
}

void
enc_put_binary(JsonEncoder* enc) {
	enc_write_mem(enc, enc->buff_binary.data, enc->buff_binary.size);
}

void
enc_put_binary_string(JsonEncoder* enc) {
	enc_write_ch(enc, '"');
	enc_put_binary(enc);
	enc_write_ch(enc, '"');	
}

void
enc_put_true(JsonEncoder* enc) { enc_write_str(enc, (unsigned char *)"true"); }

void
enc_put_false(JsonEncoder* enc) { enc_write_str(enc, (unsigned char *)"false"); }

void
enc_put_null(JsonEncoder* enc) { enc_write_str(enc, (unsigned char *)"null"); }

void
enc_to_binary(ErlNifBinary* bin, JsonEncoder* enc) {
	enif_alloc_binary(enc->size_data_output, bin);
	memcpy(bin->data, enc->buff_output, enc->size_data_output);
}

void
enc_put_atom(JsonEncoder* enc) {
	enc_write_ch(enc, '"');
	enc_write_mem(enc, enc->buff_atom, enc->size_buff_atom - 1);
	enc_write_ch(enc, '"');	
}