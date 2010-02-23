#include <stdlib.h>
#include <assert.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include "codebuf.h"

/*
 * Custom block for the struct codebuf
 */

static void custom_codebuf_finalize(value v)
{
	CAMLparam1(v);
	assert(Tag_val(v) == Custom_tag);

	codebuf_dtor(Data_custom_val(v));
	
	CAMLreturn0;
}

static int custom_codebuf_compare(value v1, value v2)
{
	CAMLparam2(v1, v2);
	assert(Tag_val(v1) == Custom_tag);
	assert(Tag_val(v2) == Custom_tag);
	struct codebuf *a = Data_custom_val(v1);
	struct codebuf *b = Data_custom_val(v2);

	int res = 0;
	if (a->addr < b->addr) {
		res = -1;
	} else if (a->addr > b->addr) {
		res = 1;
	} else /* same addr */ if (a->length < b->length) {
		res = -1;
	} else if (a->length > b->length) {
		res = 1;
	}
	
	CAMLreturn(Val_int(res));
}

static value codebuf_new(size_t length, char const *filename)
{
	CAMLparam0();
	CAMLlocal1(val);

	static struct custom_operations custom_codebuf_ops = {
		.identifier  = "org.happyleptic.jiton.codebuf.1",	// FIXME: use VERSION
		.finalize    = custom_codebuf_finalize,
		.compare     = custom_codebuf_compare,
		.hash        = custom_hash_default,
		.serialize   = custom_serialize_default,
		.deserialize = custom_deserialize_default,
	};
	
	val = caml_alloc_custom(&custom_codebuf_ops, sizeof(struct codebuf), length, 1000000);
	
	if (0 != codebuf_ctor(Data_custom_val(val), length, filename)) {
		assert(0);	// FIXME
	}

	CAMLreturn(val);
}

/*
 * Wrappers to codebuf functions
 */

CAMLprim value wrap_make_buffer(value size, value filename)
{
	CAMLparam2(size, filename);
	CAMLlocal1(buffer);

	assert(Is_long(size));
	assert(Tag_val(filename) == String_tag);

	char const *const fname = String_val(filename);
	buffer = codebuf_new(Long_val(size), fname[0] != '\0' ? fname:NULL);

	CAMLreturn(buffer);
}

CAMLprim void wrap_poke_byte(value buffer, value offset, value byte)
{
	CAMLparam3(buffer, offset, byte);
	assert(Tag_val(buffer) == Custom_tag);
	assert(Is_long(offset));
	assert(Is_long(byte));

	codebuf_poke(Data_custom_val(buffer), Long_val(offset), Long_val(byte));

	CAMLreturn0;
}

CAMLprim void wrap_exec_buffer(value buffer, value offset, value params)
{
	CAMLparam3(buffer, offset, params);
	assert(Tag_val(buffer) == Custom_tag);
	assert(Is_long(offset));
	assert(Is_block(params));

	// TODO: params
	codebuf_exec(Data_custom_val(buffer), Long_val(offset), 0, NULL);

	CAMLreturn0;
}
