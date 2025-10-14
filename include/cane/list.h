#ifndef CANE_LIST_H
#define CANE_LIST_H

#include "cane/util.h"
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// TODO:
// - create
// - head/tail
// - first/last
// - insert
// - delete
// - append
// - push/pop
// - reverse
// - transpose (broadcast): add/sub/mul/div/or/and/xor
// - repeat
// - rotate left/right
// - invert

#define CANE_VEC_CAP 16

typedef struct cane_vector cane_vector_t;

struct cane_vector {
	size_t capacity;
	size_t length;

	uint8_t* data;
};

static cane_vector_t cane_vector_create(void) {
	cane_vector_t vec;

	vec.capacity = CANE_VEC_CAP;
	vec.length = 0;

	vec.data = cane_allocate(vec.capacity);

	return vec;
}

static void cane_vector_free(cane_vector_t* vec) {
	free(vec->data);
}

static cane_vector_t* cane_vector_fit(cane_vector_t* vec, size_t new_len) {
	// vec->cap += vec->cap / 2;
	// cane_xrealloc(vec->buf, vec->cap);

	size_t new_cap = vec->capacity;

	if (new_len >= vec->capacity) {
		while (new_cap < new_len) {
			new_cap += new_cap / 2;
			// new_cap *= 2;
		}
		vec->data = cane_reallocate(vec->data, new_cap);
		memset(vec->data + vec->capacity, 0, new_cap - vec->capacity);
		vec->capacity = new_cap;
	}

	return vec;
}

static void cane_vector_push(cane_vector_t* vec, uint8_t val) {
	cane_vector_fit(vec, vec->length + 1);
	vec->data[vec->length++] = val;
}

static uint8_t cane_vector_pop(cane_vector_t* vec) {
	return vec->data[--vec->length];
}

// return ptr to lhs
static cane_vector_t* cane_vector_cat(cane_vector_t* lhs, cane_vector_t* rhs) {
	cane_vector_fit(lhs, lhs->length + rhs->length);
	memcpy(lhs->data + lhs->length, rhs->data, rhs->length);
	lhs->length += rhs->length;
	return lhs;
}

static uint8_t*
cane_vector_insert(cane_vector_t* vec, size_t pos, uint8_t val) {
	cane_vector_fit(vec, vec->length + 1);

	memmove(vec->data + pos + 1, vec->data + pos, vec->length - pos);
	vec->data[pos] = val;

	vec->length += 1;

	return vec->data + pos;
}

static uint8_t* cane_vector_insert_buf(
	cane_vector_t* vec, size_t pos, uint8_t* buf, size_t len
) {
	// CANE_ASSERT((buf == NULL), "buf is NULL");
	// CANE_ASSERT(pos <= vec->len, "pos outwith buf");

	if (len == 0) {
		return vec->data + pos;
	}

	cane_vector_fit(vec, pos);
	cane_vector_fit(vec, vec->length + len);

	memmove(vec->data + pos + len, vec->data + pos, vec->length - pos);
	memcpy(vec->data + pos, buf, len);

	vec->length += len;

	return vec->data + pos;
}

static uint8_t* cane_vector_remove(cane_vector_t* vec, size_t pos) {
	if (pos >= vec->length) {
		return NULL;
	}

	memmove(vec->data + pos, vec->data + pos + 1, vec->length - 1);
	vec->length--;

	return vec->data + pos;
}

static uint8_t*
cane_vector_remove_span(cane_vector_t* vec, size_t pos, size_t len) {
	CANE_UNUSED(vec, pos, len);
	return NULL;
}

#endif
