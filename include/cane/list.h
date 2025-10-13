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

#define CANE_VEC_CAP 0x10

typedef struct cane_vec cane_vec_t;

struct cane_vec {
	size_t cap;
	size_t len;
	uint8_t* buf;
};

static cane_vec_t cane_vec_create(void) {
	cane_vec_t vec;

	vec.cap = CANE_VEC_CAP;
	vec.len = 0;
	vec.buf = cane_xalloc(vec.cap);

	return vec;
}

static void cane_vec_free(cane_vec_t* vec) {
	free(vec->buf);
}

static cane_vec_t* cane_vec_fit(cane_vec_t* vec, size_t new_len) {
	// vec->cap += vec->cap / 2;
	// cane_xrealloc(vec->buf, vec->cap);

	size_t new_cap = vec->cap;

	if (new_len >= vec->cap) {
		while (new_cap < new_len) {
			new_cap += new_cap / 2;
			// new_cap *= 2;
		}
		vec->buf = cane_xrealloc(vec->buf, new_cap);
		memset(vec->buf + vec->cap, 0, new_cap - vec->cap);
		vec->cap = new_cap;
	}

	return vec;
}

static void cane_vec_push(cane_vec_t* vec, uint8_t val) {
	cane_vec_fit(vec, vec->len + 1);
	vec->buf[vec->len++] = val;
}

static uint8_t cane_vec_pop(cane_vec_t* vec) {
	return vec->buf[--vec->len];
}

// return ptr to lhs
static cane_vec_t* cane_vec_cat(cane_vec_t* lhs, cane_vec_t* rhs) {
	cane_vec_fit(lhs, lhs->len + rhs->len);
	memcpy(lhs->buf + lhs->len, rhs->buf, rhs->len);
	lhs->len += rhs->len;
	return lhs;
}

static uint8_t* cane_vec_insert(cane_vec_t* vec, size_t pos, uint8_t val) {
	cane_vec_fit(vec, vec->len + 1);

	memmove(vec->buf + pos + 1, vec->buf + pos, vec->len - pos);
	vec->buf[pos] = val;

	vec->len += 1;

	return vec->buf + pos;
}

static uint8_t*
cane_vec_insert_buf(cane_vec_t* vec, size_t pos, uint8_t* buf, size_t len) {
	// CANE_ASSERT((buf == NULL), "buf is NULL");
	// CANE_ASSERT(pos <= vec->len, "pos outwith buf");

	if (len == 0) {
		return vec->buf + pos;
	}

	cane_vec_fit(vec, pos);
	cane_vec_fit(vec, vec->len + len);

	memmove(vec->buf + pos + len, vec->buf + pos, vec->len - pos);
	memcpy(vec->buf + pos, buf, len);

	vec->len += len;

	return vec->buf + pos;
}

static uint8_t* cane_vec_remove(cane_vec_t* vec, size_t pos) {
	if (pos >= vec->len) {
		return NULL;
	}

	memmove(vec->buf + pos, vec->buf + pos + 1, vec->len - 1);
	vec->len--;

	return vec->buf + pos;
}

static uint8_t* cane_vec_remove_span(cane_vec_t* vec, size_t) {}

// TODO: implement

#endif
