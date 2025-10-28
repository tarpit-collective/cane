#ifndef CANE_LIST_H
#define CANE_LIST_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include <cane/util.h>

#define CANE_VECTOR_DEFAULT_CAPACITY 16

typedef struct cane_vector cane_vector_t;

typedef uint8_t (*cane_broadcast_binary_t)(uint8_t, uint8_t);
typedef uint8_t (*cane_broadcast_unary_t)(uint8_t);

struct cane_vector {
	size_t capacity;
	size_t length;

	uint8_t* data;
};

typedef struct cane_vector_info cane_vector_info_t;

struct cane_vector_info {
	uint8_t* ptr;
	size_t length;
};

static cane_vector_t cane_vector_create(void) {
	cane_vector_t vec;

	vec.capacity = CANE_VECTOR_DEFAULT_CAPACITY;
	vec.length = 0;

	vec.data = cane_allocate(vec.capacity);

	return vec;
}

static void cane_vector_free(cane_vector_t* vec) {
	free(vec->data);
}

static size_t cane_vector_length(cane_vector_t* vec) {
	return vec->length;
}

static size_t cane_vector_capacity(cane_vector_t* vec) {
	return vec->capacity;
}

static bool cane_vector_is_empty(cane_vector_t* vec) {
	return vec->length == 0;
}

static uint8_t* cane_vector_at(cane_vector_t* vec, size_t index) {
	if (index >= vec->length) {
		CANE_DIE("index %zu > length of %zu", index, vec->length);
		return NULL;
	}

	return vec->data + index;
}

static uint8_t cane_vector_front(cane_vector_t* vec) {
	if (cane_vector_is_empty(vec)) {
		return 0;
	}

	return vec->data[0];
}

static uint8_t cane_vector_back(cane_vector_t* vec) {
	if (cane_vector_is_empty(vec)) {
		return 0;
	}

	return vec->data[vec->length - 1];
}

static uint8_t* cane_vector_begin(cane_vector_t* vec) {
	return cane_vector_at(vec, 0);
}

static uint8_t* cane_vector_end(cane_vector_t* vec) {
	return cane_vector_at(vec, vec->length);
}

static cane_vector_info_t cane_vector_info(cane_vector_t* vec) {
	return (cane_vector_info_t){
		.length = vec->length,
		.ptr = vec->data,
	};
}

// Grow the vector's allocated memory to accomodate the new given size but set
// the capacity such that it is as big as it would have been had you pushed back
// each element one by one.
static cane_vector_t* cane_vector_fit(cane_vector_t* vec, size_t new_length) {
	size_t new_cap = vec->capacity;

	if (new_length >= vec->capacity) {
		while (new_cap < new_length) {
			new_cap += new_cap / 2;  // Growth factor of 1.5
		}

		// Allocate new buffer and zero out the unused elements
		vec->data = cane_reallocate(vec->data, new_cap);
		cane_zero(vec->data + vec->capacity, new_cap - vec->capacity);

		vec->capacity = new_cap;
	}

	return vec;
}

static void cane_vector_push(cane_vector_t* vec, uint8_t value) {
	cane_vector_fit(vec, cane_vector_length(vec) + 1);
	vec->data[vec->length++] = value;
}

static bool cane_vector_pop(cane_vector_t* vec, uint8_t* out) {
	if (cane_vector_is_empty(vec)) {
		return false;
	}

	*out = vec->data[--vec->length];

	return true;
}

// Concatenate two vectors.
// Resizes the first vector to accomodate the data of the second and then copies
// it into the reserved memory.
static cane_vector_t* cane_vector_cat(cane_vector_t* lhs, cane_vector_t* rhs) {
	cane_vector_fit(lhs, lhs->length + rhs->length);

	memcpy(lhs->data + lhs->length, rhs->data, rhs->length);
	lhs->length += rhs->length;

	return lhs;
}

// Insert value at given index by first shifting elements out of the way.
static uint8_t*
cane_vector_insert(cane_vector_t* vec, size_t i, uint8_t value) {
	cane_vector_fit(vec, vec->length + 1);

	memmove(vec->data + i + 1, vec->data + i, vec->length - i);

	vec->data[i] = value;
	vec->length++;

	return vec->data + i;
}

static uint8_t* cane_vector_insert_span(
	cane_vector_t* vec, size_t i, uint8_t* buffer, size_t length
) {
	if (i >= vec->length) {
		CANE_DIE("index out of range");
	}

	if (length == 0) {
		return vec->data + i;
	}

	cane_vector_fit(vec, vec->length + length);

	memmove(vec->data + i + length, vec->data + i, vec->length - i);
	memcpy(vec->data + i, buffer, length);

	vec->length += length;

	return vec->data + i;
}

static uint8_t* cane_vector_remove(cane_vector_t* vec, size_t i) {
	if (i >= vec->length) {
		CANE_DIE("index out of range");
	}

	memmove(vec->data + i, vec->data + i + 1, vec->length - 1);
	vec->length--;

	return vec->data + i;
}

static uint8_t*
cane_vector_remove_span(cane_vector_t* vec, size_t i, size_t length) {
	if (i >= vec->length) {
		CANE_DIE("index out of range");
	}

	for (size_t i = 0; i < length; i++) {
		cane_vector_remove(vec, i);
	}

	return vec->data + i;
}

static uint8_t* cane_vector_fill_span(
	cane_vector_t* vec, size_t i, size_t length, uint8_t value
) {
	if (i >= vec->length) {
		CANE_DIE("index out of range");
	}

	size_t l = CANE_MIN(vec->length - i, length);
	memset(vec->data + i, value, l);

	return vec->data + i;
}

static bool cane_vector_compare_span(
	cane_vector_t* vec, size_t i, size_t length, uint8_t* buffer
) {
	if (i > vec->length || i + length > vec->length || !buffer) {
		return false;
	}

	return memcmp(vec->data + i, buffer, length) == 0;
}

static bool cane_vector_compare(cane_vector_t* lhs, cane_vector_t* rhs) {
	if (lhs->length != rhs->length) {
		return false;
	}

	return cane_vector_compare_span(
		lhs, 0, CANE_MIN(lhs->length, rhs->length), rhs->data
	);
}

static void cane_vector_shrink(cane_vector_t* vec) {
	vec->capacity = vec->length;
	vec->data = cane_reallocate(vec->data, vec->capacity);
}

static void
cane_vector_resize(cane_vector_t* vec, size_t new_length, uint8_t value) {
	cane_vector_fit(vec, new_length);

	if (new_length > vec->length) {
		cane_vector_fill_span(
			vec, vec->length, new_length - vec->length, value
		);
	}

	vec->length = new_length;
}

static void cane_vector_reserve(cane_vector_t* vec, size_t n) {
	cane_vector_fit(vec, vec->length += n);
}

static void cane_vector_clear(cane_vector_t* vec) {
	vec->length = 0;
}

static void cane_vector_broadcast_binary(
	cane_vector_t* vec, cane_broadcast_binary_t fn, uint8_t arg
) {
	for (size_t i = 0; i < vec->length; i++) {
		vec->data[i] = fn(vec->data[i], arg);
	}
}

static void
cane_vector_broadcast_unary(cane_vector_t* vec, cane_broadcast_unary_t fn) {
	for (size_t i = 0; i < vec->length; i++) {
		vec->data[i] = fn(vec->data[i]);
	}
}

static cane_vector_t* cane_vector_repeat(cane_vector_t* lhs, size_t n) {
	size_t new_len = lhs->length * n;
	cane_vector_fit(lhs, new_len);

	if (n == 1) {
		return lhs;
	}
	else if (n == 0) {
		cane_vector_clear(lhs);
		return lhs;
	}

	for (size_t i = 1; i < n + 1; i++) {
		memcpy(lhs->data + lhs->length * i, lhs->data, lhs->length);
	}

	lhs->length = new_len;

	return lhs;
}

static void cane_vector_swap(cane_vector_t* vec, size_t from, size_t to) {
	uint8_t* from_ptr = cane_vector_at(vec, from);
	uint8_t* to_ptr = cane_vector_at(vec, to);

	if (!from_ptr || !to_ptr) {
		CANE_DIE("attempted to swap indices outside of range");
	}

	uint8_t from_val = *from_ptr;

	*from_ptr = *to_ptr;
	*to_ptr = from_val;
}

static uint8_t* cane_vector_rotate(
	cane_vector_t* vec, size_t first, size_t middle, size_t last
) {
	if (first >= vec->length || middle >= vec->length || last > vec->length) {
		CANE_DIE("index out of range");
	}

	if (first == middle) {
		return &vec->data[last];
	}
	if (middle == last) {
		return &vec->data[first];
	}

	size_t write = first;
	size_t next_read = first;

	for (size_t read = middle; read != last; ++write, ++read) {
		if (write == next_read) {
			next_read = read;
		}

		cane_vector_swap(vec, write, read);
	}

	cane_vector_rotate(vec, write, next_read, last);
	return &vec->data[write];
}

static uint8_t* cane_vector_rotate_left(cane_vector_t* vec, size_t nplaces) {
	size_t len = cane_vector_length(vec);
	size_t rel = nplaces % len;
	return cane_vector_rotate(vec, 0, rel, len);
}

static uint8_t* cane_vector_rotate_right(cane_vector_t* vec, size_t nplaces) {
	size_t len = cane_vector_length(vec);
	size_t rel = ((len - nplaces) % len);
	return cane_vector_rotate(vec, 0, rel, len);
}

static uint8_t* cane_vector_reverse(cane_vector_t* vec) {
	size_t len = cane_vector_length(vec);
	size_t middle = len / 2;

	for (size_t i = 0; i < middle; i++) {
		cane_vector_swap(vec, i, len - i - 1);
	}

	return cane_vector_end(vec);
}

static int cane_sort_gt(const void* lhs, const void* rhs) {
	int8_t lval = *(uint8_t*)lhs;
	int8_t rval = *(uint8_t*)rhs;

	return lval - rval;
}

static int cane_sort_lt(const void* lhs, const void* rhs) {
	int8_t lval = *(uint8_t*)lhs;
	int8_t rval = *(uint8_t*)rhs;

	return lval - rval;
}

static void cane_vector_sort(cane_vector_t* vec) {
	size_t len = cane_vector_length(vec);
	qsort(vec->data, len, sizeof(uint8_t), cane_sort_gt);
}

#endif
