#ifndef CANE_LIST_H
#define CANE_LIST_H

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

typedef struct cane_list cane_list_t;

struct cane_list {
	int64_t value;
	cane_list_t* next;
};

static cane_list_t* cane_list_create(int value) {
	cane_list_t* list = calloc(1, sizeof(cane_list_t));

	list->value = value;
	list->next = NULL;

	return list;
}

static cane_list_t* cane_list_head(cane_list_t* list) {
	return list;
}

static cane_list_t* cane_list_tail(cane_list_t* list) {
	return list->next;
}

static cane_list_t* cane_list_first(cane_list_t* list) {
	return list;
}

static cane_list_t* cane_list_last(cane_list_t* list) {
	while (list->next != NULL) {
		list = list->next;
	}

	return list;
}

static cane_list_t* cane_list_concat(cane_list_t* lhs, cane_list_t* rhs) {
	cane_list_t* last = cane_list_last(lhs);
	last->next = rhs;
	return lhs;
}

static cane_list_t* cane_list_append(cane_list_t* list, int value) {
	cane_list_t* new = cane_list_create(value);
	return cane_list_concat(list, new);
}

static cane_list_t* cane_list_prepend(cane_list_t* list, int value) {
	cane_list_t* new = cane_list_create(value);
	new->next = list;
	return new;
}

#endif
