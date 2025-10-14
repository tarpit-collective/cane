#ifndef CANE_MEM_H
#define CANE_MEM_H

#include <string.h>
#include <cane/log.h>

void* cane_allocate(size_t size) {
	void* ptr = calloc(1, size);
	memset(ptr, 0, size);

	if (!ptr) {
		CANE_DIE("failed to allocate %lu bytes", size);
	}

	return ptr;
}

void* cane_reallocate(void* ptr, size_t size) {
	ptr = realloc(ptr, size);

	if (!ptr) {
		CANE_DIE("failed to realloc to size %lu", size);
	}

	return ptr;
}

#endif
