#ifndef CANE_UTIL_H
#define CANE_UTIL_H

#include <stddef.h>
#include <cane/def.h>
#include <cane/log.h>
#include <stdlib.h>
#include <unistd.h>

// UTILITY FUNCTIONS
// Get the name of the binary from argv[0].
// Basically `basename` but without allocating or trimming trailing slashes.
static const char* cane_exe(const char* exe) {
	size_t slash = 0;
	size_t i = 0;

	for (; exe[i] != '\0'; ++i) {
		if (exe[i] == '/') {
			slash = i + 1;
		}
	}

	return exe + CANE_MIN(slash, i);
}

void* cane_xalloc(size_t size) {
	void* ptr = calloc(1, size);

	if (!ptr) {
		CANE_DIE("failed to allocate %lu bytes", size);
	}

	return ptr;
}

void* cane_xrealloc(void* ptr_old, size_t size) {
	void* ptr = realloc(ptr_old, size);

	if (!ptr) {
		CANE_DIE("failed to realloc 0x%p to size %lu", ptr_old, size);
	}

	return ptr;
}

void cane_xclose(int fd) {
	if (close(fd) == -1) {
		CANE_DIE("failed to close fd %d", fd);
	}
}

#endif
