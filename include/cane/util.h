#ifndef CANE_UTIL_H
#define CANE_UTIL_H

#include <stddef.h>
#include <cane/def.h>

// UTILITY FUNCTIONS
// Return absolute difference between 2 pointers regardless of order.
static size_t cane_ptrdiff(const void* a, const void* b) {
	return b > a ? b - a : a - b;
}

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

#endif
