#ifndef CANE_UTIL_H
#define CANE_UTIL_H

#include <stddef.h>
#include <stdint.h>
#include <unistd.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/str.h>

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

// Memory allocation
static void* cane_allocate(size_t size) {
	void* ptr = calloc(1, size);

	if (!ptr) {
		CANE_DIE("failed to allocate %lu bytes", size);
	}

	return ptr;
}

static void* cane_reallocate(void* ptr, size_t size) {
	ptr = realloc(ptr, size);

	if (!ptr) {
		CANE_DIE("failed to re-allocate to size %lu", size);
	}

	return ptr;
}

static void cane_zero(void* ptr, size_t length) {
	void* end = ptr + length;

	while (ptr != end) {
		*((uint8_t*)ptr++) = 0;
	}
}

static void cane_free(void* ptr) {
	if (!ptr) {
		CANE_DIE("attempting to free a NULL pointer");
	}

	free(ptr);
}

// File IO
// TODO: Properly wrap file functions.
typedef FILE* cane_file_t;

static cane_file_t cane_file_open(cane_string_view_t sv) {
	char buffer[256] = {0};
	cane_string_view_info_t info = cane_string_view_info(sv);

	if (info.length > 256) {
		CANE_DIE("filename too long");
	}

	memcpy(buffer, info.ptr, info.length);
	cane_file_t fp = fopen(buffer, "w+");

	if (!fp) {
		CANE_DIE("failed to open file");
	}

	return fp;
}

static void cane_file_close(cane_file_t fp) {
	if (fclose(fp) != 0) {
		CANE_DIE("failed to close file");
	}
}

static bool cane_read_file(const char* path, const char** data, int* size) {
	FILE* f = fopen(path, "r");
	if (!f) {
		return false;
	}

	if (fseek(f, 0, SEEK_END) == -1) {
		return false;
	}

	int len = ftell(f);
	if (len == -1) {
		return false;
	}

	rewind(f);

	char* buf = cane_allocate(len);
	size_t n = fread(buf, 1, len, f);

	if (n != (size_t)len && ferror(f)) {
		return false;
	}

	*data = buf;
	*size = n;

	if (fclose(f) == EOF) {
		return false;
	}

	return true;
}

static int cane_gcd(int lhs, int rhs) {
	while (rhs != 0) {
		int tmp = lhs % rhs;

		lhs = rhs;
		rhs = tmp;
	}

	return lhs;
}

static int cane_lcm(int lhs, int rhs) {
	return (CANE_MAX(lhs, rhs) / cane_gcd(lhs, rhs)) * CANE_MIN(lhs, rhs);
}

#endif
