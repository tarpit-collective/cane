#ifndef CANE_STR_H
#define CANE_STR_H

#include <ctype.h>
#include <string.h>

#include "cane/util.h"

/////////////
// Strings //
/////////////

typedef struct cane_string_view cane_string_view_t;

struct cane_string_view {
	const char* begin;
	const char* end;
};

// String functions
static size_t cane_string_view_length(cane_string_view_t sv) {
	return cane_ptrdiff(sv.begin, sv.end);
}

static bool
cane_string_view_eq(cane_string_view_t lhs, cane_string_view_t rhs) {
	size_t lhs_length = cane_ptrdiff(lhs.begin, lhs.end);
	size_t rhs_length = cane_ptrdiff(rhs.begin, rhs.end);

	if (lhs_length != rhs_length) {
		return false;
	}

	return strncmp(lhs.begin, rhs.begin, lhs_length) == 0;
}

typedef struct cane_string_view_info cane_string_view_info_t;

struct cane_string_view_info {
	const char* ptr;
	size_t length;
};

static cane_string_view_info_t cane_string_view_info(cane_string_view_t sv) {
	return (cane_string_view_info_t){
		.ptr = sv.begin,
		.length = cane_string_view_length(sv),
	};
}

#define CANE_SV(str) \
	((cane_string_view_t){str, ((const char*)str) + (sizeof(str) - 1)})

// Predicates
static bool cane_is_whitespace(char c) {
	return isspace(c);
}

static bool cane_is_not_newline(char c) {
	return c != '\n';
}

static bool cane_is_alpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool cane_is_digit(char c) {
	return c >= '0' && c <= '9';
}

static bool cane_is_alpha_numeric(char c) {
	return cane_is_alpha(c) || cane_is_digit(c);
}

static bool cane_is_identifier(char c) {
	return cane_is_alpha_numeric(c) || c == '_';
}

static bool cane_is_identifier_start(char c) {
	return cane_is_alpha(c) || c == '_';
}

#endif
