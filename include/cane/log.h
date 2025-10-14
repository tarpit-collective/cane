#ifndef CANE_LOG_H
#define CANE_LOG_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <cane/def.h>
#include <cane/enum.h>

// ANSI Colours
#define CANE_RESET "\x1b[0m"
#define CANE_BOLD  "\x1b[1m"

#define CANE_BLACK   "\x1b[30m"
#define CANE_RED     "\x1b[31m"
#define CANE_GREEN   "\x1b[32m"
#define CANE_YELLOW  "\x1b[33m"
#define CANE_BLUE    "\x1b[34m"
#define CANE_MAGENTA "\x1b[35m"
#define CANE_CYAN    "\x1b[36m"
#define CANE_WHITE   "\x1b[37m"

#define CANE_BLACK_BRIGHT   "\x1b[90m"
#define CANE_RED_BRIGHT     "\x1b[91m"
#define CANE_GREEN_BRIGHT   "\x1b[92m"
#define CANE_YELLOW_BRIGHT  "\x1b[93m"
#define CANE_BLUE_BRIGHT    "\x1b[94m"
#define CANE_MAGENTA_BRIGHT "\x1b[95m"
#define CANE_CYAN_BRIGHT    "\x1b[96m"
#define CANE_WHITE_BRIGHT   "\x1b[97m"

#define CANE_BG_BLACK   "\x1b[40m"
#define CANE_BG_RED     "\x1b[41m"
#define CANE_BG_GREEN   "\x1b[42m"
#define CANE_BG_YELLOW  "\x1b[43m"
#define CANE_BG_BLUE    "\x1b[44m"
#define CANE_BG_MAGENTA "\x1b[45m"
#define CANE_BG_CYAN    "\x1b[46m"
#define CANE_BG_WHITE   "\x1b[47m"

#define CANE_BG_BLACK_BRIGHT   "\x1b[100m"
#define CANE_BG_RED_BRIGHT     "\x1b[101m"
#define CANE_BG_GREEN_BRIGHT   "\x1b[102m"
#define CANE_BG_YELLOW_BRIGHT  "\x1b[103m"
#define CANE_BG_BLUE_BRIGHT    "\x1b[104m"
#define CANE_BG_MAGENTA_BRIGHT "\x1b[105m"
#define CANE_BG_CYAN_BRIGHT    "\x1b[106m"
#define CANE_BG_WHITE_BRIGHT   "\x1b[107m"

// Logging
#define CANE_LOG_KINDS \
	X(CANE_PRIORITY_INFO, "[.]", "info", CANE_WHITE) \
	X(CANE_PRIORITY_WARN, "[*]", "warn", CANE_BLUE) \
	X(CANE_PRIORITY_FAIL, "[!]", "fail", CANE_RED) \
	X(CANE_PRIORITY_OKAY, "[^]", "okay", CANE_GREEN) \
\
	X(CANE_PRIORITY_DIED, "[!]", "died", CANE_RED_BRIGHT) \
	X(CANE_PRIORITY_ENTR, "[>]", "entr", CANE_YELLOW_BRIGHT)

#define X(x, y, z, w) x,

typedef enum {
	CANE_LOG_KINDS
} cane_loglevel_t;

#undef X

// Convert from/to strings & enums
#define X(x, y, z, w) [x] = y,
const char* CANE_LOGLEVEL_TO_STR[] = {CANE_LOG_KINDS};
#undef X

#define X(x, y, z, w) [x] = z,
const char* CANE_LOGLEVEL_HUMAN_TO_STR[] = {CANE_LOG_KINDS};
#undef X

#define X(x, y, z, w) [x] = w,
const char* CANE_LOGLEVEL_COLOUR[] = {CANE_LOG_KINDS};
#undef X

#undef CANE_LOG_KINDS

////////////
// Logger //
////////////

static void cane_log_lineinfo_v(
	cane_loglevel_t lvl,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	va_list args
) {
	const char* lvl_str = CANE_LOGLEVEL_TO_STR[lvl];
	const char* lvl_human = CANE_LOGLEVEL_HUMAN_TO_STR[lvl];
	const char* lvl_colour = CANE_LOGLEVEL_COLOUR[lvl];

	fprintf(
		stderr,
		CANE_BOLD "%s%s" CANE_RESET
				  " "
				  "%s%s" CANE_RESET,
		lvl_colour,
		lvl_str,
		lvl_colour,
		lvl_human
	);

	// Check if any combination of filename or line is NULL.
	if (filename != NULL && line != NULL) {
		fprintf(stderr, " [%s:%s]", filename, line);
	}

	else if (filename != NULL && line == NULL) {
		fprintf(stderr, " [%s]", filename);
	}

	else if (filename == NULL && line != NULL) {
		fprintf(stderr, " [%s]", line);
	}

	// Check if function name is empty.
	if (func != NULL) {
		fprintf(stderr, " `%s`", func);
	}

	// Check if there is any format string (and thus any varargs).
	if (fmt != NULL) {
		fprintf(stderr, ": ");
		vfprintf(stderr, fmt, args);
	}

	fputc('\n', stderr);
}

static void cane_log_lineinfo(
	cane_loglevel_t lvl,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	...
) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(lvl, filename, line, func, fmt, args);

	va_end(args);
}

static void cane_log(cane_loglevel_t lvl, const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(lvl, NULL, NULL, NULL, fmt, args);

	va_end(args);
}

static void cane_die(
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	...
) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(CANE_PRIORITY_DIED, filename, line, func, fmt, args);

	va_end(args);
	fflush(stderr);

	exit(EXIT_FAILURE);
}

static void cane_assert(
	bool cond,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	...
) {
	if (!cond) {
		va_list args;
		va_start(args, fmt);

		cane_log_lineinfo_v(
			CANE_PRIORITY_DIED, filename, line, func, fmt, args
		);

		va_end(args);
		fflush(stderr);

		exit(EXIT_FAILURE);
	}
}

#define CANE_LOG_INFO(...) \
	cane_log_lineinfo( \
		CANE_PRIORITY_INFO, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_WARN(...) \
	cane_log_lineinfo( \
		CANE_PRIORITY_WARN, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_FAIL(...) \
	cane_log_lineinfo( \
		CANE_PRIORITY_FAIL, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_OKAY(...) \
	cane_log_lineinfo( \
		CANE_PRIORITY_OKAY, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)

// Find out where you are with a rainbow.
#define CANE_WHEREAMI() \
	cane_log_lineinfo( \
		CANE_PRIORITY_INFO, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		CANE_RED "Y" CANE_RED_BRIGHT "O" CANE_YELLOW "U" CANE_RESET \
				 " " CANE_GREEN "A" CANE_BLUE "R" CANE_MAGENTA "E" CANE_RESET \
				 " " CANE_MAGENTA_BRIGHT "H" CANE_RED "E" CANE_RED_BRIGHT \
				 "R" CANE_YELLOW "E" CANE_RESET \
	)

#define CANE_FUNCTION_ENTER() \
	cane_log_lineinfo( \
		CANE_PRIORITY_ENTR, __FILE__, CANE_STR(__LINE__), __func__, NULL \
	)

#define CANE_DIE(...) \
	cane_die(__FILE__, CANE_STR(__LINE__), __func__, __VA_ARGS__)

#define CANE_UNIMPLEMENTED() \
	cane_die(__FILE__, CANE_STR(__LINE__), __func__, "unimplemented!")
#define CANE_UNREACHABLE() \
	cane_die(__FILE__, CANE_STR(__LINE__), __func__, "unreachable!")

#define CANE_ASSERT(cond, ...) \
	cane_assert(cond, __FILE__, CANE_STR(__LINE__), __func__, __VA_ARGS__)

// TODO: Debug macro (might not be reasonable)

#endif
