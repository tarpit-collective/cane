#ifndef CANE_LOG_H
#define CANE_LOG_H

#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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
#define LOGLEVELS \
	X(CANE_PRIORITY_INFO, "[.]", "info", CANE_WHITE) \
	X(CANE_PRIORITY_WARN, "[*]", "warn", CANE_BLUE) \
	X(CANE_PRIORITY_FAIL, "[!]", "fail", CANE_RED) \
	X(CANE_PRIORITY_OKAY, "[^]", "okay", CANE_GREEN) \
\
	X(CANE_PRIORITY_DIED, "[!]", "died", CANE_RED_BRIGHT)

#define X(x, y, z, w) x,

typedef enum {
	LOGLEVELS
} cane_loglevel_t;

#undef X

// Convert from/to strings & enums
#define X(x, y, z, w) [x] = y,
const char* CANE_LOGLEVEL_TO_STR[] = {LOGLEVELS};
#undef X

#define X(x, y, z, w) [x] = z,
const char* CANE_LOGLEVEL_HUMAN_TO_STR[] = {LOGLEVELS};
#undef X

#define X(x, y, z, w) [x] = w,
const char* CANE_LOGLEVEL_COLOUR[] = {LOGLEVELS};
#undef X

#undef LOGLEVELS

////////////
// Logger //
////////////

typedef struct cane_logger cane_logger_t;

struct cane_logger {
	const char* name;  // Name of logger for filtering by pass or stage
	FILE* dest;        // Destination to log to (usually stderr)
	cane_loglevel_t level;
	size_t indent;
};

static cane_logger_t cane_logger_create_default() {
	return (cane_logger_t){
		.name = "global",
		.dest = stderr,
		.level = CANE_PRIORITY_INFO,
		.indent = 0,
	};
}

static void cane_log_lineinfo_v(
	cane_logger_t log,
	cane_loglevel_t lvl,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	va_list args
) {
	if (lvl < log.level) {
		return;
	}

	// If destination is NULL, just don't log anything.
	if (log.dest == NULL) {
		return;
	}

	const char* lvl_str = CANE_LOGLEVEL_TO_STR[lvl];
	const char* lvl_human = CANE_LOGLEVEL_HUMAN_TO_STR[lvl];
	const char* lvl_colour = CANE_LOGLEVEL_COLOUR[lvl];

	fprintf(
		log.dest,
		CANE_BOLD "%*s%s%s" CANE_RESET
				  " "
				  "%s%s" CANE_RESET,
		(int)log.indent * 4,
		"",
		lvl_colour,
		lvl_str,
		lvl_colour,
		lvl_human
	);

	// Check if any combination of filename or line is NULL.
	if (filename != NULL && line != NULL) {
		fprintf(log.dest, " [%s:%s]", filename, line);
	}

	else if (filename != NULL && line == NULL) {
		fprintf(log.dest, " [%s]", filename);
	}

	else if (filename == NULL && line != NULL) {
		fprintf(log.dest, " [%s]", line);
	}

	// Check if function name is empty.
	if (func != NULL) {
		fprintf(log.dest, " `%s`", func);
	}

	// Check if there is any format string (and thus any varargs).
	if (fmt != NULL) {
		fprintf(log.dest, ": ");
		vfprintf(log.dest, fmt, args);
	}

	fputc('\n', log.dest);
}

static void cane_log_lineinfo(
	cane_logger_t log,
	cane_loglevel_t lvl,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	...
) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(log, lvl, filename, line, func, fmt, args);

	va_end(args);
}

static void
cane_log(cane_logger_t log, cane_loglevel_t lvl, const char* fmt, ...) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(log, lvl, NULL, NULL, NULL, fmt, args);

	va_end(args);
}

static void cane_die(
	cane_logger_t log,
	const char* filename,
	const char* line,
	const char* func,
	const char* fmt,
	...
) {
	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(
		log, CANE_PRIORITY_DIED, filename, line, func, fmt, args
	);

	va_end(args);
	fflush(stderr);

	exit(EXIT_FAILURE);
}

static void cane_assert(
	cane_logger_t log,
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
			log, CANE_PRIORITY_DIED, filename, line, func, fmt, args
		);

		va_end(args);
		fflush(stderr);

		exit(EXIT_FAILURE);
	}
}

#define CANE_LOG_INFO(log, ...) \
	cane_log_lineinfo( \
		log, \
		CANE_PRIORITY_INFO, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_WARN(log, ...) \
	cane_log_lineinfo( \
		log, \
		CANE_PRIORITY_WARN, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_FAIL(log, ...) \
	cane_log_lineinfo( \
		log, \
		CANE_PRIORITY_FAIL, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)
#define CANE_LOG_OKAY(log, ...) \
	cane_log_lineinfo( \
		log, \
		CANE_PRIORITY_OKAY, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		__VA_ARGS__ \
	)

// Find out where you are with a rainbow.
#define CANE_WHEREAMI(log) \
	cane_log_lineinfo( \
		log, \
		CANE_PRIORITY_INFO, \
		__FILE__, \
		CANE_STR(__LINE__), \
		__func__, \
		CANE_RED "Y" CANE_RED_BRIGHT "O" CANE_YELLOW "U" CANE_RESET \
				 " " CANE_GREEN "A" CANE_BLUE "R" CANE_MAGENTA "E" CANE_RESET \
				 " " CANE_MAGENTA_BRIGHT "H" CANE_RED "E" CANE_RED_BRIGHT \
				 "R" CANE_YELLOW "E" CANE_RESET \
	)

#define CANE_FUNCTION_ENTER(log) \
	cane_log_lineinfo( \
		log, CANE_PRIORITY_INFO, __FILE__, CANE_STR(__LINE__), __func__, NULL \
	)

#define CANE_DIE(log, ...) \
	cane_die(log, __FILE__, CANE_STR(__LINE__), __func__, __VA_ARGS__)

#define CANE_UNIMPLEMENTED(log) \
	cane_die(log, __FILE__, CANE_STR(__LINE__), __func__, "unimplemented!")
#define CANE_UNREACHABLE(log) \
	cane_die(log, __FILE__, CANE_STR(__LINE__), __func__, "unreachable!")

#define CANE_ASSERT(log, cond, ...) \
	cane_assert(log, cond, __FILE__, CANE_STR(__LINE__), __func__, __VA_ARGS__)

// TODO: Debug macro (might not be reasonable)

#endif
