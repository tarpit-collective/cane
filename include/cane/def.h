#ifndef CANE_DEF_H
#define CANE_DEF_H

#include <stdbool.h>

// Typedefs
typedef struct cane_status cane_status_t;  // Used for error handling

struct cane_status {
	bool is_okay;
};

static cane_status_t cane_okay() {
	return (cane_status_t){.is_okay = true};
}

static cane_status_t cane_fail() {
	return (cane_status_t){.is_okay = false};
}

// Unused Macro
#define CANE_IMPL_UNUSED0()
#define CANE_IMPL_UNUSED1(a)          (void)(a)
#define CANE_IMPL_UNUSED2(a, b)       (void)(a), CANE_IMPL_UNUSED1(b)
#define CANE_IMPL_UNUSED3(a, b, c)    (void)(a), CANE_IMPL_UNUSED2(b, c)
#define CANE_IMPL_UNUSED4(a, b, c, d) (void)(a), CANE_IMPL_UNUSED3(b, c, d)
#define CANE_IMPL_UNUSED5(a, b, c, d, e) \
	(void)(a), CANE_IMPL_UNUSED4(b, c, d, e)

#define CANE_VA_NUM_ARGS_IMPL(_0, _1, _2, _3, _4, _5, N, ...) N
#define CANE_VA_NUM_ARGS(...) \
	CANE_VA_NUM_ARGS_IMPL(100, ##__VA_ARGS__, 5, 4, 3, 2, 1, 0)

#define CANE_UNUSED_IMPL_(nargs) CANE_IMPL_UNUSED##nargs
#define CANE_UNUSED_IMPL(nargs)  CANE_UNUSED_IMPL_(nargs)
#define CANE_UNUSED(...) \
	CANE_UNUSED_IMPL(CANE_VA_NUM_ARGS(__VA_ARGS__))(__VA_ARGS__)

// Macro Utils
#define CANE_STR_IMPL_(x) #x
#define CANE_STR(x)       CANE_STR_IMPL_(x)

#define CANE_CAT_IMPL_(x, y) x##y
#define CANE_CAT(x, y)       CANE_CAT_IMPL_(x, y)

#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

#define CANE_LINEINFO "[" __FILE__ ":" CANE_STR(__LINE__) "]"

#define CANE_MAX(a, b) ((a > b) ? a : b)
#define CANE_MIN(a, b) ((a < b) ? a : b)

#endif
