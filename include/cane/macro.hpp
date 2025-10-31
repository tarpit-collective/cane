#ifndef CANE_MACRO_HPP
#define CANE_MACRO_HPP

namespace cane {

// Mark as unused.
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

// String macros
#define CANE_CSTR(s \
) /* Convert c-string into a string_view before decaying to pointer. */ \
	std::string_view { \
		s, ((const char*)s) + (sizeof(s) - 1) \
	}

#define CANE_STR_IMPL_(x) #x
#define CANE_STR(x)       CANE_STR_IMPL_(x)

#define CANE_CAT_IMPL_(x, y) x##y
#define CANE_CAT(x, y)       CANE_CAT_IMPL_(x, y)

#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

// Location info
#define CANE_LINEINFO "[" __FILE__ ":" CANE_STR(__LINE__) "]"

#define CANE_LOCATION_FILE __FILE__
#define CANE_LOCATION_LINE CANE_STR(__LINE__)
#define CANE_LOCATION_FUNC __func__

// Utils
#define CANE_MAX(a, b) ((a > b) ? a : b)
#define CANE_MIN(a, b) ((a < b) ? a : b)

}  // namespace cane

#endif
