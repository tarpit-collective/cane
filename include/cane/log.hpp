#ifndef CANE_LOG_HPP
#define CANE_LOG_HPP

// Logging
namespace cane {
	#define CANE_RESET  "\x1b[0m"
	#define CANE_BOLD   "\x1b[1m"

	#define CANE_BLACK    "\x1b[30m"
	#define CANE_RED      "\x1b[31m"
	#define CANE_GREEN    "\x1b[32m"
	#define CANE_YELLOW   "\x1b[33m"
	#define CANE_BLUE     "\x1b[34m"
	#define CANE_MAGENTA  "\x1b[35m"
	#define CANE_CYAN     "\x1b[36m"
	#define CANE_WHITE    "\x1b[37m"

	#ifndef NDEBUG
		// We call this function in order to evaluate `expr` only once in case
		// it has side effects. If we used a macro for this, it would be evaluated
		// twice. Once for printing the result and once for returning it.
		namespace detail {
			template <typename T> inline decltype(auto) dbg(
				const char* file,
				const char* line,
				const char* expr_s,
				T&& expr
			) {
				cane::println(std::cerr,
					"[", file, ":", line, "] ", expr_s, " = ", std::forward<T>(expr)
				);

				return std::forward<T>(expr);
			}
		}

		#define CANE_DBG(expr) \
			(cane::detail::dbg( \
				__FILE__, CANE_STR(__LINE__), CANE_STR(expr), (expr) \
			))

		#define CANE_DBG_RUN(expr) \
			do { ((expr)); } while (0)
	#else
		#define CANE_DBG(expr) ((expr))
		#define CANE_DBG_RUN(expr) do {} while (0)
	#endif

	#define LOG_LEVELS \
		X(INF, CANE_WHITE   "[ ]") \
		X(DBG, CANE_MAGENTA "[?]") \
		X(WRN, CANE_BLUE    "[*]") \
		X(ERR, CANE_RED     "[!]") \
		X(OK,  CANE_YELLOW  "[-]") \
		X(SUC, CANE_GREEN   "[^]") \
		\
		X(BLACK,   CANE_BLACK   "[0]") \
		X(RED,     CANE_RED     "[1]") \
		X(GREEN,   CANE_GREEN   "[2]") \
		X(YELLOW,  CANE_YELLOW  "[3]") \
		X(BLUE,    CANE_BLUE    "[4]") \
		X(MAGENTA, CANE_MAGENTA "[5]") \
		X(CYAN,    CANE_CYAN    "[6]") \
		X(WHITE,   CANE_WHITE   "[7]")

	#define X(a, b) a,
		enum class LogLevel: size_t { LOG_LEVELS };
	#undef X

	namespace detail {
		#define X(a, b) b,
			constexpr const char* LOG2STR[] = { LOG_LEVELS };
		#undef X

		constexpr const char* log2str(LogLevel x) {
			return LOG2STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, LogLevel x) {
		return print(os, detail::log2str(x));
	}

	#define CANE_LOG(...) \
		do { [CANE_VAR(fn_name) = __func__] (cane::LogLevel CANE_VAR(x), auto&&... CANE_VAR(args)) { \
			CANE_DBG_RUN(( \
				cane::print(std::cerr, CANE_VAR(x), " " CANE_TRACE " ") \
			)); \
			CANE_DBG_RUN(( cane::print(std::cerr, "`", CANE_VAR(fn_name), "`" CANE_RESET) )); \
			if constexpr(sizeof...(CANE_VAR(args)) > 0) { CANE_DBG_RUN( \
				(cane::print(std::cerr, " ", std::forward<decltype(CANE_VAR(args))>(CANE_VAR(args))...)) \
			); } \
			CANE_DBG_RUN(( cane::print(std::cerr, '\n') )); \
		} ( __VA_ARGS__ ); } while (0)
}

#endif
