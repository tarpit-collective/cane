#include <iostream>
#include <sstream>
#include <fstream>
#include <string>
#include <string_view>
#include <chrono>
#include <thread>
#include <filesystem>
#include <memory>
#include <algorithm>

#include <libremidi/libremidi.hpp>
#include <libremidi/reader.hpp>

#include <conflict/conflict.hpp>

#include <fmt/core.h>
#include <fmt/format.h>
#include <fmt/ranges.h>
#include <fmt/color.h>
#include <fmt/ostream.h>

#include <commandline.h>

// Macros
namespace cane {
#define CANE_STR_IMPL_(x) #x
#define CANE_STR(x)       CANE_STR_IMPL_(x)

#define CANE_CAT_IMPL_(x, y) x##y
#define CANE_CAT(x, y)       CANE_CAT_IMPL_(x, y)

#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

#define CANE_TRACE "[" __FILE__ ":" CANE_STR(__LINE__) "] "
}  // namespace cane

// Utilities
namespace cane {
	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}
}  // namespace cane

// Errors
namespace cane {
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

#define CANE_ERR CANE_RED CANE_BOLD
#define CANE_OK  CANE_BLUE
}  // namespace cane

namespace cane {
#define LOG_KINDS \
	X(INFO, CANE_RESET "[-] info") \
	X(WARN, CANE_BLUE "[*] warn") \
	X(FAIL, CANE_RED "[!] fail") \
	X(OKAY, CANE_GREEN "[^] okay") \
	X(EXPR, CANE_MAGENTA "[=] expr")

#define X(a, b) a,
	enum class LogKind : size_t {
		LOG_KINDS
	};
#undef X

	namespace detail {
#define X(a, b) std::string_view { b },
		constexpr std::array LOG_TO_STR = { LOG_KINDS };
#undef X

		constexpr std::string_view log_to_str(LogKind x) {
			return LOG_TO_STR[static_cast<size_t>(x)];
		}
	}

	inline std::ostream& operator<<(std::ostream& os, LogKind x) {
		return (os << detail::log_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::LogKind>: fmt::ostream_formatter {};

namespace cane {
	namespace detail {
		template <typename T>
		inline decltype(auto) dbg_impl(
			std::string_view file, std::string_view line, std::string_view func, std::string_view str, T&& expr) {
			fmt::print(std::cerr,
				"{} [{}:{}] `{}`" CANE_RESET " {} = {}\n",
				LogKind::EXPR,
				std::filesystem::relative(file).native(),
				line,
				func,
				str,
				std::forward<T>(expr));

			return std::forward<T>(expr);
		}

		template <typename... Ts>
		constexpr decltype(auto) log_impl(LogKind kind,
			std::string_view file,
			std::string_view line,
			std::string_view func,
			std::string_view fmt,
			Ts&&... args) {
			fmt::print(stderr, "{} [{}:{}] ", kind, std::filesystem::relative(file).native(), line);

			if (func != "operator()") {
				fmt::print(stderr, "`{}`" CANE_RESET, func);
			}

			fmt::print(stderr, " ");
			fmt::print(stderr, fmt::runtime(fmt), std::forward<Ts>(args)...);

			fmt::print(stderr, "\n");
		}
	}  // namespace detail

#ifndef NDEBUG

#define CANE_DBG(expr) \
	[&, CANE_VAR(func) = __func__]() { \
		return cane::detail::dbg_impl(__FILE__, CANE_STR(__LINE__), CANE_VAR(func), CANE_STR(expr), (expr)); \
	}()

#define CANE_DBG_RUN(expr) \
	do { \
		((expr)); \
	} while (0)

#define CANE_LOG(...) \
	do { \
		[CANE_VAR(func) = __func__]<typename... Ts>( \
			cane::LogKind CANE_VAR(kind), std::string_view fmt, Ts&&... CANE_VAR(args)) { \
			cane::detail::log_impl(CANE_VAR(kind), \
				__FILE__, \
				CANE_STR(__LINE__), \
				CANE_VAR(func), \
				fmt, \
				std::forward<Ts>(CANE_VAR(args))...); \
		}(__VA_ARGS__); \
	} while (0)

#else

#define CANE_DBG(expr) ((expr))

#define CANE_DBG_RUN(expr) \
	do { \
	} while (0)

#define CANE_LOG(...) \
	do { \
	} while (0)

#endif

}  // namespace cane

namespace cane {
	// Error types
#define ERROR_KINDS \
	X(GENERIC, "an error occurred") \
	X(UNREACHABLE, "unreachable code") \
	X(NOT_IMPLEMENTED, "not implemented") \
	X(UNKNOWN_CHAR, "unknown character") \
	X(UNDEFINED, "undefined") \
\
	X(UNEXPECTED_TOKEN, "unexpected token") \
	X(INVALID_AST, "invalid tree") \
\
	X(EXPECTED_IDENT, "expected an identifier") \
	X(EXPECTED_NUMBER, "expected a number") \
\
	X(UNKNOWN_SYMBOL, "unknown symbol")

#define X(a, b) a,
	enum class ErrorKind : size_t {
		ERROR_KINDS
	};
#undef X

	namespace detail {
#define X(a, b) std::string_view { b },
		constexpr std::array ERROR_TO_STR = { ERROR_KINDS };
#undef X

		constexpr std::string_view error_to_str(ErrorKind x) {
			return ERROR_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, ErrorKind x) {
		return (os << detail::error_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::ErrorKind>: fmt::ostream_formatter {};

namespace cane {
	struct Report {
		std::string_view sv;
		ErrorKind kind;
	};

	[[noreturn]] inline void report(std::string_view sv, ErrorKind x) {
		throw Report { sv, x };
	}

	[[noreturn]] inline void report(ErrorKind x) {
		throw Report { "", x };
	}

	inline void report_handler(Report x) {
		auto [sv, kind] = x;

		if (sv.empty() or sv == "") {
			fmt::print(stderr, "{}" CANE_RESET " {}!\n", LogKind::FAIL, kind);
			return;
		}

		fmt::print(stderr, "{}" CANE_RESET " {}: `{}`!\n", LogKind::FAIL, kind, sv);
	}
}  // namespace cane

// Lexer
namespace cane {
	// Token types
#define SYMBOL_KINDS \
	X(NONE, "None") \
	X(ENDFILE, "Eof") \
	X(WHITESPACE, "Whitespace") \
\
	X(IDENTIFIER, "Identifier") \
	X(NUMBER, "Number") \
\
	X(BEAT, "Beat") \
	X(REST, "Rest") \
	X(RAND, "Rand") \
\
	X(ADD, "Add") \
	X(SUB, "Sub") \
	X(MUL, "Mul") \
	X(DIV, "Div") \
\
	X(EUC, "Euc") \
	X(ASSIGN, "Assign") \
\
	X(INVERT, "Invert") \
	X(REVERSE, "Reverse") \
\
	X(SHIFTLEFT, "ShiftLeft") \
	X(SHIFTRIGHT, "ShiftRight") \
\
	X(REPEAT, "Repeat") \
	X(MAP, "Map") \
\
	X(OR, "Or") \
	X(AND, "And") \
	X(XOR, "Xor") \
\
	X(BRACKETLEFT, "BracketLeft") \
	X(BRACKETRIGHT, "BracketRight") \
\
	X(PARENLEFT, "ParenLeft") \
	X(PARENRIGHT, "ParenRight") \
\
	X(END, "End")

#define X(a, b) a,
	enum class SymbolKind : size_t {
		SYMBOL_KINDS
	};
#undef X

	namespace detail {
#define X(a, b) std::string_view { b },
		constexpr std::array SYMBOL_TO_STR = { SYMBOL_KINDS };
#undef X

		constexpr std::string_view symbol_to_str(SymbolKind x) {
			return detail::SYMBOL_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, SymbolKind x) {
		return (os << detail::symbol_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::SymbolKind>: fmt::ostream_formatter {};

namespace cane {
	struct Symbol {
		std::string_view sv;  // original token string
		SymbolKind kind;

		constexpr Symbol(): sv(""), kind(SymbolKind::NONE) {}

		constexpr Symbol(SymbolKind kind_): sv(""), kind(kind_) {}

		constexpr Symbol(std::string_view sv_): sv(sv_), kind(SymbolKind::NONE) {}

		constexpr Symbol(std::string_view sv_, SymbolKind kind_): sv(sv_), kind(kind_) {}

		bool operator==(const Symbol&) const = default;
	};

	inline std::ostream& operator<<(std::ostream& os, Symbol s) {
		return (os << "{\"" << s.sv << "\", " << s.kind << "}");
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::Symbol>: fmt::ostream_formatter {};

namespace cane {
	// Predicates
	constexpr bool is_visible(char c) {
		return c >= 33 and c <= 126;
	}

	constexpr bool is_control(char c) {
		return c >= 0 and c <= 31;
	}

	constexpr bool is_alpha(char c) {
		return any(c >= 'a' and c <= 'z', c >= 'A' and c <= 'Z', c == '_');
	}

	constexpr bool is_whitespace(char c) {
		return any(c >= 9 and c <= 13, c == ' ');
	}

	constexpr bool is_digit(char c) {
		return c >= '0' and c <= '9';
	}

	constexpr bool is_alphanumeric(char c) {
		return is_alpha(c) or is_digit(c);
	}

	// Higher order functions
	template <typename... Ts>
	constexpr decltype(auto) is_sym(Ts&&... kinds) {
		return [=](Symbol other) {
			return ((other.kind == kinds) or ...);
		};
	}

	template <typename... Ts>
	constexpr decltype(auto) is_char(Ts&&... kinds) {
		return [=](char other) {
			return ((other == kinds) or ...);
		};
	}

	// Lexer
	class Lexer {
		private:
		std::string_view sv_;

		std::string_view::iterator sv_it_;
		std::string_view::iterator sv_end_;

		Symbol peek_;

		public:
		Lexer(std::string_view sv): sv_(sv), sv_it_(sv_.begin()), sv_end_(sv_.end()), peek_() {}

		template <typename F>
		[[nodiscard]] constexpr bool take_if(F&& fn) {  // Take character if it matches predicate.
			if (sv_it_ >= sv_end_ or not fn(*sv_it_)) {
				return false;
			}

			++sv_it_;
			return true;
		}

		template <typename F>
		constexpr bool take_while(F&& fn) {  // Take characters while predicate holds.
			bool at_least_one = false;

			while (take_if(std::forward<F>(fn))) {
				at_least_one = true;
			}

			return at_least_one;
		}

		[[nodiscard]] constexpr bool take_str(std::string_view sv) {  // Attempt to take string from input.
			if (not std::equal(sv.begin(), sv.end(), sv_it_)) {
				return false;
			}

			sv_it_ += sv.size();
			return true;
		}

		[[nodiscard]] constexpr bool take_ident(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (not take_if(is_alpha)) {
				return false;
			}

			take_while(is_alphanumeric);
			sym = Symbol { std::string_view { start_it, sv_it_ }, SymbolKind::IDENTIFIER };

			if (sym.sv == "inv") {
				sym.kind = SymbolKind::INVERT;
			}

			else if (sym.sv == "rev") {
				sym.kind = SymbolKind::REVERSE;
			}

			return true;
		}

		[[nodiscard]] constexpr bool take_number(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (not take_while(is_digit)) {
				return false;
			}

			sym = Symbol { std::string_view { start_it, sv_it_ }, SymbolKind::NUMBER };
			return true;
		}

		[[nodiscard]] constexpr bool take_sigil(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (take_str("!")) {
				sym.kind = SymbolKind::BEAT;
			}

			if (take_str(".")) {
				sym.kind = SymbolKind::REST;
			}

			if (take_str("?")) {
				sym.kind = SymbolKind::RAND;
			}

			else if (take_str("+")) {
				sym.kind = SymbolKind::ADD;
			}

			else if (take_str("-")) {
				sym.kind = SymbolKind::SUB;
			}

			else if (take_str("*")) {
				sym.kind = SymbolKind::MUL;
			}

			else if (take_str("/")) {
				sym.kind = SymbolKind::DIV;
			}

			else if (take_str(":")) {
				sym.kind = SymbolKind::EUC;
			}

			else if (take_str("=>")) {
				sym.kind = SymbolKind::ASSIGN;
			}

			else if (take_str("<")) {
				sym.kind = SymbolKind::SHIFTLEFT;
			}

			else if (take_str(">")) {
				sym.kind = SymbolKind::SHIFTRIGHT;
			}

			else if (take_str("**")) {
				sym.kind = SymbolKind::REPEAT;
			}

			else if (take_str("@")) {
				sym.kind = SymbolKind::MAP;
			}

			else if (take_str("[")) {
				sym.kind = SymbolKind::BRACKETLEFT;
			}

			else if (take_str("]")) {
				sym.kind = SymbolKind::BRACKETRIGHT;
			}

			else if (take_str("(")) {
				sym.kind = SymbolKind::PARENLEFT;
			}

			else if (take_str(")")) {
				sym.kind = SymbolKind::PARENRIGHT;
			}

			else {
				return false;
			}

			sym.sv = std::string_view { start_it, sv_it_ };
			return true;
		}

		constexpr bool take_whitespace(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (not take_while(is_whitespace)) {
				return false;
			}

			sym = Symbol { std::string_view { start_it, sv_it_ }, SymbolKind::WHITESPACE };
			return true;
		}

		template <typename F>
		constexpr void expect(F&& fn, ErrorKind x) {
			if (not fn(peek_)) {
				report(peek_.sv, x);
			}
		}

		[[nodiscard]] constexpr Symbol peek() const {
			return peek_;
		}

		[[nodiscard]] constexpr bool take(Symbol& out) {
			Symbol sym;

			take_whitespace(sym);  // Skip whitespace.

			if (sv_it_ >= sv_end_) {
				sym.kind = SymbolKind::ENDFILE;
			}

			else if (take_number(sym)) {}
			else if (take_ident(sym)) {}
			else if (take_sigil(sym)) {}

			else {
				return false;
			}

			out = peek_;
			peek_ = sym;

			return true;
		}
	};

}  // namespace cane

int main(int, const char* argv[]) {
	try {
		cane::Symbol s;
		cane::Lexer lx { std::string_view { CANE_DBG(argv[1]) } };

		if (not lx.take(s)) {
			cane::report(s.sv, cane::ErrorKind::UNKNOWN_CHAR);
		}

		while (lx.peek().kind != cane::SymbolKind::ENDFILE) {
			if (not lx.take(s)) {
				cane::report(s.sv, cane::ErrorKind::UNKNOWN_CHAR);
			}

			CANE_LOG(cane::LogKind::OKAY, "{}", s);
		}
		// lx.expect(cane::is_sym(cane::SymbolKind::NUMBER), cane::ErrorKind::EXPECTED_NUMBER);

		// if (not CANE_DBG(lx.take_ident(s))) {
		// 	cane::report(s.sv, cane::ErrorKind::EXPECTED_IDENT);
		// }

		// CANE_DBG(lx.take_whitespace(s));

		// if (not CANE_DBG(lx.take_number(s))) {
		// 	cane::report(s.sv, cane::ErrorKind::EXPECTED_NUMBER);
		// }

		// CANE_DBG(lx.take_sigil(s));
		// CANE_LOG(cane::LogKind::EXPR, "{}", s);
	}

	catch (cane::Report r) {
		cane::report_handler(r);
	}

	return 0;
}
