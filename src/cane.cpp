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
	X(UNDEFINED, "undefined") \
	X(NOT_IMPLEMENTED, "not implemented") \
\
	X(INVALID_AST, "invalid tree") \
	X(UNKNOWN_SYMBOL, "unknown symbol") \
\
	X(UNEXPECTED_CHAR, "unknown character") \
	X(UNEXPECTED_TOKEN, "unexpected token") \
\
	X(EXPECTED_EXPRESSION, "expected an expression") \
	X(EXPECTED_PRIMARY, "expected a primary expression") \
	X(EXPECTED_PREFIX, "expected a prefix operator") \
	X(EXPECTED_INFIX, "expected a infix operator") \
	X(EXPECTED_POSTFIX, "expected a postfix operator") \
	X(EXPECTED_PARENRIGHT, "expected closing parenthesis") \
	X(EXPECTED_BRACERIGHT, "expected closing brace") \
	X(EXPECTED_BRACKETRIGHT, "expected closing bracket") \
	X(EXPECTED_OPERATOR, "expected an operator") \
	X(EXPECTED_IDENTIFIER, "expected an identifier") \
	X(EXPECTED_NUMBER, "expected a number")

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

		if (sv.front() == '\0') {
			fmt::print(stderr, "{}" CANE_RESET " {}: `EOF`!\n", LogKind::FAIL, kind);
			return;
		}

		fmt::print(stderr, "{}" CANE_RESET " {}: `{}`!\n", LogKind::FAIL, kind, sv);
	}
}  // namespace cane

// Precedence kinds
namespace cane {
#define PREC_KINDS \
	X(NONE, "None", 0) \
	X(LAST, "Last", 0) \
	X(INCR, "Incr", 1)

#define X(a, b, c) a = c,
	enum class PrecKind : size_t {
		PREC_KINDS
	};
#undef X

	namespace detail {
#define X(a, b, c) std::string_view { b },
		constexpr std::array PREC_TO_STR = { PREC_KINDS };
#undef X

		constexpr std::string_view prec_to_str(PrecKind x) {
			return detail::PREC_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, PrecKind x) {
		return (os << detail::prec_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::PrecKind>: fmt::ostream_formatter {};

// Associativity kinds
namespace cane {
#define ASS_KINDS \
	X(NONE, "None", 0) \
	X(LEFT, "Left", 1) \
	X(RIGHT, "Right", 0)

#define X(a, b, c) a = c,
	enum class AssKind : size_t {
		ASS_KINDS
	};
#undef X

	namespace detail {
#define X(a, b, c) std::string_view { b },
		constexpr std::array ASS_TO_STR = { ASS_KINDS };
#undef X

		constexpr std::string_view prec_to_str(AssKind x) {
			return detail::ASS_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, AssKind x) {
		return (os << detail::prec_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::AssKind>: fmt::ostream_formatter {};

namespace cane {
	// Token types
	// Fields are: Name, String, Precedence, Associativity
#define SYMBOL_KINDS \
	X(NONE, "None", NONE, NONE) \
	X(ENDFILE, "Eof", NONE, NONE) \
	X(WHITESPACE, "Whitespace", NONE, NONE) \
	X(COMMA, "Comma", NONE, NONE) \
	X(END, "End", NONE, NONE) \
	X(CONS, "Cons", NONE, NONE) \
\
	X(BRACKETLEFT, "BracketLeft", NONE, NONE) \
	X(BRACKETRIGHT, "BracketRight", NONE, NONE) \
\
	X(PARENLEFT, "ParenLeft", NONE, NONE) \
	X(PARENRIGHT, "ParenRight", NONE, NONE) \
\
	X(BRACELEFT, "BraceLeft", NONE, NONE) \
	X(BRACERIGHT, "BraceRight", NONE, NONE) \
\
	X(IDENTIFIER, "Identifier", NONE, NONE) \
	X(NUMBER, "Number", NONE, NONE) \
\
	X(BEAT, "Beat", NONE, NONE) \
	X(REST, "Rest", NONE, NONE) \
\
	X(CALL, "Call", INCR, LEFT) \
\
	X(ASSIGN, "Assign", INCR, LEFT) \
\
	X(FUNCTION, "Function", INCR, LEFT) \
\
	X(OR, "Or", INCR, LEFT) \
	X(AND, "And", LAST, LEFT) \
	X(XOR, "Xor", LAST, LEFT) \
	X(REPEAT, "Repeat", LAST, LEFT) \
	X(SHIFTLEFT, "ShiftLeft", LAST, LEFT) \
	X(SHIFTRIGHT, "ShiftRight", LAST, LEFT) \
\
	X(MAP, "Map", INCR, LEFT) \
\
	X(CONCAT, "Concat", INCR, LEFT) \
\
	X(INVERT, "Invert", INCR, RIGHT) \
	X(REVERSE, "Reverse", LAST, RIGHT) \
\
	X(ADD, "Add", INCR, LEFT) \
	X(SUB, "Sub", LAST, LEFT) \
\
	X(MUL, "Mul", INCR, LEFT) \
	X(DIV, "Div", LAST, LEFT) \
\
	X(TIMEMUL, "TimeMul", INCR, RIGHT) \
	X(TIMEDIV, "TimeDiv", LAST, RIGHT) \
\
	X(EUC, "Euc", INCR, LEFT) \
\
	X(LCM, "Lcm", INCR, LEFT) \
	X(GCD, "Gcd", LAST, LEFT) \
\
	X(ABS, "Abs", INCR, RIGHT) \
	X(NEG, "Neg", LAST, RIGHT)

#define X(sym, str, prec, ass) sym,
	enum class SymbolKind : size_t {
		SYMBOL_KINDS TOTAL,
	};
#undef X

	namespace detail {
#define X(sym, str, prec, ass) std::string_view { str },
		constexpr std::array SYMBOL_TO_STR = { SYMBOL_KINDS };
#undef X

		constexpr std::string_view symbol_to_str(SymbolKind x) {
			return detail::SYMBOL_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, SymbolKind x) {
		return (os << detail::symbol_to_str(x));
	}

	namespace detail {
#define X(sym, str, prec, ass) handle_op(SymbolKind::sym, PrecKind::prec, AssKind::ass);

		constexpr decltype(auto) generate_bp_table() {
			std::array<std::pair<size_t, size_t>, static_cast<size_t>(SymbolKind::TOTAL)> table;
			size_t current_precedence_level = 0;

			auto handle_op = [&](SymbolKind sym, PrecKind prec, AssKind ass) {
				current_precedence_level += static_cast<size_t>(prec);

				size_t op_prec = prec == PrecKind::NONE ? 0 : current_precedence_level;
				size_t op_ass = ass == AssKind::NONE ? 0 : static_cast<size_t>(ass);

				table.at(static_cast<size_t>(sym)) = { op_prec, op_prec + op_ass };
			};

			SYMBOL_KINDS

			return table;
		}

#undef X
	}  // namespace detail

	inline std::pair<size_t, size_t> binding_power(SymbolKind kind) {
		constexpr auto table = detail::generate_bp_table();
		auto bp = table.at(static_cast<size_t>(kind));
		CANE_LOG(LogKind::INFO, "{} -> {}", kind, bp);
		return bp;
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
		Lexer(std::string_view sv): sv_(sv), sv_it_(sv_.begin()), sv_end_(sv_.end()), peek_() {
			take();
		}

		template <typename F>
		constexpr bool take_if(F&& fn) {  // Take character if it matches predicate.
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

		constexpr bool take_str(std::string_view sv) {  // Attempt to take string from input.
			if (not std::equal(sv.begin(), sv.end(), sv_it_)) {
				return false;
			}

			sv_it_ += sv.size();
			return true;
		}

		constexpr bool take_ident(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (not take_if(is_alpha)) {
				return false;
			}

			take_while(is_alphanumeric);
			sym = Symbol { std::string_view { start_it, sv_it_ }, SymbolKind::IDENTIFIER };

			if (sym.sv == "lcm") {
				sym.kind = SymbolKind::LCM;
			}

			else if (sym.sv == "gcd") {
				sym.kind = SymbolKind::GCD;
			}

			return true;
		}

		constexpr bool take_number(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (not take_while(is_digit)) {
				return false;
			}

			sym = Symbol { std::string_view { start_it, sv_it_ }, SymbolKind::NUMBER };
			return true;
		}

		constexpr bool take_sigil(Symbol& sym) {
			sym = Symbol({ sv_it_, sv_it_ + 1 });
			auto start_it = sv_it_;

			if (take_str(",")) {
				sym.kind = SymbolKind::COMMA;
			}

			else if (take_str("$")) {
				sym.kind = SymbolKind::CALL;
			}

			else if (take_str("\\")) {
				sym.kind = SymbolKind::FUNCTION;
			}

			else if (take_str("!")) {
				sym.kind = SymbolKind::BEAT;
			}

			else if (take_str(".")) {
				sym.kind = SymbolKind::REST;
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

			else if (take_str("~")) {
				sym.kind = SymbolKind::INVERT;
			}

			else if (take_str("'")) {
				sym.kind = SymbolKind::REVERSE;
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

			else if (take_str("{")) {
				sym.kind = SymbolKind::BRACELEFT;
			}

			else if (take_str("}")) {
				sym.kind = SymbolKind::BRACERIGHT;
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

		[[nodiscard]] constexpr Symbol peek() const {
			return peek_;
		}

		constexpr Symbol take() {
			Symbol sym;

			take_whitespace(sym);  // Skip whitespace.

			if (sv_it_ >= sv_end_) {
				sym.kind = SymbolKind::ENDFILE;
			}

			else if (take_number(sym)) {}
			else if (take_ident(sym)) {}
			else if (take_sigil(sym)) {}

			else {
				report(sym.sv, ErrorKind::UNEXPECTED_CHAR);
			}

			Symbol out = peek_;
			peek_ = sym;

			return out;
		}

		template <typename F>
		constexpr void expect(F&& fn, ErrorKind x) {
			if (not fn(peek_)) {
				report(peek_.sv, x);
			}
		}

		template <typename F>
		constexpr Symbol take_or_fail(F&& fn, ErrorKind x) {
			if (not fn(peek_)) {
				report(peek_.sv, x);
			}

			return take();
		}

		constexpr void discard() {
			take();
		}

		template <typename F>
		constexpr void discard_if(F&& fn) {
			if (fn(peek_)) {
				take();
			}
		}
	};

}  // namespace cane

// Types
namespace cane {
#define TYPE_KINDS \
	X(NONE, "None") \
	X(SCALAR, "Scalar") \
	X(MELODY, "Melody") \
	X(RHYTHM, "Rhythm") \
	X(SEQUENCE, "Sequence") \
	X(PATTERN, "Pattern")

#define X(a, b) a,
	enum class TypeKind : size_t {
		TYPE_KINDS
	};
#undef X

	namespace detail {
#define X(a, b) std::string_view { b },
		constexpr std::array TYPE_TO_STR = { TYPE_KINDS };
#undef X

		constexpr std::string_view type_to_str(TypeKind x) {
			return detail::TYPE_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, TypeKind x) {
		return (os << detail::type_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::TypeKind>: fmt::ostream_formatter {};

namespace cane {
	constexpr bool is_primary(Symbol s) {
		return eq_any(s.kind,
			SymbolKind::IDENTIFIER,
			SymbolKind::NUMBER,
			SymbolKind::BRACKETLEFT,
			SymbolKind::PARENLEFT,
			SymbolKind::BRACELEFT,
			SymbolKind::BEAT,
			SymbolKind::REST);
	}

	constexpr bool is_prefix(Symbol s) {
		return eq_any(s.kind,
			SymbolKind::INVERT,
			SymbolKind::REVERSE,
			// SymbolKind::ADD,  // Abs
			// SymbolKind::SUB,  // Neg
			// SymbolKind::MUL,  // Tempo Mul
			// SymbolKind::DIV,  // Tempo Div
			SymbolKind::ABS,
			SymbolKind::NEG,
			SymbolKind::TIMEMUL,
			SymbolKind::TIMEDIV,
			SymbolKind::FUNCTION);
	}

	constexpr bool is_expression(Symbol s) {
		return is_primary(s) or is_prefix(s);
	}

	constexpr bool is_concat(Symbol s) {
		return is_expression(s);
	}

	constexpr bool is_infix(Symbol s) {
		return eq_any(s.kind,
			SymbolKind::ADD,
			SymbolKind::SUB,
			SymbolKind::MUL,
			SymbolKind::DIV,
			SymbolKind::EUC,
			SymbolKind::MAP,
			SymbolKind::REPEAT,
			SymbolKind::SHIFTLEFT,
			SymbolKind::SHIFTRIGHT,
			SymbolKind::OR,
			SymbolKind::AND,
			SymbolKind::XOR,
			SymbolKind::CALL,
			SymbolKind::CONCAT);
	}

	constexpr bool is_postfix(Symbol s) {
		return eq_any(s.kind, SymbolKind::ASSIGN);
	}

	constexpr bool is_binary(Symbol s) {
		return is_infix(s) or is_postfix(s) or is_concat(s);
	}

	// Node
	// Visit
	// Iter children    (Iterate through nodes on the same level)
	// Insert block     (Insert begin/end pair with some other nodes inbetween)
	// Insert atom      (Insert non-block symbol like "Number")
	// Get parent block (Get the parent sub-tree)
	// Get Nth node
	// Find "End" atom  (Given a begin symbol, find its corresponding end token)
	// Extract inner nodes  (Return sub-tree with child nodes, removing the surrounding block)
	// Wrap tree inside block  (Given a sub-tree, return a new one wrapped inside some block)

	struct Type {
		TypeKind type;
		std::shared_ptr<Type> left;
		std::shared_ptr<Type> right;
	};

	struct Node {
		SymbolKind kind;
		std::string_view sv;

		std::shared_ptr<Type> type;
		std::shared_ptr<Node> left;
		std::shared_ptr<Node> right;

		Node(SymbolKind kind_): kind(kind_), sv(""), type(nullptr), left(nullptr), right(nullptr) {}

		Node(Symbol sym_): kind(sym_.kind), sv(sym_.sv), type(nullptr), left(nullptr), right(nullptr) {}

		Node(Symbol sym_, std::shared_ptr<Node> left_, std::shared_ptr<Node> right_):
				kind(sym_.kind), sv(sym_.sv), type(nullptr), left(left_), right(right_) {}

		Node(SymbolKind kind_, std::shared_ptr<Node> left_, std::shared_ptr<Node> right_):
				kind(kind_), sv(""), type(nullptr), left(left_), right(right_) {}

		Node(SymbolKind kind_, std::string_view sv_, std::shared_ptr<Node> left_, std::shared_ptr<Node> right_):
				kind(kind_), sv(sv_), type(nullptr), left(left_), right(right_) {}
	};

	// class Context {
	// 	// symbol table
	// 	// type mappings
	// };

	class Parser {
		private:
		Lexer lx;

		public:
		Parser(std::string_view sv): lx(sv) {}

		inline std::shared_ptr<Node> expression(Symbol sym, size_t bp);
		inline std::shared_ptr<Node> primary(Symbol sym);
		inline std::shared_ptr<Node> prefix(Symbol sym, size_t bp);
		inline std::shared_ptr<Node> infix(Symbol sym, std::shared_ptr<Node> left, size_t bp);
		inline std::shared_ptr<Node> postfix(Symbol sym, std::shared_ptr<Node> left);
		inline std::shared_ptr<Node> parse();
	};

	inline std::shared_ptr<Node> Parser::primary(Symbol sym) {
		CANE_LOG(LogKind::OKAY, "sym: {}", sym);

		switch (sym.kind) {
			case SymbolKind::NUMBER:
			case SymbolKind::IDENTIFIER:
			case SymbolKind::BEAT:
			case SymbolKind::REST: {
				return std::make_shared<Node>(lx.take());
			} break;

			// Grouping/layering/random
			case SymbolKind::PARENLEFT: {
				lx.discard();  // lparen

				lx.expect(is_expression, ErrorKind::EXPECTED_EXPRESSION);
				auto expr = expression(lx.peek(), 0);  // Reset binding power

				lx.take_or_fail(is_sym(SymbolKind::PARENRIGHT), ErrorKind::EXPECTED_PARENRIGHT);
				return expr;
			} break;

			case SymbolKind::BRACELEFT: {
				lx.discard();  // lbrace

				std::shared_ptr<Node> node = nullptr;

				lx.expect(is_expression, ErrorKind::EXPECTED_EXPRESSION);

				do {
					node = std::make_shared<Node>(SymbolKind::CONS, node, expression(lx.peek(), 0));
					lx.discard_if(is_sym(SymbolKind::COMMA));  // seperating comma
				} while (not is_sym(SymbolKind::BRACERIGHT, SymbolKind::ENDFILE)(lx.peek()));

				lx.take_or_fail(is_sym(SymbolKind::BRACERIGHT), ErrorKind::EXPECTED_BRACERIGHT);
				return node;
			} break;

			case SymbolKind::BRACKETLEFT: {
				lx.discard();  // lbracket

				std::shared_ptr<Node> node = nullptr;

				while (not is_sym(SymbolKind::BRACKETRIGHT, SymbolKind::ENDFILE)(lx.peek())) {
					node = std::make_shared<Node>(SymbolKind::CONS, node, expression(lx.peek(), 0));
					lx.discard_if(is_sym(SymbolKind::COMMA));  // seperating comma
				}

				lx.take_or_fail(is_sym(SymbolKind::BRACKETRIGHT), ErrorKind::EXPECTED_BRACKETRIGHT);
				return node;
			} break;

			default: break;
		}

		report(sym.sv, ErrorKind::EXPECTED_PRIMARY);
	}

	inline std::shared_ptr<Node> Parser::prefix(Symbol sym, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", sym, bp);

		switch (sym.kind) {
			// Function definition
			case SymbolKind::FUNCTION: {
				lx.discard();  // slash

				std::shared_ptr<Node> node = nullptr;

				auto name = lx.take_or_fail(is_sym(SymbolKind::IDENTIFIER), ErrorKind::EXPECTED_IDENTIFIER);
				auto ident = std::make_shared<Node>(name);

				lx.expect(is_expression, ErrorKind::EXPECTED_EXPRESSION);
				auto expr = expression(lx.peek(), bp);

				return std::make_shared<Node>(SymbolKind::FUNCTION, ident, expr);
			} break;

			case SymbolKind::TIMEMUL:
			case SymbolKind::TIMEDIV: {
				lx.discard();  // op

				auto division = expression(lx.peek(), 0);
				auto expr = expression(lx.peek(), 0);

				return std::make_shared<Node>(sym, division, expr);
			} break;

			case SymbolKind::ABS:
			case SymbolKind::NEG:

			case SymbolKind::INVERT:
			case SymbolKind::REVERSE: {
				lx.discard();  // op
				return std::make_shared<Node>(sym, nullptr, expression(lx.peek(), bp));
			} break;

			default: break;
		}

		report(sym.sv, ErrorKind::EXPECTED_PREFIX);
	}

	inline std::shared_ptr<Node> Parser::infix(Symbol sym, std::shared_ptr<Node> left, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", sym, bp);

		if (is_concat(sym)) {  // implicit concat
			return std::make_shared<Node>(SymbolKind::CONCAT, left, expression(lx.peek(), bp));
		}

		else {
			switch (sym.kind) {
				case SymbolKind::ADD:
				case SymbolKind::SUB:
				case SymbolKind::MUL:
				case SymbolKind::DIV:

				case SymbolKind::LCM:
				case SymbolKind::GCD:

				case SymbolKind::EUC:
				case SymbolKind::MAP:

				case SymbolKind::SHIFTLEFT:
				case SymbolKind::SHIFTRIGHT:

				case SymbolKind::REPEAT:

				case SymbolKind::OR:
				case SymbolKind::AND:
				case SymbolKind::XOR:

				case SymbolKind::CALL: {
					lx.discard();  // op
					return std::make_shared<Node>(sym, left, expression(lx.peek(), bp));
				} break;

				default: break;
			}
		}

		report(sym.sv, ErrorKind::EXPECTED_INFIX);
	}

	inline std::shared_ptr<Node> Parser::postfix(Symbol sym, std::shared_ptr<Node> left) {
		CANE_LOG(LogKind::OKAY, "sym: {}", sym);

		switch (sym.kind) {
			case SymbolKind::ASSIGN: {
				lx.discard();  // op

				auto ident = std::make_shared<Node>(sym);
				return std::make_shared<Node>(sym, ident, left);
			} break;

			default: break;
		}

		report(sym.sv, ErrorKind::EXPECTED_POSTFIX);
	}

	inline std::shared_ptr<Node> Parser::expression(Symbol sym, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", sym, bp);

		std::shared_ptr<Node> node = nullptr;

		// Re-assign overloaded sigils based on them being prefix.
		// This simplifies other logic later like for handling binding
		// power.
		switch (sym.kind) {
			case SymbolKind::ADD: sym.kind = SymbolKind::ABS; break;
			case SymbolKind::SUB: sym.kind = SymbolKind::NEG; break;
			case SymbolKind::MUL: sym.kind = SymbolKind::TIMEMUL; break;
			case SymbolKind::DIV: sym.kind = SymbolKind::TIMEDIV; break;

			default: break;
		}

		// Handle unary or primary expressions.
		if (is_prefix(sym)) {  // prefix expression
			auto [lbp, rbp] = binding_power(sym.kind);
			node = prefix(sym, rbp);
		}

		else if (is_primary(sym)) {  // primary expr
			node = primary(lx.peek());
		}

		else {
			report(sym.sv, ErrorKind::EXPECTED_PRIMARY);
		}

		sym = lx.peek();

		// Loop while we have binary ops.
		while (is_binary(sym)) {
			// Make sure we properly look up precedence of concat since it's implicit.
			// Without this, we would not correctly look up the precedence and the AST
			// would be malformed.
			if (is_concat(sym)) {
				sym = Symbol { sym.sv, SymbolKind::CONCAT };
			}

			// Look up binding power and handle binary op parsing.
			auto [lbp, rbp] = binding_power(sym.kind);
			if (lbp < bp) {
				break;
			}

			// Handle ops
			if (is_postfix(sym)) {
				node = postfix(lx.peek(), node);  // Re-assign tree because we've passed it in here to be consumed and
												  // we create a new tree to be used afterwards.
			}

			else if (is_infix(sym)) {
				node = infix(lx.peek(), node, rbp);
			}

			else {
				report(sym.sv, ErrorKind::UNEXPECTED_TOKEN);
			}

			sym = lx.peek();
		}

		return node;
	}

	inline std::shared_ptr<Node> Parser::parse() {
		std::shared_ptr<Node> node = nullptr;

		while (lx.peek().kind != SymbolKind::ENDFILE) {
			node = expression(lx.peek(), 0);  // Re-assign every time to discard last expression.
		}

		return node;
	}
}  // namespace cane

// Visitors
namespace cane {
	inline void visit(std::shared_ptr<Node> node, int indent = 0) {
		auto spaces = std::string(indent, ' ');

		if (node == nullptr) {
			CANE_LOG(LogKind::INFO, "{}<null>", spaces);
			return;
		}

		switch (node->kind) {
			case SymbolKind::BEAT:  // Atoms
			case SymbolKind::REST:

			case SymbolKind::NUMBER:
			case SymbolKind::IDENTIFIER: {
				CANE_LOG(LogKind::INFO, "{}{} {}", spaces, node->kind, node->sv);
			} break;

			case SymbolKind::INVERT:  // Unary
			case SymbolKind::REVERSE:

			case SymbolKind::ABS:
			case SymbolKind::NEG: {
				CANE_LOG(LogKind::INFO, "{}{}", spaces, node->kind);
				visit(node->right, indent + 1);
			} break;

			case SymbolKind::ADD:  // Binary
			case SymbolKind::SUB:
			case SymbolKind::MUL:
			case SymbolKind::DIV:

			case SymbolKind::CALL:

			case SymbolKind::ASSIGN:

			case SymbolKind::FUNCTION:

			case SymbolKind::CONCAT:
			case SymbolKind::MAP:

			case SymbolKind::OR:
			case SymbolKind::AND:
			case SymbolKind::XOR:

			case SymbolKind::REPEAT:

			case SymbolKind::SHIFTLEFT:
			case SymbolKind::SHIFTRIGHT:

			case SymbolKind::TIMEMUL:
			case SymbolKind::TIMEDIV:

			case SymbolKind::EUC:

			case SymbolKind::LCM:
			case SymbolKind::GCD: {
				CANE_LOG(LogKind::INFO, "{}{}", spaces, node->kind);

				visit(node->left, indent + 1);
				visit(node->right, indent + 1);
			} break;

			default: break;
		}
	}
}  // namespace cane

int main(int, const char* argv[]) {
	try {
		// cane::Context ctx;
		cane::Parser parser { std::string_view { argv[1] } };

		auto expr = parser.parse();
		cane::visit(expr);
		// fmt::print("{}\n", fmt::join(expr, "\n"));
	}

	catch (cane::Report r) {
		cane::report_handler(r);
	}

	return 0;
}
