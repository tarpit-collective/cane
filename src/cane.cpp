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

// Lexer
namespace cane {
	enum class PrecKind : size_t {
		NONE,
		LAST,
		INCR,
	};

	enum class AssKind : size_t {
		NONE,
		LEFT,
		RIGHT,
	};

	// Token types
#define SYMBOL_KINDS \
	X(NONE, "None", NONE, NONE) \
	X(ENDFILE, "Eof", NONE, NONE) \
	X(WHITESPACE, "Whitespace", NONE, NONE) \
	X(END, "End", NONE, NONE) \
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
	X(MAP, "Map", LAST, LEFT) \
	X(ASSIGN, "Assign", LAST, LEFT) \
\
	X(CONCAT, "Concat", INCR, LEFT) \
\
	X(OR, "Or", INCR, LEFT) \
	X(AND, "And", LAST, LEFT) \
	X(XOR, "Xor", LAST, LEFT) \
	X(REPEAT, "Repeat", LAST, LEFT) \
	X(SHIFTLEFT, "ShiftLeft", LAST, LEFT) \
	X(SHIFTRIGHT, "ShiftRight", LAST, LEFT) \
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
#define X(sym, str, prec, ass) table.at(static_cast<size_t>(SymbolKind::sym)) = { PrecKind::prec, AssKind::ass };

		constexpr decltype(auto) generate_precedence_table() {
			std::array<std::pair<PrecKind, AssKind>, static_cast<size_t>(SymbolKind::TOTAL)> table;
			table.fill({ PrecKind::NONE, AssKind::NONE });

			SYMBOL_KINDS

			std::array<std::pair<size_t, size_t>, static_cast<size_t>(SymbolKind::TOTAL)> lookup;

			size_t i = 0;
			size_t running = 0;

			for (auto [prec, ass]: table) {
				size_t assoc = 0;
				size_t prece = 0;

				switch (prec) {
					case PrecKind::NONE: prece = 0; break;
					case PrecKind::LAST: prece = running; break;
					case PrecKind::INCR: prece = ++running; break;

					default: break;
				}

				switch (ass) {
					case AssKind::RIGHT:
					case AssKind::NONE: assoc = 0; break;
					case AssKind::LEFT: assoc = 1; break;

					default: break;
				}

				lookup.at(i++) = { prece, prece + assoc };
			}

			return lookup;
		}

#undef X
	}  // namespace detail

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
			[[maybe_unused]] Symbol s = take();
		}

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

			if (sym.sv == "lcm") {
				sym.kind = SymbolKind::LCM;
			}

			else if (sym.sv == "gcd") {
				sym.kind = SymbolKind::GCD;
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

		[[nodiscard]] constexpr Symbol take() {
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
		[[nodiscard]] constexpr Symbol take_or(F&& fn, ErrorKind x) {
			if (not fn(peek_)) {
				report(peek_.sv, x);
			}

			return take();
		}
	};

}  // namespace cane

namespace cane {
	// Token types
#define OP_KINDS \
	X(PREFIX, "Prefix") \
	X(INFIX, "Infix") \
	X(POSTFIX, "Postfix")

#define X(a, b) a,
	enum class OpKind : size_t {
		OP_KINDS
	};
#undef X

	namespace detail {
#define X(a, b) std::string_view { b },
		constexpr std::array OP_TO_STR = { OP_KINDS };
#undef X

		constexpr std::string_view op_to_str(OpKind x) {
			return detail::OP_TO_STR[static_cast<size_t>(x)];
		}
	}  // namespace detail

	inline std::ostream& operator<<(std::ostream& os, OpKind x) {
		return (os << detail::op_to_str(x));
	}
}  // namespace cane

template <>
struct fmt::formatter<cane::OpKind>: fmt::ostream_formatter {};

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
			SymbolKind::TIMEDIV);
	}

	constexpr bool is_expression(Symbol s) {
		return is_primary(s) or is_prefix(s);
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
			SymbolKind::XOR);
	}

	constexpr bool is_postfix(Symbol s) {
		return eq_any(s.kind, SymbolKind::ASSIGN);
	}

	using Tree = std::vector<Symbol>;

	inline std::pair<size_t, size_t> binding_power(Symbol s) {
		constexpr auto table = detail::generate_precedence_table();
		return table.at(static_cast<size_t>(s.kind));
	}

	constexpr Tree expression(Lexer& lx, size_t bp);

	constexpr Tree primary(Lexer& lx) {
		CANE_LOG(LogKind::OKAY, "sym: {}", lx.peek());

		Symbol sym = lx.peek();
		Tree tree;

		switch (sym.kind) {
			case SymbolKind::NUMBER:
			case SymbolKind::IDENTIFIER:
			case SymbolKind::BEAT:
			case SymbolKind::REST: {
				tree = { lx.take() };
			} break;

			// Grouping/layering/random
			case SymbolKind::PARENLEFT: {
				[[maybe_unused]] auto lparen = lx.take();

				lx.expect(is_expression, ErrorKind::EXPECTED_EXPRESSION);
				tree = expression(lx, 0);  // Reset binding power

				lx.expect(is_sym(SymbolKind::PARENRIGHT), ErrorKind::EXPECTED_PARENRIGHT);
				[[maybe_unused]] auto rparen = lx.take();
			} break;

			case SymbolKind::BRACELEFT: {
				[[maybe_unused]] auto lbrace = lx.take();
				tree.emplace_back(lbrace);

				lx.expect(is_expression, ErrorKind::EXPECTED_EXPRESSION);

				do {
					auto expr = expression(lx, 0);
					tree.insert(tree.end(), expr.begin(), expr.end());
				} while (not is_sym(SymbolKind::BRACERIGHT, SymbolKind::ENDFILE)(lx.peek()));

				lx.expect(is_sym(SymbolKind::BRACERIGHT), ErrorKind::EXPECTED_BRACERIGHT);
				[[maybe_unused]] auto rbrace = lx.take();

				tree.emplace_back(rbrace.sv, SymbolKind::END);
			} break;

			case SymbolKind::BRACKETLEFT: {
				[[maybe_unused]] auto lbracket = lx.take();
				tree.emplace_back(lbracket);

				while (not is_sym(SymbolKind::BRACKETRIGHT, SymbolKind::ENDFILE)(lx.peek())) {
					auto expr = expression(lx, 0);
					tree.insert(tree.end(), expr.begin(), expr.end());
				}

				lx.expect(is_sym(SymbolKind::BRACKETRIGHT), ErrorKind::EXPECTED_BRACKETRIGHT);
				[[maybe_unused]] auto rbracket = lx.take();

				tree.emplace_back(rbracket.sv, SymbolKind::END);
			} break;

			// ...
			default: {
				report(sym.sv, ErrorKind::EXPECTED_PRIMARY);
			} break;
		}

		return tree;
	}

	constexpr Tree prefix(Lexer& lx, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", lx.peek(), bp);

		Symbol sym = lx.peek();
		Tree tree;

		switch (sym.kind) {
			case SymbolKind::ABS:
			case SymbolKind::NEG:
			case SymbolKind::TIMEMUL:
			case SymbolKind::TIMEDIV:
			case SymbolKind::INVERT:
			case SymbolKind::REVERSE: {
				auto op = lx.take();
				tree.emplace_back(op);

				auto expr = expression(lx, bp);
				tree.insert(tree.end(), expr.begin(), expr.end());

				tree.emplace_back(op.sv, SymbolKind::END);
			} break;

			// ...
			default: {
				report(sym.sv, ErrorKind::EXPECTED_PREFIX);
			} break;
		}

		return tree;
	}

	constexpr Tree infix(Lexer& lx, Tree left, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", lx.peek(), bp);

		Symbol sym = lx.peek();
		Tree tree;

		if (is_expression(sym)) {
			tree.emplace_back(sym.sv, SymbolKind::CONCAT);

			tree.insert(tree.end(), left.begin(), left.end());

			auto expr = expression(lx, bp);
			tree.insert(tree.end(), expr.begin(), expr.end());

			tree.emplace_back(sym.sv, SymbolKind::END);
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
				case SymbolKind::XOR: {
					auto op = lx.take();
					tree.emplace_back(op);

					tree.insert(tree.end(), left.begin(), left.end());

					auto expr = expression(lx, bp);
					tree.insert(tree.end(), expr.begin(), expr.end());

					tree.emplace_back(op.sv, SymbolKind::END);
				} break;

				// ...
				default: {
					report(sym.sv, ErrorKind::EXPECTED_INFIX);
				} break;
			}
		}

		return tree;
	}

	constexpr Tree postfix(Lexer& lx, Tree left) {
		CANE_LOG(LogKind::OKAY, "sym: {}", lx.peek());

		Symbol sym = lx.peek();
		Tree tree;

		switch (sym.kind) {
			case SymbolKind::ASSIGN: {
				auto op = lx.take();
				tree.emplace_back(op);

				tree.insert(tree.end(), left.begin(), left.end());

				tree.emplace_back(op.sv, SymbolKind::END);
			} break;

			// ...
			default: {
				report(sym.sv, ErrorKind::EXPECTED_POSTFIX);
			} break;
		}

		return tree;
	}

	constexpr Tree expression(Lexer& lx, size_t bp) {
		CANE_LOG(LogKind::OKAY, "sym: {}, bp: {}", lx.peek(), bp);

		Symbol sym = lx.peek();
		Tree tree;

		if (is_prefix(sym)) {  // prefix expression
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

			auto [lbp, rbp] = binding_power(sym);
			tree = prefix(lx, rbp);
		}

		else if (is_primary(sym)) {  // primary expr
			tree = primary(lx);
		}

		else {
			report(sym.sv, ErrorKind::UNEXPECTED_TOKEN);
		}

		sym = lx.peek();

		// We use is_expression here because concatenation is implicit
		while (is_infix(sym) or is_postfix(sym) or is_expression(sym)) {  // infix or postfix
			if (is_postfix(sym)) {
				auto [lbp, rbp] = binding_power(sym);
				if (lbp < bp) {
					break;
				}

				tree = postfix(lx, tree);  // Re-assign tree because we've passed it in here to be consumed and we
										   // create a new tree to be used afterwards.
			}

			else if (is_infix(sym) or is_expression(sym)) {  // Also handles implicit concat
				auto [lbp, rbp] = binding_power(sym);
				if (lbp < bp) {
					break;
				}

				tree = infix(lx, tree, rbp);
			}

			else {
				report(sym.sv, ErrorKind::UNEXPECTED_TOKEN);
			}

			sym = lx.peek();
		}

		return tree;
	}
}  // namespace cane

int main(int, const char* argv[]) {
	try {
		cane::Lexer lx { std::string_view { argv[1] } };
		auto expr = cane::expression(lx, 0);

		fmt::print("{}\n", fmt::join(expr, "\n"));
	}

	catch (cane::Report r) {
		cane::report_handler(r);
	}

	return 0;
}
