#ifndef CANE_LEX_HPP
#define CANE_LEX_HPP

#include <functional>
#include <optional>
#include <string_view>
#include <iostream>

#include <cane/macro.hpp>
#include <cane/enum.hpp>
#include <cane/log.hpp>
#include <cane/util.hpp>

namespace cane {

	// Allow us to provide line info and line preview.
	struct Location {
		std::string_view source;  // Original source file
		std::string_view symbol;  // Specific token
	};

	/////////////
	// Symbols //
	/////////////

	struct Symbol {
		SymbolKind kind;
		std::string_view sv;
	};

	constexpr std::string_view DEFAULT_SYMBOL_SV = "(empty)";

	inline std::ostream& operator<<(std::ostream& os, Symbol s) {
		return (os << "{\"" << s.kind << "\", " << s.sv << "}");
	}

	// Parser and lexer predicates & other function pointers
	using CharacterPredicate = bool (*)(char);
	using SymbolPredicate = bool (*)(SymbolKind);
	using SymbolFixup = SymbolKind (*)(SymbolKind);

	///////////
	// LEXER //
	///////////

	// template <typename T>
	// decltype(auto) alternatives(std::optional<T> opt) {
	// 	return opt;
	// }

	// template <typename T1, typename T2, typename... Ts>
	// decltype(auto) alternatives(T1&& fn1, T2&& fn2, Ts&&... fns) {
	// 	return alternatives(fn1().or_else(fn2), std::forward<Ts>(fns)...);
	// }

	class Lexer {
		private:
		std::string_view source;

		std::string_view::iterator it;
		std::string_view::iterator end;

		Symbol lookahead;

		public:
		Lexer(std::string_view sv):
				source(sv), it(sv.begin()), end(sv.end()), lookahead() {
			take();
		}

		// Basic stream interaction
		std::optional<char> str_peek() {
			if (it > end) {  // Check for EOF
				return std::nullopt;
			}

			return *it;
		}

		std::optional<char> str_take() {
			if (auto c = str_peek(); c.has_value()) {
				it++;
				return c.value();
			}

			return std::nullopt;
		}

		// Conditional consumers
		std::optional<char> str_take_if(CharacterPredicate pred) {
			auto peek = str_peek();

			if (not peek.has_value()) {
				return std::nullopt;
			}

			if (not pred(peek.value())) {
				return std::nullopt;
			}

			return str_take();
		}

		// Same as take_if but just takes a character directly
		// for common usecases.
		std::optional<char> str_take_if_char(char c) {
			auto peek = str_peek();

			if (not peek.has_value()) {
				return std::nullopt;
			}

			if (peek.value() != c) {
				return std::nullopt;
			}

			return str_take();
		}

		// Consumes a given string from the lexer stream or nothing at all.
		std::optional<std::string_view> str_take_str(std::string_view sv) {
			if (it + sv.size() > end) {
				return std::nullopt;
			}

			std::string_view peek { it, it + sv.size() };

			if (peek != sv) {
				return std::nullopt;
			}

			it += sv.size();
			return peek;
		}

		// Continue to consume characters while the predicate holds.
		std::optional<std::string_view> str_take_while(CharacterPredicate pred
		) {
			bool taken = false;
			auto begin = it;

			while (str_take_if(pred)) {
				taken = true;
			}

			auto end = it;

			if (not taken) {  // If not even a single match was encountered, we
							  // return failure
				return std::nullopt;
			}

			return std::string_view { begin, end };
		}

		/////////////////////
		// Token Producers //
		/////////////////////

		// These functions wrap the basic "str_take" functions and
		// wrap them into a symbol type/token.

		// TODO: Verify this actually works lol
		template <typename F>
		std::optional<Symbol>
		produce(F producer, SymbolKind kind, CharacterPredicate pred) {
			auto begin = it;

			if (not producer(pred)) {
				return std::nullopt;
			}

			auto end = it;

			return Symbol {
				.kind = kind,
				.sv = std::string_view { begin, end },
			};
		}

		std::optional<Symbol>
		produce_if(SymbolKind kind, CharacterPredicate pred) {
			auto begin = it;

			if (not str_take_if(pred)) {
				return std::nullopt;
			}

			auto end = it;

			return Symbol {
				.kind = kind,
				.sv = std::string_view { begin, end },
			};

			// return produce(
			// 	std::bind(&Lexer::str_take_if, this, std::placeholders::_1),
			// 	kind,
			// 	pred
			// );
		}

		std::optional<Symbol>
		produce_while(SymbolKind kind, CharacterPredicate pred) {
			auto begin = it;

			if (not str_take_while(pred)) {
				return std::nullopt;
			}

			auto end = it;

			return Symbol {
				.kind = kind,
				.sv = std::string_view { begin, end },
			};
		}

		std::optional<Symbol>
		produce_str(SymbolKind kind, std::string_view sv) {
			auto begin = it;

			if (not str_take_str(sv)) {
				return std::nullopt;
			}

			auto end = it;

			return Symbol {
				.kind = kind,
				.sv = std::string_view { begin, end },
			};
		}

		// Cane specific lexer functions
		std::optional<Symbol> produce_identifier() {
			// TODO: Can we just call produce_while and allow digits at the
			// beginning of identifiers?
			auto begin = it;

			if (not str_take_if([](char c) {
					return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
						c == '_';
				})) {
				return std::nullopt;
			}

			str_take_while([](char c) {
				return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or
					(c >= '0' and c <= '9') or c == '_';
			});

			auto end = it;

			Symbol symbol {
				.kind = SymbolKind::Identifier,
				.sv = std::string_view { begin, end },
			};

			// We could have used `cane_produce_str` here to handle these cases
			// but we want "maximal munch" meaning that we lex the entire
			// identifier before trying to classify it. Why? because if we
			// didn't, an identifier like "letfoo" would actually be lexed
			// as 2 seperate tokens because it sees `let` and stops there.

			// Keywords
			if (symbol.sv == CANE_CSTR("lcm")) {
				symbol.kind = SymbolKind::LCM;
			}

			else if (symbol.sv == CANE_CSTR("gcd")) {
				symbol.kind = SymbolKind::GCD;
			}

			// Operators
			else if (symbol.sv == CANE_CSTR("or")) {
				symbol.kind = SymbolKind::Or;
			}

			else if (symbol.sv == CANE_CSTR("xor")) {
				symbol.kind = SymbolKind::Xor;
			}

			else if (symbol.sv == CANE_CSTR("and")) {
				symbol.kind = SymbolKind::And;
			}

			// Type annotations
			else if (symbol.sv == CANE_CSTR("number")) {
				symbol.kind = SymbolKind::AnnotationNumber;
			}

			else if (symbol.sv == CANE_CSTR("string")) {
				symbol.kind = SymbolKind::AnnotationString;
			}

			else if (symbol.sv == CANE_CSTR("rhythm")) {
				symbol.kind = SymbolKind::AnnotationRhythm;
			}

			else if (symbol.sv == CANE_CSTR("melody")) {
				symbol.kind = SymbolKind::AnnotationMelody;
			}

			else if (symbol.sv == CANE_CSTR("sequence")) {
				symbol.kind = SymbolKind::AnnotationSequence;
			}

			else if (symbol.sv == CANE_CSTR("pattern")) {
				symbol.kind = SymbolKind::AnnotationPattern;
			}

			return symbol;
		}

		std::optional<Symbol> produce_sigil() {
			return produce_str(SymbolKind::Coerce, CANE_CSTR("&"))
				.or_else([&] {
					return produce_str(SymbolKind::Backslash, CANE_CSTR("\\"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Comma, CANE_CSTR(","));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Backtick, CANE_CSTR("`"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Quote, CANE_CSTR("'"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::At, CANE_CSTR("@"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Stars, CANE_CSTR("**"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Tilda, CANE_CSTR("~"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Dot, CANE_CSTR("."));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Exclaim, CANE_CSTR("!"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Colon, CANE_CSTR(":"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Semicolon, CANE_CSTR(";"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Fatarrow, CANE_CSTR("=>"));
				})
				.or_else([&] {
					return produce_str(
						SymbolKind::Wigglearrow, CANE_CSTR("~>")
					);
				})
				.or_else([&] {
					return produce_str(SymbolKind::Arrow, CANE_CSTR("->"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Add, CANE_CSTR("+"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Sub, CANE_CSTR("-"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Mul, CANE_CSTR("*"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Div, CANE_CSTR("/"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::LeftParen, CANE_CSTR("("));
				})
				.or_else([&] {
					return produce_str(SymbolKind::RightParen, CANE_CSTR(")"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::LeftBrace, CANE_CSTR("{"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::RightBrace, CANE_CSTR("}"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::LeftBracket, CANE_CSTR("["));
				})
				.or_else([&] {
					return produce_str(
						SymbolKind::RightBracket, CANE_CSTR("]")
					);
				})
				.or_else([&] {
					return produce_str(SymbolKind::LeftShift, CANE_CSTR("<<"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::RightShift, CANE_CSTR(">>"));
				});
		}

		std::optional<Symbol> produce_number() {
			return produce_while(SymbolKind::Number, [](char c) {
				return (c >= '0' and c <= '9');
			});
		}

		std::optional<Symbol> produce_whitespace() {
			return produce_while(SymbolKind::Whitespace, [](char c) {
				return c == '_';
			});
		}

		std::optional<Symbol> produce_comment() {
			auto begin = it;

			if (not str_take_str(CANE_CSTR("#!")) ||
				not str_take_while([](char c) { return c != '\n'; })) {
				return std::nullopt;
			}

			auto end = it;

			return Symbol { .kind = SymbolKind::Comment, .sv = { begin, end } };
		}

		std::optional<Symbol> produce_quoted() {
			if (not str_take_if([](char c) { return c == '"'; })) {
				return std::nullopt;
			}

			auto begin = it;
			str_take_while([](char c) { return c != '"'; });
			auto end = it;

			if (not str_take_if([](char c) { return c == '"'; })) {
				return std::nullopt;
			}

			return Symbol { .kind = SymbolKind::String, .sv = { begin, end } };
		}

		////////////////
		// Lexer Core //
		////////////////

		// With fixup
		std::optional<Symbol> peek(SymbolFixup fixup) {
			if (not str_peek()) {
				return std::nullopt;
			}

			return Symbol { .kind = fixup(lookahead.kind), .sv = lookahead.sv };
		}

		bool peek_is(SymbolPredicate cond, SymbolFixup fixup) {
			auto next = peek(fixup);

			if (not next or not cond(next.value().kind)) {
				return false;
			}

			return true;
		}

		bool peek_is_kind(SymbolKind kind, SymbolFixup fixup) {
			auto next = peek(fixup);

			if (not next or kind != next.value().kind) {
				return false;
			}

			return true;
		}

		// Without fixup
		std::optional<Symbol> peek() {
			return peek([](auto x) { return x; });
		}

		bool peek_is(SymbolPredicate cond) {
			return peek_is(cond, [](auto x) { return x; });
		}

		bool peek_is_kind(SymbolKind kind) {
			return peek_is_kind(kind, [](auto x) { return x; });
		}

		std::optional<Symbol> take_any(SymbolFixup fixup) {
			SymbolKind kind = SymbolKind::None;

			// Handle EOF
			if (it >= end) {  // Must be >= which means we can't
							  // use `cane_str_peek`
				kind = SymbolKind::EndFile;
			}

			// Handle normal tokens
			// TODO: Implement `try_all` style function for chaining a bunch of
			// failable functions returning an optional. Return the one that
			// succeeds.
			auto next =
				produce_identifier().or_else([&] { return produce_number(); }
				).or_else([&] {
					 return produce_sigil();
				 }).or_else([&] { return produce_quoted(); });

			if (not next.has_value()) {
				// TODO: Better error/cleanup error API
				cane::die("unknown charater `{}`!", *it);
			}

			lookahead = next.value();

			return Symbol { .kind = fixup(kind), .sv = DEFAULT_SYMBOL_SV };
		}

		std::optional<Symbol> take_any() {
			return take_any([](auto x) { return x; });
		}

		// Filter out whitespace and comments.
		std::optional<Symbol> take(SymbolFixup fixup) {
			std::optional<Symbol> symbol;

			do {
				symbol = produce_whitespace().or_else([&] {
					return produce_comment();
				});
			} while (symbol.has_value());

			return take_any(fixup);
		}

		std::optional<Symbol> take() {
			return take([](auto x) { return x; });
		}

		std::optional<Symbol> take_if(SymbolPredicate pred, SymbolFixup fixup) {
			auto next = peek(fixup);

			if (not next or not pred(next.value().kind)) {
				return std::nullopt;
			}

			return take(fixup);
		}

		std::optional<Symbol> take_if_kind(SymbolKind kind, SymbolFixup fixup) {
			auto next = peek(fixup);

			if (not next or kind != next.value().kind) {
				return std::nullopt;
			}

			return take(fixup);
		}

		std::optional<Symbol> take_if(SymbolPredicate pred) {
			return take_if(pred, [](auto x) { return x; });
		}

		std::optional<Symbol> take_if_kind(SymbolKind kind) {
			return take_if_kind(kind, [](auto x) { return x; });
		}

		bool discard() {
			return take().has_value();
		}

		bool discard_if(SymbolPredicate pred) {
			return take_if(pred).has_value();
		}

		bool discard_if_kind(SymbolKind kind) {
			return take_if_kind(kind).has_value();
		}

		///////////////
		// Reporting //
		///////////////

		// 	cane_location_t cane_location_create(Lexer* lx) {
		// 		return (cane_location_t) {
		// 			.source = lx->location.source,
		// 			.symbol = lx->peek.location.symbol,
		// 		};
		// 	}

		// 	typedef struct cane_lineinfo cane_lineinfo_t;

		// 	struct cane_lineinfo {
		// 		size_t line;
		// 		size_t column;
		// 	};

		// 	// Calculate line and column.
		// 	cane_lineinfo_t cane_location_coordinates(cane_location_t location)
		// { 		cane_lineinfo_t info = (cane_lineinfo_t) { 			.line =
		// 1, 			.column = 1,
		// 		};

		// 		std::string_view source = location.source;
		// 		std::string_view symbol = location.symbol;

		// 		if (!(symbol.begin >= source.begin && symbol.end <= source.end))
		// { 			CANE_DIE("symbol not in range of source");
		// 		}

		// 		for (const char* ptr = source.begin; ptr != symbol.end; ptr++) {
		// 			if (*ptr == '\n') {
		// 				info.line++;
		// 				info.column = 0;
		// 			}

		// 			info.column++;
		// 		}

		// 		return info;
		// 	}

		// 	// TODO: Preview of token/line of code where error occured
		// 	void cane_report_and_die(
		// 		cane_location_t loc, cane_report_kind_t kind, const char* fmt,
		// ... 	) { 		cane_lineinfo_t info =
		// cane_location_coordinates(loc);

		// 		va_list args;
		// 		va_start(args, fmt);

		// 		fprintf(
		// 			stderr,
		// 			"%s%s %s error" CANE_RESET " @ %zu:%zu => ",
		// 			CANE_LOGLEVEL_COLOUR[CANE_PRIORITY_FAIL],
		// 			CANE_LOGLEVEL_TO_STR[CANE_PRIORITY_FAIL],
		// 			CANE_REPORT_KIND_TO_STR_HUMAN[kind],
		// 			info.line,
		// 			info.column
		// 		);

		// 		vfprintf(stderr, fmt, args);
		// 		fputc('\n', stderr);

		// 		va_end(args);
		// 		exit(EXIT_FAILURE);
		// 	}
	};

}  // namespace cane

#endif
