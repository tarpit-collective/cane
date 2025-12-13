#ifndef CANE_LEX_HPP
#define CANE_LEX_HPP

#include <optional>
#include <string_view>
#include <iostream>

#include <cane/def.hpp>
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

	constexpr std::ostream& operator<<(std::ostream& os, Symbol s) {
		std::print(
			os, "{{ kind: {}, sv: '{}' }}", symbol_kind_to_str(s.kind), s.sv
		);
		return os;
	}

}  // namespace cane

CANE_FORMATTER_DEF(cane::Symbol);

namespace cane {

	// Parser and lexer predicates & other function pointers
	using CharacterPredicate = bool (*)(char);

	using SymbolPredicate = bool (*)(SymbolKind);
	using SymbolFixup = SymbolKind (*)(SymbolKind);

	///////////
	// LEXER //
	///////////

	class Lexer {
		private:
		std::string_view source;

		std::string_view::iterator it;
		std::string_view::iterator end;

		Symbol lookahead;

		public:
		Lexer(std::string_view sv):
				source(sv), it(sv.begin()), end(sv.end()), lookahead() {
			CANE_UNUSED(take_opt());
		}

		// Basic stream interaction
		[[nodiscard]] bool is_eof() {
			return it > end;
		}

		[[nodiscard]] std::optional<char> str_peek() {
			if (is_eof()) {  // Check for EOF
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
		std::optional<std::string_view>
		str_take_while(CharacterPredicate pred) {
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
			if (symbol.sv == CANE_CSTR("let")) {
				symbol.kind = SymbolKind::Let;
			}

			else if (symbol.sv == CANE_CSTR("lcm")) {
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

			else if (symbol.sv == CANE_CSTR("head")) {
				symbol.kind = SymbolKind::Head;
			}

			else if (symbol.sv == CANE_CSTR("tail")) {
				symbol.kind = SymbolKind::Tail;
			}

			return symbol;
		}

		std::optional<Symbol> produce_sigil() {
			return produce_str(SymbolKind::Coerce, CANE_CSTR("&"))
				// .or_else([&] {
				// 	return produce_str(SymbolKind::Assign, CANE_CSTR("=>"));
				// })
				.or_else([&] {
					return produce_str(SymbolKind::Send, CANE_CSTR("~>"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Incr, CANE_CSTR("++"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Decr, CANE_CSTR("--"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Random, CANE_CSTR("?"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Layer, CANE_CSTR("$"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Function, CANE_CSTR("\\"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Concatenate, CANE_CSTR(","));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Reverse, CANE_CSTR("'"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Map, CANE_CSTR("@"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Repeat, CANE_CSTR("**"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Invert, CANE_CSTR("~"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Call, CANE_CSTR("_"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Rest, CANE_CSTR("."));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Beat, CANE_CSTR("!"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Euclidean, CANE_CSTR(":"));
				})
				.or_else([&] {
					return produce_str(SymbolKind::Block, CANE_CSTR(";"));
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

		std::optional<Symbol> produce_whitespace() {
			return produce_while(SymbolKind::Whitespace, [](char c) {
				return static_cast<bool>(std::isspace(c)) or c == '\n';
			});
		}

		std::optional<Symbol> produce_comment() {
			auto begin = it;

			if (not str_take_str(CANE_CSTR("#!"))) {
				return std::nullopt;
			}

			str_take_while([](char c) { return c != '\n'; });

			auto end = it;

			return Symbol { .kind = SymbolKind::Comment, .sv = { begin, end } };
		}

		std::optional<Symbol> produce_eof() {
			if (it < end) {
				return std::nullopt;
			}

			return Symbol { .kind = SymbolKind::EndFile, .sv = { it, it } };
		}

		////////////////
		// Lexer Core //
		////////////////

		// With fixup
		[[nodiscard]] std::optional<Symbol> peek_opt(SymbolFixup fixup) {
			if (not str_peek()) {
				return std::nullopt;
			}

			return Symbol { .kind = fixup(lookahead.kind), .sv = lookahead.sv };
		}

		[[nodiscard]] bool peek_is(SymbolPredicate cond, SymbolFixup fixup) {
			auto next = peek_opt(fixup);

			if (not next or not cond(next.value().kind)) {
				return false;
			}

			return true;
		}

		[[nodiscard]] bool peek_is_kind(SymbolKind kind, SymbolFixup fixup) {
			auto next = peek_opt(fixup);

			if (not next or kind != next.value().kind) {
				return false;
			}

			return true;
		}

		// Without fixup
		[[nodiscard]] std::optional<Symbol> peek_opt() {
			return peek_opt([](auto x) { return x; });
		}

		[[nodiscard]] bool peek_is(SymbolPredicate cond) {
			return peek_is(cond, [](auto x) { return x; });
		}

		[[nodiscard]] bool peek_is_kind(SymbolKind kind) {
			return peek_is_kind(kind, [](auto x) { return x; });
		}

		//////////////
		// Take Any //
		//////////////

		[[nodiscard]] std::optional<Symbol> take_any_opt(SymbolFixup fixup) {
			SymbolKind kind = SymbolKind::None;
			std::string_view sv = DEFAULT_SYMBOL_SV;

			produce_eof()
				.or_else([&] { return produce_whitespace(); })
				.or_else([&] { return produce_comment(); })
				.or_else([&] { return produce_sigil(); })
				.or_else([&] { return produce_identifier(); })
				.or_else([&] { return produce_number(); })
				.or_else([&] { return produce_quoted(); })

				// Success case, we assign `kind` and `sv`.
				.transform([&](auto x) {
					kind = x.kind;
					sv = x.sv;
					return x;
				})

				// Error case, we die.
				.or_else([&] {
					char c = *it;

					cane::report(
						ReportKind::Lexical,
						"unknown character '{}'({})",
						c == '\0' ? ' ' : c,
						static_cast<uint8_t>(c)
					);

					return std::optional<Symbol> {};
				});

			auto out =
				Symbol { .kind = fixup(lookahead.kind), .sv = lookahead.sv };

			lookahead = Symbol { .kind = kind, .sv = sv };

			// CANE_OKAY("current = {}, lookahead = {}", out, lookahead);
			return out;
		}

		[[nodiscard]] std::optional<Symbol> take_any_opt() {
			return take_any_opt([](auto x) { return x; });
		}

		//////////
		// Take //
		//////////

		// Filter out whitespace and comments.
		[[nodiscard]] std::optional<Symbol> take_opt(SymbolFixup fixup) {
			while (true) {
				if (not(produce_whitespace() or produce_comment())) {
					break;
				}
			}

			return take_any_opt(fixup);
		}

		[[nodiscard]] std::optional<Symbol> take_opt() {
			return take_opt([](auto x) { return x; });
		}

		///////////////////////
		// Take If Predicate //
		///////////////////////

		[[nodiscard]] std::optional<Symbol>
		take_if_opt(SymbolPredicate pred, SymbolFixup fixup) {
			auto next = peek_opt(fixup);

			if (not next or not pred(next.value().kind)) {
				return std::nullopt;
			}

			return take_opt(fixup);
		}

		[[nodiscard]] std::optional<Symbol> take_if_opt(SymbolPredicate pred) {
			return take_if_opt(pred, [](auto x) { return x; });
		}

		//////////////////
		// Take If Kind //
		//////////////////

		[[nodiscard]] std::optional<Symbol>
		take_if_kind_opt(SymbolKind kind, SymbolFixup fixup) {
			auto next = peek_opt(fixup);

			if (not next or kind != next.value().kind) {
				return std::nullopt;
			}

			return take_opt(fixup);
		}

		[[nodiscard]] std::optional<Symbol> take_if_kind_opt(SymbolKind kind) {
			return take_if_kind_opt(kind, [](auto x) { return x; });
		}

		///////////////
		// Unwrapped //
		///////////////

		// Peek
		[[nodiscard]] Symbol peek() {
			return peek_opt().value();
		}

		[[nodiscard]] Symbol peek(SymbolFixup fixup) {
			return peek_opt(fixup).value();
		}

		// Take Any
		[[nodiscard]] Symbol take_any() {
			return take_any_opt().value();
		}

		[[nodiscard]] Symbol take_any(SymbolFixup fixup) {
			return take_any_opt(fixup).value();
		}

		// Take
		[[nodiscard]] Symbol take() {
			return take_opt().value();
		}

		[[nodiscard]] Symbol take(SymbolFixup fixup) {
			return take_opt(fixup).value();
		}

		// Take If
		[[nodiscard]] Symbol take_if(SymbolPredicate pred) {
			return take_if_opt(pred).value();
		}

		[[nodiscard]] Symbol take_if(SymbolPredicate pred, SymbolFixup fixup) {
			return take_if_opt(pred, fixup).value();
		}

		// Take If Kind
		[[nodiscard]] Symbol take_if_kind(SymbolKind kind) {
			return take_if_kind_opt(kind).value();
		}

		[[nodiscard]] Symbol take_if_kind(SymbolKind kind, SymbolFixup fixup) {
			return take_if_kind_opt(kind, fixup).value();
		}

		/////////////
		// Discard //
		/////////////

		bool discard() {
			return take_opt().has_value();
		}

		bool discard_if(SymbolPredicate pred) {
			return take_if_opt(pred).has_value();
		}

		bool discard_if_kind(SymbolKind kind) {
			return take_if_kind_opt(kind).has_value();
		}
	};

}  // namespace cane

#endif
