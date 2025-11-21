#ifndef CANE_PARSE_HPP
#define CANE_PARSE_HPP

#include <string_view>
#include <memory>

#include <cane/macro.hpp>
#include <cane/enum.hpp>
#include <cane/log.hpp>
#include <cane/util.hpp>
#include <cane/lex.hpp>

namespace cane {
	//////////////////////
	// Symbol Remapping //
	//////////////////////

	// Remap symbols based on their position.
	// Makes it easier to reason about operator kinds during parsing and when
	// constructing the AST.

#define CANE_SYMBOL_OPFIX \
	X(OpfixKind::Prefix, SymbolKind::Ampersand, SymbolKind::Coerce) \
\
	X(OpfixKind::Prefix, SymbolKind::Dot, SymbolKind::Rest) \
	X(OpfixKind::Prefix, SymbolKind::Exclaim, SymbolKind::Beat) \
\
	X(OpfixKind::Prefix, SymbolKind::Tilda, SymbolKind::Invert) \
	X(OpfixKind::Prefix, SymbolKind::Quote, SymbolKind::Reverse) \
\
	X(OpfixKind::Prefix, SymbolKind::Add, SymbolKind::Abs) \
	X(OpfixKind::Prefix, SymbolKind::Sub, SymbolKind::Neg) \
\
	X(OpfixKind::Prefix, SymbolKind::Backslash, SymbolKind::Function) \
	X(OpfixKind::Prefix, SymbolKind::LeftBracket, SymbolKind::Layer) \
\
	X(OpfixKind::Infix, SymbolKind::Colon, SymbolKind::Euclidean) \
	X(OpfixKind::Infix, SymbolKind::Stars, SymbolKind::Repeat) \
	X(OpfixKind::Infix, SymbolKind::At, SymbolKind::Map) \
	X(OpfixKind::Infix, SymbolKind::Comma, SymbolKind::Concatenate) \
	X(OpfixKind::Infix, SymbolKind::Question, SymbolKind::Random) \
\
	X(OpfixKind::Infix, SymbolKind::LeftChevron, SymbolKind::LeftShift) \
	X(OpfixKind::Infix, SymbolKind::RightChevron, SymbolKind::RightShift) \
\
	X(OpfixKind::Infix, SymbolKind::FatArrow, SymbolKind::Assign) \
	X(OpfixKind::Infix, SymbolKind::WiggleArrow, SymbolKind::Send) \
	X(OpfixKind::Infix, SymbolKind::LeftParen, SymbolKind::Call)

	constexpr SymbolKind fix_symbol(OpfixKind opfix, SymbolKind kind) {
#define X(o, f, t) \
	if (o == opfix && f == kind) { \
		return t; \
	}

		CANE_SYMBOL_OPFIX;
		return kind;
#undef X
	}

	// Wrappers so we can pass them directly to various lexer utility functions.
	constexpr SymbolKind fix_prefix_symbol(SymbolKind kind) {
		return fix_symbol(OpfixKind::Prefix, kind);
	}

	constexpr SymbolKind fix_infix_symbol(SymbolKind kind) {
		return fix_symbol(OpfixKind::Infix, kind);
	}

	constexpr SymbolKind fix_postfix_symbol(SymbolKind kind) {
		return fix_symbol(OpfixKind::Postfix, kind);
	}

	constexpr SymbolKind fix_unary_symbol(SymbolKind kind) {
		return fix_symbol(OpfixKind::Prefix, kind);
	}

	constexpr SymbolKind fix_binary_symbol(SymbolKind kind) {
		kind = fix_symbol(OpfixKind::Infix, kind);
		kind = fix_symbol(OpfixKind::Postfix, kind);

		return kind;
	}

	/////////////////////////////////////////
	// Binding Power / Operator Precedence //
	/////////////////////////////////////////

	constexpr std::pair<size_t, size_t> binding_power(SymbolKind kind) {
		// TODO: Combine this with symbol definition X macros aswell as
		// remapping macros

		// clang-format off

#define CANE_BINDING_POWERS \
	X(SymbolKind::Send,        1, 2) \
	X(SymbolKind::Map,         2, 3) \
\
	X(SymbolKind::Concatenate, 3, 4) \
\
	X(SymbolKind::Call,        4, 5) \
	X(SymbolKind::Assign,      5, 6) \
\
	X(SymbolKind::Or,          6, 7) \
	X(SymbolKind::And,         6, 7) \
	X(SymbolKind::Xor,         6, 7) \
	X(SymbolKind::Repeat,      6, 7) \
	X(SymbolKind::LeftShift,   6, 7) \
	X(SymbolKind::RightShift,  6, 7) \
\
	X(SymbolKind::Invert,      9, 9) \
	X(SymbolKind::Reverse,     9, 9) \
\
	X(SymbolKind::Add,         10, 11) \
	X(SymbolKind::Sub,         10, 11) \
\
	X(SymbolKind::Mul,         11, 12) \
	X(SymbolKind::Div,         11, 12) \
\
	X(SymbolKind::Euclidean,   12, 13) \
\
	X(SymbolKind::LCM,         13, 14) \
	X(SymbolKind::GCD,         13, 14) \
\
	X(SymbolKind::Random,      14, 15) \
\
	X(SymbolKind::Abs,         15, 15) \
	X(SymbolKind::Neg,         15, 15)

		// clang-format on

#define X(symbol, lbp, rbp) \
	case symbol: return { lbp, rbp }; break;

		switch (kind) {
			CANE_BINDING_POWERS;

			default: {
				CANE_UNREACHABLE();
			} break;
		}

		return { 0, 0 };

#undef X
#undef CANE_BINDING_POWERS
	}

	/////////
	// AST //
	/////////

	struct Node {
		SymbolKind kind;
		SymbolKind op;

		std::string_view sv;

		TypeKind type;

		std::shared_ptr<Node> lhs;
		std::shared_ptr<Node> rhs;

		Node(SymbolKind kind_, std::string_view sv_, TypeKind type_):
				kind(kind_),
				op(SymbolKind::None),
				sv(sv_),
				type(type_),
				lhs(nullptr),
				rhs(nullptr) {}

		Node(
			SymbolKind kind_,
			std::string_view sv_,

			TypeKind type_,

			std::shared_ptr<Node> lhs_,
			std::shared_ptr<Node> rhs_
		):
				kind(kind_),
				op(SymbolKind::None),
				sv(sv_),
				type(type_),
				lhs(lhs_),
				rhs(rhs_) {}
	};
}  // namespace cane

template <>
struct std::formatter<cane::Node>: std::formatter<std::string_view> {
	auto format(cane::Node x, format_context& ctx) const {
		return formatter<std::string_view>::format(
			std::format(
				"{{ kind: {}, op: {}, sv: '{}', type: {}, lhs: {}, rhs: {} }}",
				x.kind,
				x.op,
				x.sv,
				x.type,

				static_cast<void*>(x.lhs.get()),
				static_cast<void*>(x.rhs.get())
			),
			ctx
		);
	}
};

namespace cane {

	////////////
	// PARSER //
	////////////

	constexpr bool is_literal(SymbolKind kind) {
		return eq_any(
			kind,
			SymbolKind::Number,
			SymbolKind::String,

			SymbolKind::Rest,
			SymbolKind::Beat
		);
	}

	constexpr bool is_primary(SymbolKind kind) {
		return is_literal(kind) or
			eq_any(kind,
				   SymbolKind::Coerce,
				   SymbolKind::Function,
				   SymbolKind::Identifier,
				   SymbolKind::LeftParen,
				   SymbolKind::Layer);
	}

	constexpr bool is_prefix(SymbolKind kind) {
		return eq_any(
			kind,
			SymbolKind::Abs,
			SymbolKind::Neg,
			SymbolKind::Invert,
			SymbolKind::Reverse
		);
	}

	constexpr bool is_infix(SymbolKind kind) {
		return eq_any(
			kind,
			// Arithmetic
			SymbolKind::Add,
			SymbolKind::Sub,
			SymbolKind::Mul,
			SymbolKind::Div,

			SymbolKind::LCM,
			SymbolKind::GCD,

			// Misc.
			SymbolKind::Euclidean,    // Euclide
			SymbolKind::Repeat,       // Repeat
			SymbolKind::Map,          // Map
			SymbolKind::Concatenate,  // Concatenate
			SymbolKind::Random,       // Random

			// Logic
			SymbolKind::Or,
			SymbolKind::Xor,
			SymbolKind::And,

			// Left/Right Shift
			SymbolKind::LeftShift,
			SymbolKind::RightShift,

			SymbolKind::Assign,  // Assignment
			SymbolKind::Call,    // Function call
			SymbolKind::Send     // Send to channel
		);
	}

	constexpr bool is_postfix(SymbolKind kind) {
		// return eq_any(kind, );
		return false;
	}

	constexpr bool is_unary(SymbolKind kind) {
		return is_prefix(kind);
	}

	constexpr bool is_binary(SymbolKind kind) {
		return is_infix(kind) or is_postfix(kind);
	}

	class Parser {
		private:
		Lexer lx;

		public:
		Parser(std::string_view sv): lx(sv) {}

		//////////////////////
		// PARSER FUNCTIONS //
		//////////////////////

		// Core parsing functions
		std::shared_ptr<Node> parse() {
			CANE_FUNC();

			std::shared_ptr<Node> root = nullptr;

			while (not lx.peek_is_kind(SymbolKind::EndFile)) {
				auto stmt = lx.peek();
				auto node = expression();

				root = std::make_shared<Node>(
					SymbolKind::Statement, stmt.sv, TypeKind::None, node, root
				);

				// Statements must be terminated by a semicolon unless they are
				// EOF.
				if (not lx.discard_if_kind(SymbolKind::Semicolon) and
					not lx.discard_if_kind(SymbolKind::EndFile)) {
					cane::die("expected `;`");
				}
			}

			if (not lx.discard_if_kind(SymbolKind::EndFile)) {
				cane::die("expected end of file");
			}

			return root;
		}

		// Expression parsing
		std::optional<TypeKind> type_annotation() {
			CANE_FUNC();

			if (lx.discard_if_kind(SymbolKind::AnnotationNumber)) {
				return TypeKind::Scalar;
			}

			else if (lx.discard_if_kind(SymbolKind::AnnotationString)) {
				return TypeKind::String;
			}

			else if (lx.discard_if_kind(SymbolKind::AnnotationRhythm)) {
				return TypeKind::Rhythm;
			}

			else if (lx.discard_if_kind(SymbolKind::AnnotationMelody)) {
				return TypeKind::Melody;
			}

			else if (lx.discard_if_kind(SymbolKind::AnnotationSequence)) {
				return TypeKind::Sequence;
			}

			else if (lx.discard_if_kind(SymbolKind::AnnotationPattern)) {
				return TypeKind::Pattern;
			}

			return std::nullopt;
		}

		std::optional<TypeKind> type() {
			CANE_FUNC();

			if (not lx.discard_if_kind(SymbolKind::Arrow)) {
				return std::nullopt;
			}

			return type_annotation();
		}

		std::shared_ptr<Node> primary(Symbol symbol) {
			CANE_FUNC();

			switch (symbol.kind) {
				// Literals
				case SymbolKind::Identifier: {
					lx.discard();
					return std::make_shared<Node>(
						symbol.kind, symbol.sv, TypeKind::None
					);
				}

				case SymbolKind::String: {
					lx.discard();
					return std::make_shared<Node>(
						symbol.kind, symbol.sv, TypeKind::String
					);
				}

				case SymbolKind::Number: {
					auto number = lx.take();

					auto root = std::make_shared<Node>(
						symbol.kind, number.sv, TypeKind::Scalar
					);

					while (lx.peek_is_kind(SymbolKind::Number)) {
						auto number = lx.take();

						auto node = std::make_shared<Node>(
							symbol.kind, number.sv, TypeKind::Scalar
						);

						root = std::make_shared<Node>(
							SymbolKind::Concatenate,
							number.sv,
							TypeKind::Melody,
							root,
							node
						);
					}

					return root;
				}

				// Literals (Implicit Concat)
				case SymbolKind::Beat:
				case SymbolKind::Rest: {
					lx.discard();

					auto root = std::make_shared<Node>(
						symbol.kind, symbol.sv, TypeKind::Rhythm
					);

					while (
						lx.peek_is_kind(SymbolKind::Beat, fix_unary_symbol) or
						lx.peek_is_kind(SymbolKind::Rest, fix_unary_symbol)
					) {
						auto beat = lx.take(fix_unary_symbol);

						auto node = std::make_shared<Node>(
							beat.kind, beat.sv, TypeKind::Rhythm
						);

						root = std::make_shared<Node>(
							SymbolKind::Concatenate,
							beat.sv,
							TypeKind::Rhythm,
							root,
							node
						);
					}

					return root;
				} break;

				// Melody Coercion (Convert a single scalar to a melody)
				case SymbolKind::Coerce: {
					lx.discard();  // Skip `&`
					auto expr = expression();

					return std::make_shared<Node>(
						SymbolKind::Coerce,
						symbol.sv,
						TypeKind::None,
						nullptr,
						expr
					);
				} break;

				case SymbolKind::LeftParen: {
					lx.discard();  // Skip `(`

					auto expr = expression();

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::die("expected `)`");
					}

					return expr;
				} break;

				case SymbolKind::Layer: {
					lx.discard();  // Skip `[`
					std::shared_ptr<Node> root = nullptr;

					// Need at least one expression.
					do {
						auto node = expression();

						root = std::make_shared<Node>(
							SymbolKind::Layer,
							symbol.sv,
							TypeKind::None,
							node,
							root
						);

						lx.discard_if_kind(SymbolKind::Comma);
					} while (not lx.peek_is_kind(SymbolKind::RightBracket));

					if (not lx.discard_if_kind(SymbolKind::RightBracket)) {
						cane::die("expected `]`");
					}

					return root;
				} break;

				case SymbolKind::Function: {
					lx.discard();  // Skip `\`

					// Parameter
					auto identifier =
						lx.take_if_kind_opt(SymbolKind::Identifier);

					if (not identifier.has_value()) {
						cane::die("expected an identifier");
					}

					auto param = std::make_shared<Node>(
						SymbolKind::Identifier,
						identifier.value().sv,
						TypeKind::None
					);

					// Parameter type
					auto param_type = type();
					if (not param_type.has_value()) {
						cane::die("expected a type annotation");
					}

					// Reset binding power and parse body
					auto body = expression();

					// Body type
					auto body_type = type();
					if (not body_type.has_value()) {
						cane::die("expected a type annotation");
					}

					auto root = std::make_shared<Node>(
						SymbolKind::Function, symbol.sv, body->type
					);

					param->type = param_type.value();
					body->type = body_type.value();

					root->lhs = param;
					root->rhs = body;

					return root;
				} break;

				default: break;
			}

			cane::die("expected a primary expression");
			return nullptr;
		}

		std::shared_ptr<Node> prefix(Symbol symbol, size_t bp) {
			CANE_FUNC();

			// We need to call this function directly instead of using something
			// like `cane_lexer_discard_if` because we have fixed up the symbol
			// earlier and peeking again would return the incorrect/lexical
			// token kind instead.
			if (not is_prefix(symbol.kind)) {
				cane::die("expected a prefix operator");
			}

			lx.discard();
			auto expr = expression(bp);

			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, expr, nullptr
			);
		}

		std::shared_ptr<Node>
		infix(Symbol symbol, std::shared_ptr<Node> lhs, size_t bp) {
			CANE_FUNC();

			if (not is_infix(symbol.kind)) {
				cane::die("expected an infix operator");
			}

			lx.discard();
			auto rhs = expression(bp);

			// Special cases
			switch (symbol.kind) {
				case SymbolKind::Call: {
					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::die("expecting `)`");
					}

					return std::make_shared<Node>(
						symbol.kind, symbol.sv, TypeKind::None, lhs, rhs
					);
				}

				default: break;
			}

			// Normal case
			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, lhs, rhs
			);
		}

		std::shared_ptr<Node>
		postfix(Symbol symbol, std::shared_ptr<Node> lhs) {
			CANE_FUNC();

			if (not is_postfix(symbol.kind)) {
				cane::die("expected a postfix operator");
			}

			auto node = std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, lhs, /* rhs = */ nullptr
			);

			lx.discard();
			return node;
		}

		std::shared_ptr<Node> expression(size_t min_bp = 0) {
			CANE_FUNC();

			Symbol symbol = lx.peek(fix_unary_symbol);
			std::shared_ptr<Node> node = nullptr;

			if (is_primary(symbol.kind)) {
				node = primary(symbol);
			}

			else if (is_prefix(symbol.kind)) {
				auto [lbp, rbp] = binding_power(symbol.kind);
				node = prefix(symbol, rbp);
			}

			else {
				cane::die("expected a primary expression or a prefix operator");
			}

			// State has changed since we called prefix/primary parser functions
			// so we need to peek again.
			symbol = lx.peek(fix_binary_symbol);

			while (is_infix(symbol.kind) or is_postfix(symbol.kind)) {
				auto [lbp, rbp] = binding_power(symbol.kind);

				if (lbp < min_bp) {
					break;
				}

				if (is_postfix(symbol.kind)) {
					node = postfix(symbol, node);
				}

				else if (is_infix(symbol.kind)) {
					node = infix(symbol, node, rbp);
				}

				else {
					cane::die("expected an infix or postfix operator");
				}

				symbol = lx.peek(fix_binary_symbol);
			}

			return node;
		}
	};

}  // namespace cane

#endif
