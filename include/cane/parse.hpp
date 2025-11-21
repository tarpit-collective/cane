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
	X(SymbolKind::Neg,         15, 15) \
\
	X(SymbolKind::Incr,        16, 16) \
	X(SymbolKind::Decr,        16, 16) \
\
	X(SymbolKind::Coerce,      17, 17)

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
				   SymbolKind::Function,
				   SymbolKind::Identifier,
				   SymbolKind::LeftParen);
	}

	constexpr bool is_prefix(SymbolKind kind) {
		return eq_any(
			kind,
			SymbolKind::Abs,
			SymbolKind::Neg,

			SymbolKind::Invert,
			SymbolKind::Reverse,

			SymbolKind::Coerce
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
			SymbolKind::Euclidean,
			SymbolKind::Repeat,
			SymbolKind::Map,
			SymbolKind::Concatenate,
			SymbolKind::Layer,
			SymbolKind::Random,

			// Logic
			SymbolKind::Or,
			SymbolKind::Xor,
			SymbolKind::And,

			// Left/Right Shift
			SymbolKind::LeftShift,
			SymbolKind::RightShift,

			SymbolKind::Assign,
			SymbolKind::Call,
			SymbolKind::Send
		);
	}

	constexpr bool is_postfix(SymbolKind kind) {
		return eq_any(kind, SymbolKind::Incr, SymbolKind::Decr);
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
				auto node = parse_expression();

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
		std::optional<TypeKind> parse_type_annotation() {
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

		std::optional<TypeKind> parse_type() {
			CANE_FUNC();

			if (not lx.discard_if_kind(SymbolKind::Arrow)) {
				return std::nullopt;
			}

			return parse_type_annotation();
		}

		std::shared_ptr<Node> parse_primary(Symbol symbol) {
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

					while (lx.peek_is_kind(SymbolKind::Beat) or
						   lx.peek_is_kind(SymbolKind::Rest)) {
						auto beat = lx.take();

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
					// case SymbolKind::Coerce: {
					// 	lx.discard();  // Skip `&`
					// 	auto expr = parse_expression();

					// 	return std::make_shared<Node>(
					// 		SymbolKind::Coerce,
					// 		symbol.sv,
					// 		TypeKind::None,
					// 		nullptr,
					// 		expr
					// 	);
					// } break;

				case SymbolKind::LeftParen: {
					lx.discard();  // Skip `(`

					auto expr = parse_expression();

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::die("expected `)`");
					}

					return expr;
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
					auto param_type = parse_type();
					if (not param_type.has_value()) {
						cane::die("expected a type annotation");
					}

					// Reset binding power and parse body
					auto body = parse_expression();

					// Body type
					auto body_type = parse_type();
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

		std::shared_ptr<Node> parse_prefix(Symbol symbol, size_t bp) {
			CANE_FUNC();

			// We need to call this function directly instead of using something
			// like `cane_lexer_discard_if` because we have fixed up the symbol
			// earlier and peeking again would return the incorrect/lexical
			// token kind instead.
			if (not is_prefix(symbol.kind)) {
				cane::die("expected a prefix operator");
			}

			lx.discard();
			auto expr = parse_expression(bp);

			return std::make_shared<Node>(
				symbol.kind,
				symbol.sv,
				TypeKind::None,
				/* lhs = */ expr,
				/* rhs = */ nullptr
			);
		}

		std::shared_ptr<Node>
		parse_infix(Symbol symbol, std::shared_ptr<Node> lhs, size_t bp) {
			CANE_FUNC();

			if (not is_infix(symbol.kind)) {
				cane::die("expected an infix operator");
			}

			lx.discard();
			auto rhs = parse_expression(bp);

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
		parse_postfix(Symbol symbol, std::shared_ptr<Node> lhs) {
			CANE_FUNC();

			if (not is_postfix(symbol.kind)) {
				cane::die("expected a postfix operator");
			}

			lx.discard();

			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, lhs, /* rhs = */ nullptr
			);
		}

		std::shared_ptr<Node> parse_expression(size_t min_bp = 0) {
			CANE_FUNC();

			Symbol symbol = lx.peek();
			std::shared_ptr<Node> node = nullptr;

			if (is_primary(symbol.kind)) {
				node = parse_primary(symbol);
			}

			else if (is_prefix(symbol.kind)) {
				auto [lbp, rbp] = binding_power(symbol.kind);
				node = parse_prefix(symbol, rbp);
			}

			else {
				cane::die("expected a primary expression or a prefix operator");
			}

			// State has changed since we called prefix/primary parser functions
			// so we need to peek again.
			symbol = lx.peek();

			while (is_infix(symbol.kind) or is_postfix(symbol.kind)) {
				auto [lbp, rbp] = binding_power(symbol.kind);

				if (lbp < min_bp) {
					break;
				}

				if (is_postfix(symbol.kind)) {
					node = parse_postfix(symbol, node);
				}

				else if (is_infix(symbol.kind)) {
					node = parse_infix(symbol, node, rbp);
				}

				else {
					cane::die("expected an infix or postfix operator");
				}

				symbol = lx.peek();
			}

			return node;
		}
	};

}  // namespace cane

#endif
