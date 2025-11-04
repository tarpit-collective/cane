#ifndef CANE_PARSE_HPP
#define CANE_PARSE_HPP

#include <string_view>
#include <memory>
#include <variant>

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
	X(OpfixKind::Postfix, SymbolKind::FatArrow, SymbolKind::Assign) \
	X(OpfixKind::Postfix, SymbolKind::WiggleArrow, SymbolKind::Send) \
	X(OpfixKind::Postfix, SymbolKind::LeftParen, SymbolKind::Call)

	constexpr SymbolKind cane_fix_symbol(OpfixKind opfix, SymbolKind kind) {
#define X(o, f, t) \
	if (o == opfix && f == kind) { \
		return t; \
	}

		CANE_SYMBOL_OPFIX;
		return kind;
#undef X
	}

	// Wrappers so we can pass them directly to various lexer utility functions.
	constexpr SymbolKind cane_fix_prefix_symbol(SymbolKind kind) {
		return cane_fix_symbol(OpfixKind::Prefix, kind);
	}

	constexpr SymbolKind cane_fix_infix_symbol(SymbolKind kind) {
		return cane_fix_symbol(OpfixKind::Infix, kind);
	}

	constexpr SymbolKind cane_fix_postfix_symbol(SymbolKind kind) {
		return cane_fix_symbol(OpfixKind::Postfix, kind);
	}

	constexpr SymbolKind cane_fix_unary_symbol(SymbolKind kind) {
		return cane_fix_symbol(OpfixKind::Prefix, kind);
	}

	constexpr SymbolKind cane_fix_binary_symbol(SymbolKind kind) {
		kind = cane_fix_symbol(OpfixKind::Infix, kind);
		kind = cane_fix_symbol(OpfixKind::Postfix, kind);

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
	X(SymbolKind::Call,        1, 2) \
	X(SymbolKind::Assign,      2, 3) \
\
	X(SymbolKind::Or,          3, 4) \
	X(SymbolKind::And,         3, 4) \
	X(SymbolKind::Xor,         3, 4) \
	X(SymbolKind::Repeat,      3, 4) \
	X(SymbolKind::LeftShift,   3, 4) \
	X(SymbolKind::RightShift,  3, 4) \
\
	X(SymbolKind::Map,         4, 5) \
\
	X(SymbolKind::Concatenate, 5, 6) \
\
	X(SymbolKind::Invert,      6, 6) \
	X(SymbolKind::Reverse,     6, 6) \
\
	X(SymbolKind::Add,         7, 8) \
	X(SymbolKind::Sub,         7, 8) \
\
	X(SymbolKind::Mul,         8, 9) \
	X(SymbolKind::Div,         8, 9) \
\
	X(SymbolKind::Euclidean,   9, 10) \
\
	X(SymbolKind::LCM,         10, 11) \
	X(SymbolKind::GCD,         10, 11) \
\
	X(SymbolKind::Random,      11, 12) \
\
	X(SymbolKind::Abs,         12, 12) \
	X(SymbolKind::Neg,         12, 12)

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

	struct ASTNode {
		using Ptr = std::shared_ptr<ASTNode>;
		using Binary = std::pair<Ptr, Ptr>;

		SymbolKind kind;
		TypeKind type;

		Ptr lhs;
		Ptr rhs;

		std::
			variant<std::monostate, std::string_view, int, std::vector<uint8_t>>
				value;

		ASTNode(SymbolKind kind_, TypeKind type_):
				kind(kind_),
				type(type_),
				lhs(nullptr),
				rhs(nullptr),
				value(std::monostate {}) {}

		ASTNode(SymbolKind kind_, int number):
				kind(kind_),
				type(TypeKind::Scalar),
				lhs(nullptr),
				rhs(nullptr),
				value(number) {}

		ASTNode(SymbolKind kind_, std::string_view sv):
				kind(kind_),
				type(TypeKind::String),
				lhs(nullptr),
				rhs(nullptr),
				value(sv) {}

		ASTNode(SymbolKind kind_, TypeKind type_, std::vector<uint8_t> vec):
				kind(kind_),
				type(type_),
				lhs(nullptr),
				rhs(nullptr),
				value(vec) {}

		ASTNode(SymbolKind kind_, TypeKind type_, Ptr lhs_, Ptr rhs_):
				kind(kind_),
				type(type_),
				lhs(lhs_),
				rhs(rhs_),
				value(std::monostate {}) {}

		ASTNode(SymbolKind kind_, TypeKind type_, Ptr node):
				kind(kind_),
				type(type_),
				lhs(nullptr),
				rhs(node),
				value(std::monostate {}) {}
	};

	////////////
	// PARSER //
	////////////

	class Parser {
		private:
		Lexer lx;

		public:
		Parser(std::string_view sv): lx(sv) {}

		/////////////////
		// Classifiers //
		/////////////////

		// Note: these functions use the symbol kind _after_ remapping.

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
				SymbolKind::RightShift
			);
		}

		constexpr bool is_postfix(SymbolKind kind) {
			return eq_any(
				kind,
				SymbolKind::Assign,  // Assignment
				SymbolKind::Call,    // Function call
				SymbolKind::Send     // Send to channel
			);
		}

		constexpr bool is_unary(SymbolKind kind) {
			return is_prefix(kind);
		}

		constexpr bool is_binary(SymbolKind kind) {
			return is_infix(kind) or is_postfix(kind);
		}

		//////////////////////
		// PARSER FUNCTIONS //
		//////////////////////

		// Core parsing functions
		std::shared_ptr<ASTNode> parse() {
			std::shared_ptr<ASTNode> root = nullptr;

			while (not lx.peek_is_kind(SymbolKind::EndFile)) {
				std::shared_ptr<ASTNode> node = expression(0);

				std::shared_ptr<ASTNode> concat = std::make_shared<ASTNode>(
					SymbolKind::Statement, TypeKind::None, node, root
				);

				root = concat;

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
			if (not lx.discard_if_kind(SymbolKind::Arrow)) {
				return std::nullopt;
			}

			return type_annotation();
		}

		std::shared_ptr<ASTNode> primary(Symbol symbol) {
			switch (symbol.kind) {
				// Literals
				case SymbolKind::Identifier: {
					lx.discard();
					return std::make_shared<ASTNode>(symbol.kind, symbol.sv);
				}

				case SymbolKind::String: {
					lx.discard();
					return std::make_shared<ASTNode>(symbol.kind, symbol.sv);
				}

				case SymbolKind::Number: {
					auto number = lx.take();

					// TODO: Convert to integer.
					std::shared_ptr<ASTNode> root =
						std::make_shared<ASTNode>(symbol.kind, 0);

					// TODO: Parse adjacent numbers as a melody.
					// TODO: Create vector of melody values
					while (lx.peek_is_kind(SymbolKind::Number)) {
						number = lx.take();
					}

					return root;
				}

				// Literals (Implicit Concat)
				case SymbolKind::Beat:
				case SymbolKind::Rest: {
					lx.discard();

					std::vector<uint8_t> vec;
					std::shared_ptr<ASTNode> rhythm = std::make_shared<ASTNode>(
						SymbolKind::Rhythm, TypeKind::Rhythm, vec
					);

					while (
						lx.peek_is_kind(
							SymbolKind::Beat, cane_fix_unary_symbol
						) or
						lx.peek_is_kind(SymbolKind::Rest, cane_fix_unary_symbol)
					) {
						// TODO: Create vector of rhythm values.
						lx.take(cane_fix_unary_symbol);
					}

					return rhythm;
				} break;

				// Melody Coercion (Convert a single scalar to a melody)
				case SymbolKind::Coerce: {
					lx.discard();  // Skip `&`

					std::shared_ptr<ASTNode> expr = expression(0);

					return std::make_shared<ASTNode>(
						SymbolKind::Coerce, TypeKind::Melody, expr
					);
				} break;

				case SymbolKind::LeftParen: {
					lx.discard();  // Skip `(`

					std::shared_ptr<ASTNode> expr = expression(0);

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::die("expected `)`");
					}

					return expr;
				} break;

				case SymbolKind::Layer: {
					lx.discard();  // Skip `[`
					std::shared_ptr<ASTNode> root = NULL;

					// Need at least one expression.
					do {
						std::shared_ptr<ASTNode> node = expression(0);

						std::shared_ptr<ASTNode> layer =
							std::make_shared<ASTNode>(
								SymbolKind::Layer, TypeKind::None, node, root
							);

						root = layer;

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
					// cane_symbol_t ident;
					auto identifier =
						lx.take_if_kind_opt(SymbolKind::Identifier);

					if (not identifier.has_value()) {
						cane::die("expected an identifier");
					}

					std::shared_ptr<ASTNode> param = std::make_shared<ASTNode>(
						SymbolKind::Identifier, identifier.value().sv
					);

					// Parameter type
					auto param_type = type();
					if (not param_type.has_value()) {
						cane::die("expected a type annotation");
					}

					// Reset binding power and parse body
					std::shared_ptr<ASTNode> body = expression(0);

					// Body type
					auto body_type = type();
					if (not body_type.has_value()) {
						cane::die("expected a type annotation");
					}

					std::shared_ptr<ASTNode> fn = std::make_shared<ASTNode>(
						SymbolKind::Function, body->type
					);

					param->type = param_type.value();
					body->type = body_type.value();

					fn->lhs = param;
					fn->rhs = body;

					return fn;
				} break;

				default: break;
			}

			cane::die("expected a primary expression");
			return nullptr;
		}

		std::shared_ptr<ASTNode> prefix(Symbol symbol, size_t bp) {
			// We need to call this function directly instead of using something
			// like `cane_lexer_discard_if` because we have fixed up the symbol
			// earlier and peeking again would return the incorrect/lexical
			// token kind instead.
			if (not is_prefix(symbol.kind)) {
				cane::die("expected a prefix operator");
			}

			lx.discard();
			std::shared_ptr<ASTNode> expr = expression(bp);

			return std::make_shared<ASTNode>(symbol.kind, TypeKind::None, expr);
		}

		std::shared_ptr<ASTNode>
		infix(Symbol symbol, std::shared_ptr<ASTNode> lhs, size_t bp) {
			if (not is_infix(symbol.kind)) {
				cane::die("expected an infix operator");
			}

			lx.discard();

			std::shared_ptr<ASTNode> rhs = expression(bp);

			return std::make_shared<ASTNode>(
				symbol.kind, TypeKind::None, lhs, rhs
			);
		}

		std::shared_ptr<ASTNode>
		postfix(Symbol symbol, std::shared_ptr<ASTNode> lhs) {
			if (not is_postfix(symbol.kind)) {
				cane::die("expected a postfix operator");
			}

			std::shared_ptr<ASTNode> node = std::make_shared<ASTNode>(
				symbol.kind, TypeKind::None, lhs, /* rhs = */ nullptr
			);

			lx.discard();

			// Assignment is a bit special in that it has a parameter in the
			// form of an identifier to bind the expression's value to.
			switch (symbol.kind) {
				case SymbolKind::Assign: {
					auto identifier =
						lx.take_if_kind_opt(SymbolKind::Identifier);

					if (not identifier.has_value()) {
						cane::die("expected an identifier");
					}

					node->rhs = std::make_shared<ASTNode>(
						SymbolKind::Identifier, TypeKind::None
					);
				} break;

				case SymbolKind::Send: {
					auto channel = lx.take_if_kind_opt(SymbolKind::String);

					if (not channel.has_value()) {
						cane::die("expected a string");
					}

					node->rhs = std::make_shared<ASTNode>(
						SymbolKind::Send, TypeKind::None
					);
				} break;

				case SymbolKind::Call: {
					std::shared_ptr<ASTNode> expr = expression(0);

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::die("expecting `)`");
					}

					node->rhs = expr;
				}

				default: break;
			}

			return node;
		}

		std::shared_ptr<ASTNode> expression(size_t min_bp) {
			std::shared_ptr<ASTNode> node = NULL;

			Symbol symbol = lx.peek(cane_fix_unary_symbol);

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
			symbol = lx.peek(cane_fix_binary_symbol);

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

				symbol = lx.peek(cane_fix_binary_symbol);
			}

			return node;
		}
	};

}  // namespace cane

#endif
