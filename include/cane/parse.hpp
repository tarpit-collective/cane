#ifndef CANE_PARSE_HPP
#define CANE_PARSE_HPP

#include <string_view>
#include <memory>

#include <cane/def.hpp>
#include <cane/log.hpp>
#include <cane/util.hpp>
#include <cane/lex.hpp>

namespace cane {

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

		Node(
			SymbolKind kind_,
			std::string_view sv_,

			TypeKind type_,

			std::shared_ptr<Node> lhs_
		):
				kind(kind_),
				op(SymbolKind::None),
				sv(sv_),
				type(type_),
				lhs(lhs_),
				rhs(nullptr) {}
	};
}  // namespace cane

// TODO: Implement operator<< overload for Node and then just reuse that for the
// formatter impl.
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

	using BoxNode = std::shared_ptr<Node>;
	using OptionalNode = std::optional<BoxNode>;

	class Parser {
		private:
		Lexer lx;

		public:
		Parser(std::string_view sv): lx(sv) {}

		//////////////////////
		// PARSER FUNCTIONS //
		//////////////////////

		// Core parsing functions
		[[nodiscard]] BoxNode parse() {
			CANE_FUNC();

			BoxNode root = nullptr;

			while (not lx.peek_is_kind(SymbolKind::EndFile)) {
				auto stmt = lx.peek();
				auto node = parse_expression();

				root = std::make_shared<Node>(
					SymbolKind::Statement, stmt.sv, TypeKind::None, root, node
				);

				// Statements must be terminated by a semicolon unless they are
				// EOF.
				if (not lx.discard_if_kind(SymbolKind::Semicolon) and
					not lx.discard_if_kind(SymbolKind::EndFile)) {
					cane::report(
						ReportKind::Syntactical, "expected `;` or end of file"
					);
				}
			}

			if (not lx.discard_if_kind(SymbolKind::EndFile)) {
				cane::report(ReportKind::Syntactical, "expected end of file");
			}

			return root;
		}

		// Expression parsing
		[[nodiscard]] std::optional<TypeKind> parse_type_annotation() {
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

		[[nodiscard]] std::optional<TypeKind> parse_type() {
			CANE_FUNC();

			if (not lx.discard_if_kind(SymbolKind::Arrow)) {
				return std::nullopt;
			}

			return parse_type_annotation();
		}

		[[nodiscard]] OptionalNode parse_primary() {
			CANE_FUNC();
			Symbol symbol = lx.peek();

			switch (symbol.kind) {
				// Literals
				case SymbolKind::Identifier: {
					lx.discard();
					return std::make_shared<Node>(
						SymbolKind::Identifier, symbol.sv, TypeKind::None
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

						root = std::make_shared<Node>(
							SymbolKind::Concatenate,
							beat.sv,
							TypeKind::Rhythm,

							// Children
							/* lhs = */ root,
							/* rhs = */
							std::make_shared<Node>(
								beat.kind, beat.sv, TypeKind::Rhythm
							)
						);
					}

					return root;
				} break;

				case SymbolKind::LeftParen: {
					lx.discard();  // Skip `(`

					auto expr = parse_expression();

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::report(ReportKind::Syntactical, "expected `)`");
					}

					return expr;
				} break;

				case SymbolKind::Function: {
					lx.discard();  // Skip `\`

					// Parameter
					auto identifier =
						lx.take_if_kind_opt(SymbolKind::Identifier);
					cane::report_if(
						not identifier.has_value(),
						ReportKind::Syntactical,
						"expected an identifier"
					);

					auto param = std::make_shared<Node>(
						SymbolKind::Identifier,
						identifier.value().sv,
						TypeKind::None
					);

					// Parameter type
					auto param_type = parse_type();
					cane::report_if(
						not param_type.has_value(),
						ReportKind::Syntactical,
						"expected a type annotation"
					);

					// Reset binding power and parse body
					auto body = parse_expression();

					// Body type
					auto body_type = parse_type();
					cane::report_if(
						not body_type.has_value(),
						ReportKind::Syntactical,
						"expected a type annotation"
					);

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

			return std::nullopt;
		}

		[[nodiscard]] OptionalNode parse_prefix() {
			CANE_FUNC();

			Symbol symbol = lx.peek();
			auto bp = binding_power(symbol.kind);

			if (not bp.has_value()) {
				return std::nullopt;
			}

			// Remapping
			switch (symbol.kind) {
				case SymbolKind::Add: symbol.kind = SymbolKind::Abs; break;
				case SymbolKind::Sub: symbol.kind = SymbolKind::Neg; break;
				default: break;
			}

			if (not is_prefix(symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();
			auto expr = parse_expression(bp.value().right);

			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, expr
			);
		}

		[[nodiscard]] OptionalNode parse_infix(BoxNode lhs, size_t bp) {
			CANE_FUNC();
			Symbol symbol = lx.peek();

			if (not is_infix(symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();
			auto rhs = parse_expression(bp);

			// Special cases.
			// switch (symbol.kind) {
			// 	case SymbolKind::Assign: {
			// 		// We need to assert that the `rhs` is an
			// 		// identifier because it doesn't make sense for
			// 		// the name to be anything other than identifier.
			// 		cane::report_if(
			// 			rhs->kind != SymbolKind::Identifier and
			// 				rhs->kind != SymbolKind::Reference,
			// 			ReportKind::Syntactical,
			// 			"expected an identifier"
			// 		);

			// 		rhs->kind = SymbolKind::Binding;
			// 	} break;

			// 	default: break;
			// }

			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, lhs, rhs
			);
		}

		[[nodiscard]] OptionalNode parse_postfix(BoxNode lhs) {
			CANE_FUNC();
			Symbol symbol = lx.peek();

			if (not is_postfix(symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();

			return std::make_shared<Node>(
				symbol.kind, symbol.sv, TypeKind::None, lhs
			);
		}

		[[nodiscard]] BoxNode parse_expression(size_t min_bp = 0) {
			CANE_FUNC();

			OptionalNode node =
				parse_primary().or_else([&] { return parse_prefix(); });

			cane::report_if(
				not node.has_value(),
				ReportKind::Syntactical,
				"expected a primary expression or a prefix operator"

			);

			while (is_infix(lx.peek().kind) or is_postfix(lx.peek().kind)) {
				auto bp = binding_power(lx.peek().kind);

				if (not bp.has_value() or bp.value().left < min_bp) {
					break;
				}

				node = parse_postfix(node.value()).or_else([&] {
					return parse_infix(node.value(), bp.value().right);
				});

				cane::report_if(
					not node.has_value(),
					ReportKind::Syntactical,
					"expected an infix or postfix operator"
				);
			}

			return node.value();
		}
	};

}  // namespace cane

#endif
