#ifndef CANE_PARSE_HPP
#define CANE_PARSE_HPP

#include <string_view>
#include <memory>

#include <cane/def.hpp>
#include <cane/log.hpp>
#include <cane/util.hpp>
#include <cane/lex.hpp>

namespace cane {

	///////////////
	// Utilities //
	///////////////

	template <typename T>
	inline std::shared_ptr<T> deepcopy(
		std::shared_ptr<T> root,

		std::shared_ptr<T> T::* lhs,
		std::shared_ptr<T> T::* rhs
	) {
		if (root == nullptr) {
			return nullptr;
		}

		auto ptr = std::make_shared<T>(*root);

		(*ptr).*lhs = deepcopy(root->lhs, lhs, rhs);
		(*ptr).*rhs = deepcopy(root->rhs, lhs, rhs);

		return ptr;
	}

	//////////
	// Type //
	//////////

	struct Type {
		TypeKind kind;

		std::shared_ptr<Type> lhs;
		std::shared_ptr<Type> rhs;

		// Type(const Type& n) = default;

		Type(TypeKind type_): kind(type_), lhs(nullptr), rhs(nullptr) {}

		Type(TypeKind type_, std::shared_ptr<Type> type):
				kind(type_), lhs(type), rhs(nullptr) {}

		Type(
			TypeKind type_,

			std::shared_ptr<Type> lhs_,
			std::shared_ptr<Type> rhs_
		):
				kind(type_), lhs(lhs_), rhs(rhs_) {}
	};

	/////////
	// AST //
	/////////

	struct Node {
		SymbolKind kind;
		SymbolKind op;

		std::string_view sv;

		std::shared_ptr<Type> type;

		std::shared_ptr<Node> lhs;
		std::shared_ptr<Node> rhs;

		// Node(const Node& n) = default;

		// No child nodes
		Node(
			SymbolKind kind_, std::string_view sv_, std::shared_ptr<Type> type_
		):
				kind(kind_), sv(sv_), type(type_), lhs(nullptr), rhs(nullptr) {}

		// Unary
		Node(
			SymbolKind kind_,
			std::string_view sv_,
			std::shared_ptr<Type> type_,
			std::shared_ptr<Node> node
		):
				kind(kind_), sv(sv_), type(type_), lhs(node), rhs(nullptr) {}

		// Binary
		Node(
			SymbolKind kind_,
			std::string_view sv_,
			std::shared_ptr<Type> type_,
			std::shared_ptr<Node> lhs_,
			std::shared_ptr<Node> rhs_
		):
				kind(kind_), sv(sv_), type(type_), lhs(lhs_), rhs(rhs_) {}

		Node(
			SymbolKind kind_,
			SymbolKind op_,
			std::string_view sv_,
			std::shared_ptr<Type> type_,
			std::shared_ptr<Node> lhs_,
			std::shared_ptr<Node> rhs_
		):
				kind(kind_),
				op(op_),
				sv(sv_),
				type(type_),
				lhs(lhs_),
				rhs(rhs_) {}
	};

	////////////
	// PARSER //
	////////////

	class Parser {
		private:
		Lexer lx;

		public:
		Parser(std::string_view sv): lx(sv) {}

		//////////////////////
		// PARSER FUNCTIONS //
		//////////////////////

		// Core parsing functions
		[[nodiscard]] std::optional<std::shared_ptr<Node>> parse() {
			// TODO: Handle unconsumed input.

			// CANE_FUNC();

			// if (lx.peek_is_kind(SymbolKind::EndFile)) {
			// 	return std::nullopt;
			// }

			return parse_expression();

			// if (not lx.peek_is_kind(SymbolKind::EndFile)) {
			// 	cane::report(
			// 		ReportKind::Syntactical,
			// 		"unexpected token `{}`",
			// 		lx.peek().sv
			// 	);
			// }

			// return { expr, type };
		}

		std::optional<TypeKind> parse_type_name() {
			// TODO: Parse nested types into a type tree
			// CANE_FUNC();

			if (lx.discard_if_kind(SymbolKind::TypeNumber)) {
				return TypeKind::Scalar;
			}

			else if (lx.discard_if_kind(SymbolKind::TypeString)) {
				return TypeKind::String;
			}

			else if (lx.discard_if_kind(SymbolKind::TypeRhythm)) {
				return TypeKind::Rhythm;
			}

			else if (lx.discard_if_kind(SymbolKind::TypeMelody)) {
				return TypeKind::Melody;
			}

			else if (lx.discard_if_kind(SymbolKind::TypeSequence)) {
				return TypeKind::Sequence;
			}

			else if (lx.discard_if_kind(SymbolKind::TypePattern)) {
				return TypeKind::Pattern;
			}

			return std::nullopt;
		}

		std::optional<std::shared_ptr<Type>> parse_type() {
			// CANE_FUNC();

			auto type_name = parse_type_name();

			if (not type_name.has_value()) {
				return std::nullopt;
			}

			auto type = std::make_shared<Type>(type_name.value());

			while (lx.peek_is_kind(SymbolKind::Arrow)) {
				lx.discard();
				type_name = parse_type_name();

				if (not type_name.has_value()) {
					return std::nullopt;
				}

				auto t = std::make_shared<Type>(
					TypeKind::Function,
					type,
					std::make_shared<Type>(type_name.value())
				);

				type = t;
			}

			return type;
		}

		[[nodiscard]] std::optional<std::shared_ptr<Node>> parse_primary() {
			// CANE_FUNC();

			Symbol symbol = lx.peek();
			auto bp = binding_power(symbol.kind);

			if (not bp.has_value()) {
				return std::nullopt;
			}

			lx.discard();

			switch (symbol.kind) {
				// Literals
				case SymbolKind::Identifier: {
					return std::make_shared<Node>(
						symbol.kind,
						symbol.sv,
						std::make_shared<Type>(TypeKind::None)
					);
				}

				case SymbolKind::String: {
					return std::make_shared<Node>(
						symbol.kind,
						symbol.sv,
						std::make_shared<Type>(TypeKind::String)
					);
				}

				case SymbolKind::Number: {
					return std::make_shared<Node>(
						symbol.kind,
						symbol.sv,
						std::make_shared<Type>(TypeKind::Scalar)
					);
				}

				// Literals (Implicit Concat)
				case SymbolKind::Beat:
				case SymbolKind::Rest: {
					auto root = std::make_shared<Node>(
						symbol.kind,
						symbol.sv,
						std::make_shared<Type>(TypeKind::Rhythm)
					);

					while (lx.peek_is_kind(SymbolKind::Beat) or
						   lx.peek_is_kind(SymbolKind::Rest)) {
						auto beat = lx.take();

						auto rhythm = std::make_shared<Node>(
							beat.kind,
							beat.sv,
							std::make_shared<Type>(TypeKind::Rhythm)
						);

						root = std::make_shared<Node>(
							SymbolKind::Concatenate,
							beat.sv,

							std::make_shared<Type>(TypeKind::Rhythm),

							/* lhs = */ root,
							/* rhs = */ rhythm
						);
					}

					return root;
				} break;

				case SymbolKind::LeftParen: {
					auto expr = parse_expression(/* binding power = */ 0);

					if (not lx.discard_if_kind(SymbolKind::RightParen)) {
						cane::report(ReportKind::Syntactical, "expected `)`");
					}

					return expr;
				} break;

				case SymbolKind::Function: {
					// Parameter
					auto parameter =
						lx.take_if_kind_opt(SymbolKind::Identifier);

					cane::report_if(
						not parameter.has_value(),
						ReportKind::Syntactical,
						"expected an identifier"
					);

					// TODO: Rename to `colon`
					if (not lx.discard_if_kind(SymbolKind::Euclidean)) {
						return std::nullopt;
					}

					// Parse type signature of function, then we pull out the
					// type of the param and body and construct the various
					// nodes
					auto fn_type = parse_type();

					cane::report_if(
						not fn_type.has_value() or fn_type.value() == nullptr,
						ReportKind::Syntactical,
						"expected a type annotation"
					);

					auto param_type = fn_type.value()->lhs;
					auto body_type = fn_type.value()->rhs;

					cane::report_if(
						param_type == nullptr or body_type == nullptr,
						ReportKind::Syntactical,
						"malformed function signature"
					);

					auto parameter_node = std::make_shared<Node>(
						SymbolKind::Identifier, parameter.value().sv, param_type
					);

					auto body = parse_expression(bp.value().right);

					cane::report_if(
						not body.has_value(),
						ReportKind::Syntactical,
						"expected a body"
					);

					return std::make_shared<Node>(
						SymbolKind::Function,
						symbol.sv,

						fn_type.value(),

						/* lhs = */ parameter_node,
						/* rhs = */ body.value()
					);
				} break;

				case SymbolKind::Let: {
					// Binding name
					auto identifier =
						lx.take_if_kind_opt(SymbolKind::Identifier);

					cane::report_if(
						not identifier.has_value(),
						ReportKind::Syntactical,
						"expected an identifier"
					);

					auto binding = std::make_shared<Node>(
						SymbolKind::Identifier,
						identifier.value().sv,
						std::make_shared<Type>(TypeKind::None)
					);

					// Reset binding power and parse body
					auto body = parse_expression(bp.value().right);

					cane::report_if(
						not body.has_value(),
						ReportKind::Syntactical,
						"expected a body"
					);

					return std::make_shared<Node>(
						SymbolKind::Let,
						symbol.sv,

						std::make_shared<Type>(TypeKind::None),

						/* lhs = */ binding,
						/* rhs = */ body.value()
					);
				} break;

				default: break;
			}

			return std::nullopt;
		}

		[[nodiscard]] std::optional<std::shared_ptr<Node>> parse_prefix() {
			// CANE_FUNC();

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

			if (not is_opfix(OpfixKind::Prefix, symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();
			auto expr = parse_expression(bp.value().right);

			cane::report_if(
				not expr.has_value(),
				ReportKind::Syntactical,
				"expected an expression"
			);

			return std::make_shared<Node>(
				symbol.kind,
				symbol.sv,
				std::make_shared<Type>(TypeKind::None),
				expr.value()
			);
		}

		[[nodiscard]] std::optional<std::shared_ptr<Node>>
		parse_infix(std::shared_ptr<Node> lhs) {
			// CANE_FUNC();

			Symbol symbol = lx.peek();
			auto bp = binding_power(symbol.kind);

			if (not bp.has_value()) {
				return std::nullopt;
			}

			if (not is_opfix(OpfixKind::Infix, symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();
			auto rhs = parse_expression(bp.value().right);

			cane::report_if(
				not rhs.has_value(),
				ReportKind::Syntactical,
				"expected a right-hand-side expression"
			);

			return std::make_shared<Node>(
				symbol.kind,
				symbol.sv,
				std::make_shared<Type>(TypeKind::None),
				lhs,
				rhs.value()
			);
		}

		[[nodiscard]] std::optional<std::shared_ptr<Node>>
		parse_postfix(std::shared_ptr<Node> lhs) {
			// CANE_FUNC();

			Symbol symbol = lx.peek();

			if (not is_opfix(OpfixKind::Postfix, symbol.kind)) {
				return std::nullopt;
			}

			lx.discard();

			return std::make_shared<Node>(
				symbol.kind,
				symbol.sv,
				std::make_shared<Type>(TypeKind::None),
				lhs
			);
		}

		[[nodiscard]] std::optional<std::shared_ptr<Node>>
		parse_expression(size_t min_bp = 0) {
			// CANE_FUNC();

			auto node = parse_primary().or_else([&] { return parse_prefix(); });

			cane::report_if(
				not node.has_value(),
				ReportKind::Syntactical,
				"expected a primary expression or a prefix operator"

			);

			// A function call is just juxtaposed expressions.
			constexpr auto is_call = [](SymbolKind kind) {
				return is_opfix(OpfixKind::Literal, kind) or
					is_opfix(OpfixKind::Primary, kind) or
					kind == SymbolKind::Call;
			};

			while (true) {
				auto [kind, sv] = lx.peek();

				if (not(is_opfix(OpfixKind::Infix, kind) or
						is_opfix(OpfixKind::Postfix, kind) or is_call(kind))) {
					break;
				}

				// Remap to a call operator if the next symbol is a
				// primary expression
				kind = is_call(kind) ? SymbolKind::Call : kind;

				auto bp = binding_power(kind);

				if (not bp.has_value() or bp.value().left < min_bp) {
					break;
				}

				// Parse a function call.
				if (is_call(kind)) {
					auto rhs = parse_expression(bp.value().right);

					cane::report_if(
						not rhs.has_value(),
						ReportKind::Syntactical,
						"expected a right-hand-side expression"
					);

					node = std::make_shared<Node>(
						SymbolKind::Call,
						sv,
						std::make_shared<Type>(TypeKind::None),
						node.value(),
						rhs.value()
					);

					continue;
				}

				// Parse everything else.
				node = parse_postfix(node.value()).or_else([&] {
					return parse_infix(node.value());
				});

				cane::report_if(
					not node.has_value(),
					ReportKind::Syntactical,
					"expected an infix or postfix operator"
				);
			}

			return node;
		}
	};

}  // namespace cane

#endif
