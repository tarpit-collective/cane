#ifndef CANE_PARSE_HPP
#define CANE_PARSE_HPP

#include <string_view>
#include <memory>

#include <cane/def.hpp>
#include <cane/log.hpp>
#include <cane/util.hpp>
#include <cane/lex.hpp>
#include <cane/value.hpp>

namespace cane {

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
		[[nodiscard]] std::optional<cane::Instructions> parse() {
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

		[[nodiscard]] std::optional<cane::Instructions> parse_primary() {
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

		[[nodiscard]] std::optional<cane::Instructions> parse_prefix() {
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

		[[nodiscard]] std::optional<cane::Instructions>
		parse_infix(cane::Instructions lhs) {
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

		[[nodiscard]] std::optional<cane::Instructions>
		parse_postfix(cane::Instructions lhs) {
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

		[[nodiscard]] std::optional<cane::Instructions>
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
