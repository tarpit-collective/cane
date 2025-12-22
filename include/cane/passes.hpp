#ifndef CANE_PASSES_HPP
#define CANE_PASSES_HPP

#include <memory>
#include <print>

#include <unordered_set>
#include <unordered_map>
#include <map>
#include <vector>

#include <cane/parse.hpp>
#include <cane/value.hpp>
#include <cane/ops.hpp>

////////////
// Passes //
////////////

namespace cane {

	struct BindingEntry {
		std::string_view sv;
		BindingKind kind;
		size_t depth;
		BoxNode node;
	};

	using BindingEnvironment = std::vector<BindingEntry>;

	inline void print_binding_environment(const BindingEnvironment& bindings) {
		// Print the bindings in the current scope.
		CANE_OKAY("bindings:");
		for (auto [sv, kind, depth, node]: bindings) {
			std::string indent(depth * 2, ' ');
			std::println(
				"  [{}]{} ► " CANE_COLOUR_YELLOW "`{}`" CANE_RESET " = {}",
				depth,
				indent,
				sv,
				kind
			);
		}
	}

	inline Sequence pass_evaluator(Configuration cfg, BoxNode node);

	inline BoxNode pass_print(Configuration cfg, BoxNode node);
	inline BoxNode pass_trace(Configuration cfg, BoxNode node);
	inline BoxNode pass_validate(Configuration cfg, BoxNode node);

	//////////////////
	// Compile/Eval //
	//////////////////

	using Pass = BoxNode (*)(Configuration, BoxNode);

	// Run a collection of passes in series.
	template <typename... Ts>
	[[nodiscard]] decltype(auto)
	pipeline(Configuration cfg, BoxNode root, Ts&&... passes) {
		return (passes(cfg, root), ...);
	}

	template <typename... Ts>
	[[nodiscard]] decltype(auto)
	debug_pipeline(Configuration cfg, BoxNode root, Ts&&... passes) {
		(
			[&](Pass pass) {
				root = pass_print(cfg, pass_validate(cfg, pass(cfg, root)));
			}(passes),
			...
		);

		return root;
	}

	// Run a collection of passes in series before finally evaluating the
	// tree.
	template <typename... Ts>
	[[nodiscard]] decltype(auto)
	compile(Configuration cfg, BoxNode root, Ts&&... passes) {
		root = pipeline(cfg, root, std::forward<Ts>(passes)...);
		return pass_evaluator(cfg, root);
	}

	template <typename... Ts>
	[[nodiscard]] decltype(auto)
	debug_compile(Configuration cfg, BoxNode root, Ts&&... passes) {
		root = debug_pipeline(cfg, root, std::forward<Ts>(passes)...);
		return pass_evaluator(cfg, root);
	}

	// Parse a string and run a collection of passes on it before evaluating
	// it.
	template <typename... Ts>
	[[nodiscard]] std::optional<Sequence> parse_and_compile(
		std::string_view source, Configuration cfg, Ts&&... passes
	) {
		cane::Parser parser { source };
		auto root = parser.parse();

		return root.transform([&](auto root) {
			return pass_evaluator(
				cfg, pipeline(cfg, root, std::forward<Ts>(passes)...)
			);
		});
	}

	template <typename... Ts>
	[[nodiscard]] std::optional<Sequence> debug_parse_and_compile(
		std::string_view source, Configuration cfg, Ts&&... passes
	) {
		cane::Parser parser { source };
		auto root = parser.parse();

		return root.transform([&](auto root) {
			return pass_evaluator(
				cfg,
				debug_pipeline(
					cfg, pass_print(cfg, root), std::forward<Ts>(passes)...
				)
			);
		});
	}

	//////////////////////
	// AST Printer Pass //
	//////////////////////

	[[nodiscard]] inline BoxNode pass_print_walk(
		Configuration cfg,
		BoxNode node,
		std::vector<bool> bits = {},
		size_t depth = 0
	);

	namespace detail {
		inline std::string_view SIGIL_NULL = CANE_CSTR("⊗ (null)");
		inline std::string_view SIGIL_BRANCH = CANE_CSTR("○");
		inline std::string_view SIGIL_LEAF = CANE_CSTR("►");

		inline std::string_view SIGIL_LHS = CANE_CSTR("├");
		inline std::string_view SIGIL_RHS = CANE_CSTR("└");

		inline void pass_print_indent_node(
			std::string_view bar, std::vector<bool> bits = {}
		) {
			for (bool b: bits) {
				std::print(" {}", b ? "│" : " ");
			}

			std::print(" {}", bar);
		}

		inline void pass_print_indent_node_child_lhs(
			Configuration cfg,
			BoxNode node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_LHS, bits);

			bits.push_back(true);
			CANE_UNUSED(pass_print_walk(cfg, node->lhs, bits, depth));
			bits.pop_back();
		}

		inline void pass_print_indent_node_child_rhs(
			Configuration cfg,
			BoxNode node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_RHS, bits);

			bits.push_back(false);
			CANE_UNUSED(pass_print_walk(cfg, node->rhs, bits, depth));
			bits.pop_back();
		}
	}  // namespace detail

	[[nodiscard]] inline BoxNode pass_print(Configuration cfg, BoxNode node) {
		return pass_print_walk(cfg, node);
	}

	[[nodiscard]] inline BoxNode pass_print_walk(
		Configuration cfg, BoxNode node, std::vector<bool> bits, size_t depth
	) {
		if (node == nullptr) {
			std::cout << detail::SIGIL_NULL << '\n';
			return node;
		}

		bool is_root = depth == 0;
		bool is_leaf = is_opfix(OpfixKind::Literal, node->kind);

		auto joiner = is_root ? " " : "─";
		auto marker = is_leaf ? detail::SIGIL_LEAF : detail::SIGIL_BRANCH;

		std::cout << joiner << marker;

		auto symbol_sv = symbol_kind_to_str(node->kind);
		auto op_sv = symbol_kind_to_str(node->op);
		auto node_sv = node->sv;
		auto type_sv = type_kind_to_str(node->type);

		std::print(" " CANE_COLOUR_YELLOW "{}:" CANE_RESET, symbol_sv);
		std::print(" " CANE_COLOUR_GREEN "{}" CANE_RESET, op_sv);
		std::print(" " CANE_COLOUR_RED "`{}`" CANE_RESET, type_sv);

		if (is_leaf) {
			std::print(" " CANE_COLOUR_BLUE "\"{}\"" CANE_RESET, node_sv);
		}

		std::cout << '\n';

		if (not is_leaf) {
			detail::pass_print_indent_node_child_lhs(
				cfg, node, bits, depth + 1
			);

			detail::pass_print_indent_node_child_rhs(
				cfg, node, bits, depth + 1
			);
		}

		return node;
	}

	///////////
	// Trace //
	///////////

	// Print the tree as close to the original source as possible for debugging
	// more complex transformations.

	// TODO: Can we just also use this as a code formatter?

	[[nodiscard]] inline BoxNode
	pass_trace_walk(Configuration cfg, BoxNode node, size_t depth);

	[[nodiscard]] inline BoxNode pass_trace(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		auto root = pass_trace_walk(cfg, node, 0);

		std::println();
		std::cout.flush();

		return root;
	}

	[[nodiscard]] inline BoxNode
	pass_trace_walk(Configuration cfg, BoxNode node, size_t depth) {
		std::string indent(depth * 2, ' ');

		if (node == nullptr) {
			return nullptr;
		}

		switch (node->kind) {
			case SymbolKind::String: {
				std::print("\"{}\"", node->sv);
			} break;

			case SymbolKind::Let: {
				std::print("{}let ", indent);
				CANE_UNUSED(pass_trace_walk(cfg, node->lhs, depth + 1));

				std::print(" ");
				CANE_UNUSED(pass_trace_walk(cfg, node->rhs, depth + 1));
			} break;

			case SymbolKind::Function: {
				auto fn = node;

				// Descend while we have nested functions so that we can print
				// the parameters all on the same line.
				while (fn->kind == SymbolKind::Function) {
					std::print("\\");
					CANE_UNUSED(pass_trace_walk(cfg, fn->lhs, depth + 1));

					std::print(" ");
					fn = fn->rhs;
				}

				std::println();
				std::print("{}", indent);

				CANE_UNUSED(pass_trace_walk(cfg, fn, depth + 1));
			} break;

			case SymbolKind::Call: {
				std::print("(");
				CANE_UNUSED(pass_trace_walk(cfg, node->lhs, depth + 1));
				std::print(" ");
				CANE_UNUSED(pass_trace_walk(cfg, node->rhs, depth + 1));
				std::print(")");
			} break;

			case SymbolKind::Block: {
				CANE_UNUSED(pass_trace_walk(cfg, node->lhs, depth));
				std::print(";\n\n");
				CANE_UNUSED(pass_trace_walk(cfg, node->rhs, depth));
			} break;

			default: {
				if (is_opfix(OpfixKind::Literal, node->kind)) {
					std::print("{}", node->sv);
				}

				else if (is_opfix(OpfixKind::Prefix, node->kind)) {
					std::print("{}", node->kind);
					CANE_UNUSED(pass_trace_walk(cfg, node->lhs, depth + 1));
				}

				else if (is_opfix(OpfixKind::Infix, node->kind)) {
					CANE_UNUSED(pass_trace_walk(cfg, node->lhs, depth + 1));
					std::print(" {} ", node->kind);
					CANE_UNUSED(pass_trace_walk(cfg, node->rhs, depth + 1));
				}

				else {
					cane::report(
						ReportKind::Internal, "unhandled node `{}`", node->kind
					);
				}
			} break;
		}

		return node;
	}

	//////////////
	// Validate //
	//////////////

	// Ensure that the tree is correctly formed.
	// This includes things like ensuring that the `lhs` of assign/function is
	// an identifier node.

	[[nodiscard]] inline BoxNode
	pass_validate_walk(Configuration cfg, BoxNode node);

	[[nodiscard]] inline BoxNode
	pass_validate(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		return pass_validate_walk(cfg, node);
	}

	[[nodiscard]] inline BoxNode
	pass_validate_walk(Configuration cfg, BoxNode node) {
		if (node == nullptr) {
			return nullptr;
		}

		if (is_opfix(OpfixKind::Operator, node->kind)) {
			cane::report(ReportKind::Internal, "unexpected operator node");
		}

		switch (node->kind) {
			case SymbolKind::Number:
			case SymbolKind::String:

			case SymbolKind::Beat:
			case SymbolKind::Rest: {
				auto lhs = pass_validate_walk(cfg, node->lhs);
				auto rhs = pass_validate_walk(cfg, node->rhs);

				cane::report_if(
					lhs != nullptr or rhs != nullptr,
					ReportKind::Internal,
					"literals should not have children nodes"
				);
			} break;

			case SymbolKind::Let: {
				auto lhs = pass_validate_walk(cfg, node->lhs);
				auto rhs = pass_validate_walk(cfg, node->rhs);

				cane::report_if(
					lhs->kind != SymbolKind::Identifier,
					ReportKind::Internal,
					"`lhs` should be an identifier"
				);
			} break;

			case SymbolKind::Function: {
				auto lhs = pass_validate_walk(cfg, node->lhs);
				auto rhs = pass_validate_walk(cfg, node->rhs);

				cane::report_if(
					lhs->kind != SymbolKind::Identifier,
					ReportKind::Internal,
					"`lhs` should be an identifier"
				);
			} break;

			default: {
				auto lhs = pass_validate_walk(cfg, node->lhs);
				auto rhs = pass_validate_walk(cfg, node->rhs);

				if (is_opfix(OpfixKind::Prefix, node->kind) or
					is_opfix(OpfixKind::Postfix, node->kind)) {
					cane::report_if(
						lhs == nullptr,
						ReportKind::Internal,
						"unary node should have a `lhs` child"
					);

					cane::report_if(
						rhs != nullptr,
						ReportKind::Internal,
						"unary node should not have a `rhs` child"
					);
				}

				else if (is_opfix(OpfixKind::Infix, node->kind)) {
					cane::report_if(
						lhs == nullptr or rhs == nullptr,
						ReportKind::Internal,
						"binary node should have both a `lhs` and `rhs` child"
					);
				}
			} break;
		}

		return node;
	}

	//////////////////////
	// Resolve Bindings //
	//////////////////////

	// Resolve all bindings (not parameters/arguments) and return a single
	// expression (since the only side effect is assignment, once resolved, all
	// that actually matters is the final expression).

	// We track stack depth so that we can assign a unique ID to each
	// identifier. This is to workaround things like shadowing where we have
	// multiple of the same identifier.

	[[nodiscard]] inline std::tuple<BoxNode, BindingEnvironment>
	pass_binding_resolution_walk(
		Configuration cfg,
		BoxNode node,
		BindingEnvironment bindings,
		size_t depth
	);

	[[nodiscard]] inline BoxNode
	pass_binding_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();

		BindingEnvironment bindings;

		auto [root, root_env] =
			pass_binding_resolution_walk(cfg, node, bindings, 0);

		return root;
	}

	[[nodiscard]] inline std::tuple<BoxNode, BindingEnvironment>
	pass_binding_resolution_walk(
		Configuration cfg,
		BoxNode node,
		BindingEnvironment bindings,
		size_t depth
	) {
		print_binding_environment(bindings);

		if (node == nullptr) {
			return { nullptr, bindings };
		}

		// TODO: Just copy the whole tree in the entry function for this pass
		// and then we can update it in-place.
		auto new_node = deepcopy(node, &Node::lhs, &Node::rhs);

		switch (node->kind) {
			case SymbolKind::Number:
			case SymbolKind::String:

			case SymbolKind::Beat:
			case SymbolKind::Rest: {
				return { new_node, bindings };
			} break;

			case SymbolKind::Function: {
				auto param = new_node->lhs;

				bindings.emplace_back(
					param->sv, BindingKind::Parameter, depth, nullptr
				);

				// We must visit the function body here to resolve any
				// references to bindings.
				auto [fn, fn_env] = pass_binding_resolution_walk(
					cfg, new_node->rhs, bindings, depth + 1
				);

				new_node->rhs = fn;
				bindings.pop_back();

				return { new_node, bindings };
			} break;

			case SymbolKind::Let: {
				auto binding = new_node->lhs;

				auto [assign, assign_env] = pass_binding_resolution_walk(
					cfg, new_node->rhs, bindings, depth + 1
				);

				new_node->rhs = deepcopy(assign, &Node::lhs, &Node::rhs);

				bindings.emplace_back(
					binding->sv,
					BindingKind::Binding,
					depth,
					deepcopy(assign, &Node::lhs, &Node::rhs)
				);

				// Return the inner node so that `let` becomes it's own value.
				return { assign, bindings };
			} break;

			case SymbolKind::Call: {
				// Resolve any references in the argument before passing it to
				// the function.
				auto [arg, arg_env] = pass_binding_resolution_walk(
					cfg, new_node->rhs, bindings, depth + 1
				);

				// Now resolve references in the function on the left.
				auto [call, call_env] = pass_binding_resolution_walk(
					cfg, new_node->lhs, bindings, depth + 1
				);

				new_node->rhs = arg;
				new_node->lhs = call;

				return { new_node, bindings };
			} break;

			case SymbolKind::Identifier: {
				// To find a valid binding, we do a search backwards through the
				// scope. We then check if we've found _any_ bindings. If not,
				// we know that no bindings _or_ parameters with that name exist
				// so we error.

				// If we _do_ find something, in the case of a
				// parameter, we just keep the graph as it is but in the case of
				// a binding, we substitute in the subtree in place of the
				// identifier.

				auto ref = std::ranges::find(
					bindings | std::views::reverse,
					new_node->sv,
					&BindingEntry::sv
				);

				if (ref == bindings.rend()) {
					cane::report(
						ReportKind::Semantic,
						"unknown binding `{}`",
						new_node->sv
					);

					return { new_node, bindings };
				}

				auto [sv, kind, depth, node] = *ref;

				if (kind == BindingKind::Binding) {
					auto [ident, ident_env] = pass_binding_resolution_walk(
						cfg,
						deepcopy(node, &Node::lhs, &Node::rhs),
						bindings,
						depth + 1
					);

					new_node = ident;

					return { ident, bindings };
				}

				// In the case of a parameter, just return the node unchanged.
				return { new_node, bindings };
			} break;

			case SymbolKind::Block: {
				// Blocks discard the `lhs` value but they do still maintain
				// their binding environment. Below, we pass the `lhs`
				// environment into the `rhs`.

				auto [lhs, lhs_env] = pass_binding_resolution_walk(
					cfg, new_node->lhs, bindings, depth + 1
				);

				auto [rhs, rhs_env] = pass_binding_resolution_walk(
					cfg, new_node->rhs, lhs_env, depth + 1
				);

				new_node->lhs = lhs;
				new_node->rhs = rhs;

				return { rhs, bindings };
			} break;

			default: {
				auto [lhs, lhs_env] = pass_binding_resolution_walk(
					cfg, new_node->lhs, bindings, depth + 1
				);

				auto [rhs, rhs_env] = pass_binding_resolution_walk(
					cfg, new_node->rhs, lhs_env, depth + 1
				);

				new_node->lhs = lhs;
				new_node->rhs = rhs;

				return { new_node, bindings };
			} break;
		}

		return { new_node, bindings };
	}

	//////////////////
	// Type Checker //
	//////////////////

	[[nodiscard]] inline std::tuple<TypeKind, BoxNode, BindingEnvironment>
	pass_type_resolution_walk(
		Configuration cfg,
		BoxNode node,
		BindingEnvironment bindings,
		OptionalBoxNode arg,
		bool is_called,
		size_t depth
	);

	[[nodiscard]] inline BoxNode
	pass_type_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();

		BindingEnvironment bindings;

		auto [root, root_node, root_env] = pass_type_resolution_walk(
			cfg, node, bindings, std::nullopt, true, 0
		);

		// CANE_UNUSED(pass_print(cfg, root_node));

		// cane::report_if(
		// 	type != TypeKind::Pattern,
		// 	ReportKind::Type,
		// 	"expected a pattern type"
		// );
		//

		CANE_OKAY("type = {}", root);
		return root_node;
	}

	[[nodiscard]] inline std::tuple<bool, TypeKind, SymbolKind>
	type_remap_trivial(
		[[maybe_unused]] Configuration cfg,
		TypeKind lhs,
		TypeKind rhs,
		BoxNode node
	) {
		if (node == nullptr) {
			return { false, TypeKind::None, SymbolKind::None };
		}

		// clang-format off

		// If the types don't match, it just means this overload of
		// the operator isn't the correct one but we might have one
		// handled later.
	#define CANE_TYPE_REMAP(symbol, expected_lhs, expected_rhs, new_type, new_op) \
		if (node->kind == SymbolKind::symbol and \
			(lhs == TypeKind::expected_lhs and rhs == TypeKind::expected_rhs)) { \
			return { true, TypeKind::new_type, SymbolKind::new_op }; \
		}

 		/* Prefix/Unary */
 		CANE_TYPE_REMAP(Abs, Scalar, None, Scalar, AbsScalar);
 		CANE_TYPE_REMAP(Neg, Scalar, None, Scalar, NegScalar);

 		CANE_TYPE_REMAP(Invert, Rhythm, None, Rhythm, InvertRhythm);
 		CANE_TYPE_REMAP(Reverse, Rhythm, None, Rhythm, ReverseRhythm);

 		CANE_TYPE_REMAP(Reverse, Melody, None, Melody, ReverseMelody);

 		CANE_TYPE_REMAP(Incr, Scalar, None, Scalar, IncrScalar);
 		CANE_TYPE_REMAP(Decr, Scalar, None, Scalar, DecrScalar);

 		CANE_TYPE_REMAP(Coerce, Scalar, None, Melody, CoerceScalar);
 		CANE_TYPE_REMAP(Coerce, Melody, None, Melody, CoerceMelody);

 		/* Scalar */
 		CANE_TYPE_REMAP(Add, Scalar, Scalar, Scalar, AddScalarScalar);
 		CANE_TYPE_REMAP(Sub, Scalar, Scalar, Scalar, SubScalarScalar);
 		CANE_TYPE_REMAP(Mul, Scalar, Scalar, Scalar, MulScalarScalar);
 		CANE_TYPE_REMAP(Div, Scalar, Scalar, Scalar, DivScalarScalar);

 		CANE_TYPE_REMAP(LeftShift, Scalar, Scalar, Scalar, LeftShiftScalarScalar);
 		CANE_TYPE_REMAP(RightShift, Scalar, Scalar, Scalar, RightShiftScalarScalar);

 		CANE_TYPE_REMAP(LCM, Scalar, Scalar, Scalar, LCMScalarScalar);
 		CANE_TYPE_REMAP(GCD, Scalar, Scalar, Scalar, GCDScalarScalar);

 		CANE_TYPE_REMAP(Euclidean, Scalar, Scalar, Rhythm, EuclideanScalarScalar);
 		CANE_TYPE_REMAP(Concatenate, Scalar, Scalar, Melody, ConcatenateScalarScalar);
		CANE_TYPE_REMAP(Random, Scalar, Scalar, Scalar, RandomScalarScalar);

 		/* Melody */
 		CANE_TYPE_REMAP(Map, Melody, Rhythm, Sequence, MapMelodyRhythm);
 		CANE_TYPE_REMAP(Map, Scalar, Rhythm, Sequence, MapScalarRhythm);

 		CANE_TYPE_REMAP(LeftShift, Melody, Scalar, Melody, LeftShiftMelodyScalar);
 		CANE_TYPE_REMAP(RightShift, Melody, Scalar, Melody, RightShiftMelodyScalar);

 		CANE_TYPE_REMAP(Add, Melody, Scalar, Melody, AddMelodyScalar);
 		CANE_TYPE_REMAP(Sub, Melody, Scalar, Melody, SubMelodyScalar);
 		CANE_TYPE_REMAP(Mul, Melody, Scalar, Melody, MulMelodyScalar);
 		CANE_TYPE_REMAP(Div, Melody, Scalar, Melody, DivMelodyScalar);

 		CANE_TYPE_REMAP(Add, Melody, Melody, Melody, AddMelodyMelody);
 		CANE_TYPE_REMAP(Sub, Melody, Melody, Melody, SubMelodyMelody);
 		CANE_TYPE_REMAP(Mul, Melody, Melody, Melody, MulMelodyMelody);
 		CANE_TYPE_REMAP(Div, Melody, Melody, Melody, DivMelodyMelody);

 		CANE_TYPE_REMAP(Repeat, Melody, Scalar, Melody, RepeatMelodyScalar);

 		CANE_TYPE_REMAP(Concatenate, Melody, Melody, Melody, ConcatenateMelodyMelody);
 		CANE_TYPE_REMAP(Concatenate, Melody, Scalar, Melody, ConcatenateMelodyScalar);
 		CANE_TYPE_REMAP(Concatenate, Scalar, Melody, Melody, ConcatenateScalarMelody);

 		CANE_TYPE_REMAP(Head, Melody, None, Melody, HeadMelody);
 		CANE_TYPE_REMAP(Tail, Melody, None, Melody, TailMelody);

 		/* Rhythm */
 		CANE_TYPE_REMAP(Map, Rhythm, Melody, Sequence, MapRhythmMelody);
 		CANE_TYPE_REMAP(Map, Rhythm, Scalar, Sequence, MapRhythmScalar);

 		CANE_TYPE_REMAP(LeftShift, Rhythm, Scalar, Rhythm, LeftShiftRhythmScalar);
 		CANE_TYPE_REMAP(RightShift, Rhythm, Scalar, Rhythm, RightShiftRhythmScalar);

 		CANE_TYPE_REMAP(Repeat, Rhythm, Scalar, Rhythm, RepeatRhythmScalar);
 		CANE_TYPE_REMAP(Concatenate, Rhythm, Rhythm, Rhythm, ConcatenateRhythmRhythm);

 		CANE_TYPE_REMAP(Or, Rhythm, Rhythm, Rhythm, OrRhythmRhythm);
 		CANE_TYPE_REMAP(Xor, Rhythm, Rhythm, Rhythm, XorRhythmRhythm);
 		CANE_TYPE_REMAP(And, Rhythm, Rhythm, Rhythm, AndRhythmRhythm);

 		CANE_TYPE_REMAP(Head, Rhythm, None, Rhythm, HeadRhythm);
 		CANE_TYPE_REMAP(Tail, Rhythm, None, Rhythm, TailRhythm);

 		/* Sequence */
 		CANE_TYPE_REMAP(Concatenate, Sequence, Sequence, Sequence, ConcatenateSequenceSequence);
 		CANE_TYPE_REMAP(Layer, Sequence, Sequence, Sequence, LayerSequenceSequence);

 		CANE_TYPE_REMAP(Head, Sequence, None, Sequence, HeadSequence);
 		CANE_TYPE_REMAP(Tail, Sequence, None, Sequence, TailSequence);

 		CANE_TYPE_REMAP(Mul, Sequence, Scalar, Sequence, MulSequenceScalar);
 		CANE_TYPE_REMAP(Div, Sequence, Scalar, Sequence, DivSequenceScalar);

 		CANE_TYPE_REMAP(Send, Sequence, String, Pattern, SendSequenceString);

 		/* Pattern */
 		CANE_TYPE_REMAP(Head, Pattern, None, Pattern, HeadPattern);
 		CANE_TYPE_REMAP(Tail, Pattern, None, Pattern, TailPattern);

 		CANE_TYPE_REMAP(Layer, Pattern, Pattern, Pattern, LayerPatternPattern);
 		CANE_TYPE_REMAP(Layer, Pattern, Sequence, Pattern, LayerPatternSequence);
 		CANE_TYPE_REMAP(Layer, Sequence, Pattern, Pattern, LayerSequencePattern);

 		CANE_TYPE_REMAP(Concatenate, Pattern, Pattern, Pattern, ConcatenatePatternPattern);
 		CANE_TYPE_REMAP(Concatenate, Pattern, Sequence, Pattern, ConcatenatePatternSequence);
 		CANE_TYPE_REMAP(Concatenate, Sequence, Pattern, Pattern, ConcatenateSequencePattern);

 		CANE_TYPE_REMAP(Send, Pattern, String, Pattern, SendPatternString);
 	;
		// clang-format on

		return { false, TypeKind::None, SymbolKind::None };

#undef CANE_TYPE_REMAP
	}

	[[nodiscard]] inline std::tuple<TypeKind, BoxNode, BindingEnvironment>
	pass_type_resolution_walk(
		Configuration cfg,
		BoxNode node,
		BindingEnvironment bindings,
		OptionalBoxNode arg,
		bool is_called,
		size_t depth
	) {
		print_binding_environment(bindings);
		CANE_UNUSED(pass_print(cfg, node));

		if (node == nullptr) {
			return { TypeKind::None, nullptr, bindings };
		}

		// TODO: Just copy the whole tree in the entry function for this pass
		// and then we can update it in-place.
		auto new_node = deepcopy(node, &Node::lhs, &Node::rhs);

		switch (node->kind) {
			// Literals
			case SymbolKind::Number: {
				new_node->type = TypeKind::Scalar;
				return { TypeKind::Scalar, new_node, bindings };
			} break;

			case SymbolKind::String: {
				new_node->type = TypeKind::String;
				return { TypeKind::String, new_node, bindings };
			} break;

			case SymbolKind::Beat:
			case SymbolKind::Rest: {
				new_node->type = TypeKind::Rhythm;
				return { TypeKind::Rhythm, new_node, bindings };
			} break;

			case SymbolKind::Function: {
				// If we return a function from another function that uses
				// bindings from the parent scope, we need to replace them
				// in the body of this function.

				// NOTE: How do we replace any references to bindings in the
				// body without evaluating it?

				// Uncalled function.
				auto param = new_node->lhs;

				bindings.emplace_back(
					param->sv,
					BindingKind::Parameter,
					depth,
					arg.value_or(nullptr)
				);

				CANE_OKAY(
					"new parameter = {}, is_called = {}",
					param->sv,
					arg.has_value()
				);

				// Bind argument in function environment.
				// Do not visit the argument here, we only visit it once it's
				// _used_.

				// Important that we shadow any other bindings of the same name.

				auto [fn, fn_node, fn_env] = pass_type_resolution_walk(
					cfg,
					new_node->rhs,
					bindings,
					std::nullopt,
					arg.has_value(),
					depth + 1
				);

				new_node->type = fn;
				new_node->rhs = fn_node;

				if (arg.has_value() and is_called) {
					return { fn, fn_node, bindings };
				}

				new_node->type = TypeKind::Function;
				return { TypeKind::Function, new_node, bindings };
			} break;

			case SymbolKind::Let: {
				auto binding = new_node->lhs;

				auto [assign, assign_node, assign_env] =
					pass_type_resolution_walk(
						cfg, new_node->rhs, bindings, arg, is_called, depth + 1
					);

				new_node->type = assign;
				new_node->rhs = deepcopy(assign_node, &Node::lhs, &Node::rhs);

				bindings.emplace_back(
					binding->sv,
					BindingKind::Binding,
					depth,
					deepcopy(assign_node, &Node::lhs, &Node::rhs)
				);

				CANE_OKAY("new binding = {}", binding->sv);

				if (is_called) {
					return { assign, assign_node, bindings };
				}

				return { assign, new_node, bindings };
			} break;

			case SymbolKind::Call: {
				auto [argument, argument_node, argument_env] =
					pass_type_resolution_walk(
						cfg,
						new_node->rhs,
						bindings,
						std::nullopt,
						is_called,
						depth + 1
					);

				// cane::report_if(
				// 	fn != TypeKind::Function,
				// 	ReportKind::Semantic,
				// 	"cannot call non-function type `{}`",
				// 	fn
				// );

				auto [call, call_node, call_env] = pass_type_resolution_walk(
					cfg,
					new_node->lhs,
					bindings,
					argument_node,
					is_called,
					depth + 1
				);

				new_node->type = call;

				new_node->rhs = argument_node;
				new_node->lhs = call_node;

				// if (is_called) {
				// 	return { call, call_node, bindings };
				// }

				return { call, new_node, bindings };
			} break;

			case SymbolKind::Identifier: {
				// To find a valid binding, we do a search backwards through the
				// scope. We then check if we've found _any_ bindings. If not,
				// we know that no bindings _or_ parameters with that name exist
				// so we error.

				// If we _do_ find something, in the case of a
				// parameter, we just keep the graph as it is but in the case of
				// a binding, we substitute in the subtree in place of the
				// identifier.

				auto ref = std::ranges::find(
					bindings | std::views::reverse, node->sv, &BindingEntry::sv
				);

				if (ref == bindings.rend()) {
					cane::report(
						ReportKind::Semantic,
						"unknown binding `{}`",
						new_node->sv
					);

					return { TypeKind::None, new_node, bindings };
				}

				auto [sv, kind, depth, node] = *ref;

				if (kind == BindingKind::Binding or
					(is_called and kind == BindingKind::Parameter)) {
					auto [ident, ident_node, ident_env] =
						pass_type_resolution_walk(
							cfg,
							deepcopy(node, &Node::lhs, &Node::rhs),
							bindings,
							arg,
							is_called,
							depth + 1
						);

					new_node = ident_node;

					return { ident, ident_node, bindings };
				}

				return { TypeKind::None, new_node, bindings };
			} break;

			case SymbolKind::Block: {
				// We need to visit both sides of the node in case of side
				// effects from assignment.

				// We just return the right hand side type.

				auto [lhs, lhs_node, lhs_env] = pass_type_resolution_walk(
					cfg, new_node->lhs, bindings, arg, is_called, depth + 1
				);

				auto [rhs, rhs_node, rhs_env] = pass_type_resolution_walk(
					cfg, new_node->rhs, lhs_env, arg, is_called, depth + 1
				);

				new_node->type = rhs;

				new_node->lhs = lhs_node;
				new_node->rhs = rhs_node;

				return { rhs, new_node, bindings };
			} break;

			// Attempt trivial cases.
			default: {
				// Visit both children, even in the case of unary nodes.
				// We will exit early if any child is a nullptr anyway.

				auto [lhs, lhs_node, lhs_env] = pass_type_resolution_walk(
					cfg, new_node->lhs, bindings, arg, is_called, depth + 1
				);

				auto [rhs, rhs_node, rhs_env] = pass_type_resolution_walk(
					cfg, new_node->rhs, lhs_env, arg, is_called, depth + 1
				);

				// Search for a type mapping.
				auto [succ, type, op] =
					type_remap_trivial(cfg, lhs, rhs, new_node);

				CANE_INFO(
					succ ? "found a valid type mapping: `{}`({}) {} `{}`({})" :
						   "did not find a valid type mapping: `{}`({}) {} "
						   "`{}`({})",
					lhs,
					new_node->lhs == nullptr ? "nullptr" : new_node->lhs->sv,
					new_node->kind,
					rhs,
					new_node->rhs == nullptr ? "nullptr" : new_node->rhs->sv
				);

				// cane::report_if(
				// 	not succ,
				// 	ReportKind::Type,
				// 	"no mapping for `{}` {} `{}`",
				// 	lhs,
				// 	node->kind,
				// 	rhs
				// );

				new_node->lhs = lhs_node;
				new_node->rhs = rhs_node;

				new_node->op = op;
				new_node->type = type;

				return { type, new_node, bindings };
			} break;
		}

		return { TypeKind::None, new_node, bindings };
	}

	///////////////
	// Evaluator //
	///////////////

	struct EvalEnvironment {
		std::unordered_map<std::string_view, const BoxNode> bindings;
	};

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, EvalEnvironment env, BoxNode node);

	[[nodiscard]] inline Sequence
	pass_evaluator(Configuration cfg, BoxNode node) {
		CANE_FUNC();

		EvalEnvironment env;

		auto value = pass_evaluator_walk(cfg, env, node);
		CANE_OKAY(CANE_BOLD "value = {}" CANE_RESET, value);

		// Valid cane programs should return a pattern/sequence with a list of
		// events.
		if (std::holds_alternative<Sequence>(value)) {
			auto seq = std::get<Sequence>(value);

			CANE_OKAY("success!");
			return seq;
		}

		cane::report(ReportKind::Type, "program should return pattern type");
	}

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, EvalEnvironment env, BoxNode node) {
		if (node == nullptr) {
			return std::monostate {};
		}

		// Trivial/special cases
		switch (node->kind) {
			case SymbolKind::String: {
				return std::string { node->sv };
			} break;

			case SymbolKind::Number: {
				Scalar s = 0;

				auto number_sv = node->sv;
				auto [ptr, err] = std::from_chars(
					number_sv.data(), number_sv.data() + number_sv.size(), s
				);

				if (err != std::errc()) {
					cane::report(
						ReportKind::Eval,
						"unable to parse integer `{}`",
						number_sv
					);
				}

				return s;
			} break;

			case SymbolKind::Beat: {
				return Rhythm { EventKind::Beat };
			} break;

			case SymbolKind::Rest: {
				return Rhythm { EventKind::Rest };
			} break;

			case SymbolKind::Call: {
				cane::report(ReportKind::Semantic, "unresolved call");
				return std::monostate {};
			} break;

			case SymbolKind::Identifier: {
				cane::report(
					ReportKind::Semantic,
					"identifier `{}` not resolved in call context",
					node->sv
				);

				return std::monostate {};
			} break;

			// NOTE: These should already have been resolved by semantic
			// analysis pass so a function or binding don't need to be walked.
			case SymbolKind::Let:
			case SymbolKind::Function: {
				return std::monostate {};
			} break;

			case SymbolKind::Block: {
				CANE_UNUSED(pass_evaluator_walk(cfg, env, node->lhs));
				return pass_evaluator_walk(cfg, env, node->rhs);
			} break;

			default: break;
		}

		Value lhs = pass_evaluator_walk(cfg, env, node->lhs);
		Value rhs = pass_evaluator_walk(cfg, env, node->rhs);

		switch (node->op) {
			// Unary Scalar
			case SymbolKind::AbsScalar:
				return std::abs(std::move(std::get<Scalar>(lhs)));
			case SymbolKind::NegScalar: return -std::get<Scalar>(lhs);

			case SymbolKind::IncrScalar: return std::get<Scalar>(lhs) + 1;
			case SymbolKind::DecrScalar: return std::get<Scalar>(lhs) - 1;

			// Binary Scalar
			case SymbolKind::AddScalarScalar:
				return std::get<Scalar>(lhs) + std::get<Scalar>(rhs);

			case SymbolKind::SubScalarScalar:
				return std::get<Scalar>(lhs) - std::get<Scalar>(rhs);

			case SymbolKind::MulScalarScalar:
				return std::get<Scalar>(lhs) * std::get<Scalar>(rhs);

			case SymbolKind::DivScalarScalar:
				return std::get<Scalar>(lhs) / std::get<Scalar>(rhs);

			case SymbolKind::LeftShiftScalarScalar:
				return std::get<Scalar>(lhs) << std::get<Scalar>(rhs);

			case SymbolKind::RightShiftScalarScalar:
				return std::get<Scalar>(lhs) >> std::get<Scalar>(rhs);

			case SymbolKind::LCMScalarScalar:
				return std::lcm(
					std::move(std::get<Scalar>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::GCDScalarScalar:
				return std::gcd(
					std::move(std::get<Scalar>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			// Unary vectors
			case SymbolKind::CoerceScalar:
				return Melody { std::get<Scalar>(lhs) };

			case SymbolKind::CoerceMelody: return lhs;

			case SymbolKind::InvertRhythm:
				return bit_not(std::move(std::get<Rhythm>(lhs)));

			case SymbolKind::ReverseRhythm:
				return reverse(std::move(std::get<Rhythm>(lhs)));

			case SymbolKind::ReverseMelody:
				return reverse(std::move(std::get<Melody>(lhs)));

			// Head/Tail
			case SymbolKind::HeadMelody:
				return head(std::move(std::get<Melody>(lhs)), 1);

			case SymbolKind::TailMelody:
				return tail(std::move(std::get<Melody>(lhs)), 1);

			case SymbolKind::HeadRhythm:
				return head(std::move(std::get<Rhythm>(lhs)), 1);

			case SymbolKind::TailRhythm:
				return tail(std::move(std::get<Rhythm>(lhs)), 1);

			case SymbolKind::HeadSequence:
				return head(std::move(std::get<Sequence>(lhs)), 1);

			case SymbolKind::TailSequence:
				return tail(std::move(std::get<Sequence>(lhs)), 1);

			case SymbolKind::HeadPattern:
				return head(std::move(std::get<Sequence>(lhs)), 1);

			case SymbolKind::TailPattern:
				return tail(std::move(std::get<Sequence>(lhs)), 1);

			// Misc.
			case SymbolKind::EuclideanScalarScalar:
				return euclidean<Rhythm>(
					std::move(std::get<Scalar>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::RandomScalarScalar:
				return choice(
					std::move(std::get<Scalar>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			// Rotate vectors
			case SymbolKind::LeftShiftMelodyScalar:
				return rotate_left(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::RightShiftMelodyScalar:
				return rotate_right(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::LeftShiftRhythmScalar:
				return rotate_left(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::RightShiftRhythmScalar:
				return rotate_right(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			// Arithmetic on vectors
			case SymbolKind::AddMelodyScalar:
				return scalar_add(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::SubMelodyScalar:
				return scalar_sub(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::MulMelodyScalar:
				return scalar_mul(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::DivMelodyScalar:
				return scalar_div(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::AddMelodyMelody:
				return vector_add(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Melody>(rhs))
				);

			case SymbolKind::SubMelodyMelody:
				return vector_sub(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Melody>(rhs))
				);

			case SymbolKind::MulMelodyMelody:
				return vector_mul(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Melody>(rhs))
				);

			case SymbolKind::DivMelodyMelody:
				return vector_div(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Melody>(rhs))
				);

			// Repeat
			case SymbolKind::RepeatMelodyScalar:
				return repeat(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::RepeatRhythmScalar:
				return repeat(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			// Concatenate
			case SymbolKind::ConcatenateScalarScalar: {
				Melody melody;

				melody.emplace_back(std::get<Scalar>(lhs));
				melody.emplace_back(std::get<Scalar>(rhs));

				return melody;
			} break;

			case SymbolKind::ConcatenateMelodyScalar: {
				auto melody = std::get<Melody>(lhs);
				melody.emplace_back(std::get<Scalar>(rhs));
				return melody;
			} break;

			case SymbolKind::ConcatenateScalarMelody: {
				auto melody = std::get<Melody>(rhs);
				melody.emplace_back(std::get<Scalar>(lhs));
				return melody;
			} break;

			case SymbolKind::ConcatenateMelodyMelody:
				return concatenate(
					std::move(std::get<Melody>(lhs)),
					std::move(std::get<Melody>(rhs))
				);

			case SymbolKind::ConcatenateRhythmRhythm:
				return concatenate(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Rhythm>(rhs))
				);

			case SymbolKind::ConcatenateSequenceSequence:
			case SymbolKind::ConcatenatePatternPattern:
			case SymbolKind::ConcatenatePatternSequence:
			case SymbolKind::ConcatenateSequencePattern:
				// All of these concat ops operate on the same underlying type
				// and do the same thing.
				return join(
					std::move(std::get<Sequence>(lhs)),
					std::move(std::get<Sequence>(rhs))
				);

			// Layer
			case SymbolKind::LayerSequenceSequence:
			case SymbolKind::LayerPatternPattern:
			case SymbolKind::LayerPatternSequence:
			case SymbolKind::LayerSequencePattern:
				return concatenate(
					std::move(std::get<Sequence>(lhs)),
					std::move(std::get<Sequence>(rhs))
				);

			// Logical
			case SymbolKind::OrRhythmRhythm:
				return bit_or(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Rhythm>(rhs))
				);

			case SymbolKind::XorRhythmRhythm:
				return bit_xor(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Rhythm>(rhs))
				);

			case SymbolKind::AndRhythmRhythm:
				return bit_and(
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Rhythm>(rhs))
				);

			// Time divisions
			case SymbolKind::MulSequenceScalar:
				return timemul(
					std::move(std::get<Sequence>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			case SymbolKind::DivSequenceScalar:
				return timediv(
					std::move(std::get<Sequence>(lhs)),
					std::move(std::get<Scalar>(rhs))
				);

			// Mapping
			case SymbolKind::MapRhythmScalar: {
				return map<Sequence>(
					MINUTE / cfg.bpm,
					std::move(std::get<Rhythm>(lhs)),
					Melody { std::get<Scalar>(rhs) }
				);
			} break;

			case SymbolKind::MapRhythmMelody: {
				return map<Sequence>(
					MINUTE / cfg.bpm,
					std::move(std::get<Rhythm>(lhs)),
					std::move(std::get<Melody>(rhs))
				);
			} break;

			case SymbolKind::MapScalarRhythm: {
				return map<Sequence>(
					MINUTE / cfg.bpm,
					std::move(std::get<Rhythm>(rhs)),
					Melody { std::get<Scalar>(lhs) }
				);
			} break;

			case SymbolKind::MapMelodyRhythm: {
				return map<Sequence>(
					MINUTE / cfg.bpm,
					std::move(std::get<Rhythm>(rhs)),
					Melody { std::get<Scalar>(lhs) }
				);
			} break;

			case SymbolKind::SendPatternString:
			case SymbolKind::SendSequenceString: {
				// Sequences and patterns are just the same data structure in
				// C++ land.

				auto seq = std::get<Sequence>(lhs);
				auto str = std::get<String>(rhs);

				if (auto it = cfg.channel_bindings.find(str);
					it != cfg.channel_bindings.end()) {
					for (auto& ev: seq) {
						ev.channel = it->second;
					}

					return seq;
				}

				cane::report(
					ReportKind::Eval, "unknown channel binding `{}`", str
				);
			} break;

			default: {
				cane::report(
					ReportKind::Internal,
					"unable to evaluate `{}`",
					symbol_kind_to_str(node->kind)
				);
			} break;
		}

		CANE_UNREACHABLE();
	}

}  // namespace cane

#endif
