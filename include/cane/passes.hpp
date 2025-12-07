#ifndef CANE_PASSES_HPP
#define CANE_PASSES_HPP

#include <memory>
#include <random>
#include <print>
#include <vector>
#include <unordered_map>

#include <cane/parse.hpp>
#include <cane/value.hpp>
#include <cane/ops.hpp>

////////////
// Passes //
////////////

namespace cane {

	inline Sequence pass_evaluator(Configuration cfg, BoxNode node);

	inline BoxNode pass_print(Configuration cfg, BoxNode node);
	inline BoxNode pass_binding_resolution(Configuration cfg, BoxNode node);
	inline BoxNode pass_type_resolution(Configuration cfg, BoxNode node);

	///////////////
	// Utilities //
	///////////////

	inline BoxNode deepcopy(BoxNode root) {
		if (root == nullptr) {
			return nullptr;
		}

		return std::make_shared<Node>(
			root->kind,
			root->op,
			root->sv,
			root->type,

			/* lhs = */ deepcopy(root->lhs),
			/* rhs = */ deepcopy(root->rhs)
		);
	}

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
				root = pass_print(cfg, pass(cfg, root));
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
	[[nodiscard]] decltype(auto) parse_and_compile(
		std::string_view source, Configuration cfg, Ts&&... passes
	) {
		cane::Parser parser { source };
		auto root = parser.parse();

		root = pipeline(cfg, root, std::forward<Ts>(passes)...);
		return pass_evaluator(cfg, root);
	}

	template <typename... Ts>
	[[nodiscard]] decltype(auto) debug_parse_and_compile(
		std::string_view source, Configuration cfg, Ts&&... passes
	) {
		cane::Parser parser { source };
		auto root = parser.parse();

		CANE_UNUSED(pass_print(cfg, root));

		root = debug_pipeline(cfg, root, std::forward<Ts>(passes)...);
		return pass_evaluator(cfg, root);
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
		CANE_FUNC();
		auto root = pass_print_walk(cfg, node);
		CANE_OKAY("success!");
		return root;
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

	////////////////////////
	// Binding Resolution //
	////////////////////////

	struct BindingEnvironment {
		std::unordered_map<std::string_view, const BoxNode> bindings;
	};

	[[nodiscard]] inline BoxNode pass_binding_resolution_walk(
		Configuration cfg,
		BindingEnvironment& env,
		BoxNode node,
		std::vector<BoxNode> args
	);

	[[nodiscard]] inline BoxNode
	pass_binding_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		BindingEnvironment env;

		auto root = pass_binding_resolution_walk(cfg, env, node, {});
		CANE_OKAY("success!");

		return root;
	}

	[[nodiscard]] inline BoxNode pass_binding_resolution_walk(
		Configuration cfg,
		BindingEnvironment& env,
		BoxNode node,
		std::vector<BoxNode> args
	) {
		CANE_UNUSED(pass_print(cfg, node));

		if (node == nullptr) {
			return nullptr;
		}

		switch (node->kind) {
			case SymbolKind::Function: {
				CANE_OKAY("Function {}", node->sv);

				if (args.empty()) {
					CANE_OKAY("Not called");
					return node;
				}

				CANE_OKAY("Is called");

				auto param = node->rhs;

				auto arg = args.back();
				args.pop_back();

				auto fn_env = env;
				fn_env.bindings.try_emplace(param->sv, arg);

				node->lhs =
					pass_binding_resolution_walk(cfg, fn_env, node->lhs, args);

				return node->lhs;
			} break;

			case SymbolKind::Call: {
				CANE_OKAY("Call");

				// Visit argument first
				node->rhs =
					pass_binding_resolution_walk(cfg, env, node->rhs, args);

				// If argument is nullptr, we can assume it was an uncalled
				// function passed as an argument to the function on our left.
				// This obviously means that this function has to be uncalled
				// aswell so let's just remove this node.
				if (node->rhs == nullptr) {
					return node;
				}

				// Visit function with newly evaluated argument above
				args.emplace_back(node->rhs);

				node->lhs =
					pass_binding_resolution_walk(cfg, env, node->lhs, args);

				return node->lhs;
			} break;

			case SymbolKind::Identifier: {
				CANE_OKAY("Identifier {}", node->sv);

				// Look up the binding in the environment, if it doesn't
				// exist, we just bail out.

				// If we _do_ find a match, we replace the current node in
				// the AST (an identifier) with the node stored in the
				// environment.

				// This is lazy eval as we copy the tree directly rather
				// than evaluating it to a simpler form first.

				auto it = env.bindings.find(node->sv);

				cane::report_if(
					it == env.bindings.end(),
					ReportKind::Semantic,
					"unknown binding `{}`",
					node->sv
				);

				auto expr = deepcopy(it->second);

				return pass_binding_resolution_walk(cfg, env, expr, args);
			} break;

			case SymbolKind::Assign: {
				// Assignment stores a mapping of string_view to an AST node
				// in the environment.

				// We need to visit the expression on the left-hand side of
				// the assignment node in order to fully resolve any nested
				// references to bindings.

				// It's important that this node still remains in the tree
				// since it will later contribute to type checking.

				auto binding = node->rhs;

				cane::report_if(
					binding != nullptr and
						binding->kind != SymbolKind::Identifier,
					ReportKind::Syntactical,
					"expected an identifier"
				);

				CANE_OKAY("Binding {}", binding->sv);

				auto expr = deepcopy(node->lhs);
				auto [it, succ] = env.bindings.try_emplace(binding->sv, expr);

				node->lhs =
					pass_binding_resolution_walk(cfg, env, node->lhs, args);

				cane::report_if(
					not succ,
					ReportKind::Semantic,
					"attempting to rebind `{}`",
					binding->sv
				);

				return node->lhs;
			} break;

			case SymbolKind::Block: {
				node->lhs =
					pass_binding_resolution_walk(cfg, env, node->lhs, args);

				node->rhs =
					pass_binding_resolution_walk(cfg, env, node->rhs, args);

				return node->rhs;
			} break;

			default: {
				// Visit both children, even in the case of unary nodes.
				// We will exit early if any child is a nullptr anyway.

				// Most nodes will take this path since we only care about
				// handling bindings and references.

				node->lhs =
					pass_binding_resolution_walk(cfg, env, node->lhs, args);

				node->rhs =
					pass_binding_resolution_walk(cfg, env, node->rhs, args);

				return node;
			} break;
		}

		return node;
	}

	//////////////////
	// Type Checker //
	//////////////////

	struct TypeEnvironment {
		std::unordered_map<std::string_view, BoxNode> bindings;
	};

	[[nodiscard]] inline std::pair<TypeKind, BoxNode> pass_type_resolution_walk(
		Configuration cfg,
		TypeEnvironment& env,
		BoxNode node,
		std::vector<BoxNode> args
	);

	[[nodiscard]] inline BoxNode
	pass_type_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		TypeEnvironment env;

		auto [root, root_node] = pass_type_resolution_walk(cfg, env, node, {});

		CANE_UNUSED(pass_print(cfg, node));

		// cane::report_if(
		// 	type != TypeKind::Pattern,
		// 	ReportKind::Type,
		// 	"expected a pattern type"
		// );

		CANE_OKAY("success!");
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

	[[nodiscard]] inline std::pair<TypeKind, BoxNode> pass_type_resolution_walk(
		Configuration cfg,
		TypeEnvironment& env,
		BoxNode node,
		std::vector<BoxNode> args
	) {
		CANE_UNUSED(pass_print(cfg, node));

		if (node == nullptr) {
			return { TypeKind::None, nullptr };
		}

		switch (node->kind) {
			// Literals
			case SymbolKind::Number: {
				return { TypeKind::Scalar, node };
			} break;

			case SymbolKind::String: {
				return { TypeKind::String, node };
			} break;

			case SymbolKind::Beat:
			case SymbolKind::Rest: {
				return { TypeKind::Rhythm, node };
			} break;

			case SymbolKind::Function: {
				// Uncalled function.
				if (args.empty()) {
					return { TypeKind::Function, node };
				}

				// Pop and bind argument in function environment.
				// Do not visit the argument here, we only visit it once it's
				// _used_.
				auto arg = args.back();
				args.pop_back();

				auto param = node->rhs;

				cane::report_if(
					param != nullptr and param->kind != SymbolKind::Identifier,
					ReportKind::Syntactical,
					"expected an identifier"
				);

				auto fn_env = env;
				fn_env.bindings.try_emplace(param->sv, arg);

				auto [fn, fn_node] =
					pass_type_resolution_walk(cfg, fn_env, node->lhs, args);

				node->type = fn;

				return { fn, fn_node };
			} break;

			case SymbolKind::Call: {
				auto [arg, arg_node] =
					pass_type_resolution_walk(cfg, env, node->rhs, args);

				args.emplace_back(arg_node);

				auto [call, call_node] =
					pass_type_resolution_walk(cfg, env, node->lhs, args);

				node->type = call;

				return { call, call_node };
			} break;

			case SymbolKind::Identifier: {
				auto it = env.bindings.find(node->sv);

				cane::report_if(
					it == env.bindings.end(),
					ReportKind::Semantic,
					"unknown binding `{}`",
					node->sv
				);

				auto expr = deepcopy(it->second);

				auto [ident, ident_body] =
					pass_type_resolution_walk(cfg, env, expr, args);

				node->type = ident;

				return { ident, ident_body };
			} break;

			case SymbolKind::Assign: {
				auto binding = node->rhs;

				cane::report_if(
					binding != nullptr and
						binding->kind != SymbolKind::Identifier,
					ReportKind::Syntactical,
					"expected an identifier"
				);

				// NOTE: We have to bind the node before we visit it otherwise
				// things like functions are thrown away due to being uncalled.
				auto [assign, assign_node] =
					pass_type_resolution_walk(cfg, env, node->lhs, args);

				auto expr = deepcopy(node->lhs);
				auto [it, succ] = env.bindings.try_emplace(binding->sv, expr);

				cane::report_if(
					not succ,
					ReportKind::Semantic,
					"attempting to rebind `{}`",
					binding->sv
				);

				node->type = assign;

				return { assign, assign_node };
			} break;

			case SymbolKind::Block: {
				// We return the last expression's type.

				auto [lhs, lhs_node] =
					pass_type_resolution_walk(cfg, env, node->lhs, args);

				auto [rhs, rhs_node] =
					pass_type_resolution_walk(cfg, env, node->rhs, args);

				node->type = rhs;

				return { rhs, rhs_node };
			} break;

			default: {
				// Attempt trivial cases.
				auto [lhs, lhs_node] =
					pass_type_resolution_walk(cfg, env, node->lhs, args);

				auto [rhs, rhs_node] =
					pass_type_resolution_walk(cfg, env, node->rhs, args);

				auto [succ, type, op] = type_remap_trivial(cfg, lhs, rhs, node);

				if (succ) {
					CANE_OKAY(
						"found a valid type mapping: `{}` {} `{}`",
						lhs,
						node->kind,
						rhs
					);
				}

				else {
					CANE_FAIL(
						"did not find a valid type mapping: `{}`({}) {} "
						"`{}`({})",
						lhs,
						node->lhs == nullptr ? "nullptr" : node->lhs->sv,
						node->kind,
						rhs,
						node->rhs == nullptr ? "nullptr" : node->rhs->sv
					);
				}

				cane::report_if(
					not succ,
					ReportKind::Type,
					"no mapping for `{}` {} `{}`",
					lhs,
					node->kind,
					rhs
				);

				auto new_node = deepcopy(node);

				new_node->lhs = lhs_node;
				new_node->rhs = rhs_node;

				new_node->op = op;
				new_node->type = type;

				return { type, new_node };
			} break;
		}

		return { TypeKind::None, node };
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

		EvalEnvironment env {
			.bindings = {},
		};

		auto value = pass_evaluator_walk(cfg, env, node);
		CANE_OKAY(CANE_BOLD "value = {}" CANE_RESET, value);

		if (std::holds_alternative<Sequence>(value)) {
			auto seq = std::get<Sequence>(value);
			// std::ranges::sort(seq, {}, &Event::key);

			CANE_OKAY("success!");
			return seq;
		}

		cane::report(ReportKind::Type, "program should return pattern type");
	}

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, EvalEnvironment env, BoxNode node) {
		if (node == nullptr) {
			return std::monostate {};  // Cons lists will enter this case.
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

			case SymbolKind::Identifier: {
				cane::report(
					ReportKind::Internal,
					"identifier not resolved in call context"
				);

				return std::monostate {};
			} break;

			case SymbolKind::Assign: {
				// `rhs` is just an identifier, so walk eval `lhs`
				return pass_evaluator_walk(cfg, env, node->lhs);
			} break;

			case SymbolKind::Call: {
				// Argument was already passed down and expanded during binding
				// resolution pass so everything we need is on the left-hand
				// side and we can discard the right-hand side.
				CANE_UNUSED(pass_evaluator_walk(cfg, env, node->rhs));

				return pass_evaluator_walk(cfg, env, node->lhs);
			} break;

			case SymbolKind::Function: {
				// Just return the left hand side (body) since we've already
				// visited this during the binding resolution pass and expanded
				// out any bindings (including call arguments).

				// Right hand side is just an identifier anyway.
				return pass_evaluator_walk(cfg, env, node->lhs);
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
