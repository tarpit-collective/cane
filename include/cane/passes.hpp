#ifndef CANE_PASSES_HPP
#define CANE_PASSES_HPP

#include <memory>
#include <print>
#include <vector>
#include <unordered_map>

#include <cane/parse.hpp>
#include <cane/ops.hpp>

////////////
// Passes //
////////////

namespace cane {

	//////////////////////
	// AST Printer Pass //
	//////////////////////

	inline void pass_print_walk(
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
			pass_print_walk(cfg, node->lhs, bits, depth);
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
			pass_print_walk(cfg, node->rhs, bits, depth);
			bits.pop_back();
		}
	}  // namespace detail

	inline void pass_print(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		pass_print_walk(cfg, node);
	}

	inline void pass_print_walk(
		Configuration cfg, BoxNode node, std::vector<bool> bits, size_t depth
	) {
		if (node == nullptr) {
			std::cout << detail::SIGIL_NULL << '\n';
			return;
		}

		bool is_root = depth == 0;
		bool is_leaf = cane::is_literal(node->kind);

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
	}

	////////////////////////
	// Binding Resolution //
	////////////////////////

	struct BindingEnvironment {
		std::unordered_map<std::string_view, BoxNode> bindings;
	};

	[[nodiscard]] inline BoxNode pass_binding_resolution_walk(
		Configuration cfg, BindingEnvironment& env, BoxNode node
	);

	[[nodiscard]] inline BoxNode
	pass_binding_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		BindingEnvironment env;

		return pass_binding_resolution_walk(cfg, env, node);
	}

	[[nodiscard]] inline BoxNode pass_binding_resolution_walk(
		Configuration cfg, BindingEnvironment& env, BoxNode node
	) {
		if (node == nullptr) {
			return nullptr;
		}

		switch (node->kind) {
			case SymbolKind::Function: {
				CANE_OKAY("Function {}", node->sv);
				return node;
			} break;

			case SymbolKind::Identifier: {
				// Look up the binding in the environment, if it doesn't exist,
				// we just bail out.

				// If we _do_ find a match, we replace the current node in the
				// AST (an identifier) with the node stored in the environment.

				// This is lazy eval as we copy the tree directly rather than
				// evaluating it to a simpler form first.

				CANE_OKAY("Ident {}", node->sv);

				auto it = env.bindings.find(node->sv);

				if (it == env.bindings.end()) {
					cane::report(
						ReportKind::Semantic, "unknown binding `{}`", node->sv
					);
				}

				return it->second;
			} break;

			case SymbolKind::Assign: {
				// Assignment stores a mapping of string_view to an AST node in
				// the environment.

				// We need to visit the expression on the left-hand side of the
				// assignment node in order to fully resolve any nested
				// references to bindings.

				// It's important that this node still remains in the tree since
				// it will later contribute to type checking.

				CANE_OKAY("Assign {}", node->sv);

				auto binding = node->rhs;

				cane::report_if(
					binding != nullptr and
						binding->kind != SymbolKind::Identifier,
					ReportKind::Syntactical,
					"expected an identifier"
				);

				CANE_OKAY("Binding {}", binding->sv);

				node->lhs = pass_binding_resolution_walk(cfg, env, node->lhs);

				auto [it, succ] =
					env.bindings.try_emplace(binding->sv, node->lhs);

				if (not succ) {
					cane::report(
						ReportKind::Semantic,
						"attempting to rebind `{}`",
						binding->sv
					);
				}

				return node;
			} break;

			default: {
				CANE_OKAY("Default {}", node->sv);

				// Visit both children, even in the case of unary nodes.
				// We will exit early if any child is a nullptr anyway.

				// Most nodes will take this path since we only care about
				// handling bindings and references.

				node->lhs = pass_binding_resolution_walk(cfg, env, node->lhs);
				node->rhs = pass_binding_resolution_walk(cfg, env, node->rhs);

				return node;
			} break;
		}

		return node;
	}

	//////////////////
	// Type Checker //
	//////////////////

	struct TypeEnvironment {
		std::unordered_map<std::string_view, TypeKind> bindings;
	};

	[[nodiscard]] inline TypeKind pass_type_resolution_walk(
		Configuration cfg, TypeEnvironment& env, BoxNode node
	);

	[[nodiscard]] inline BoxNode
	pass_type_resolution(Configuration cfg, BoxNode node) {
		CANE_FUNC();
		TypeEnvironment env;

		auto type = pass_type_resolution_walk(cfg, env, node);

		cane::report_if(
			type != TypeKind::Pattern,
			ReportKind::Type,
			"expected a pattern as output type of program"
		);

		return node;
	}

	[[nodiscard]] inline bool type_remap_trivial(
		[[maybe_unused]] Configuration cfg,
		TypeKind lhs,
		TypeKind rhs,
		BoxNode node
	) {
		// CANE_INFO(
		// 	"attempt " CANE_COLOUR_BLUE "`{}`" CANE_RESET
		// 	" [ `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
		// 	"`, `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
		// 	"` ] -> " CANE_COLOUR_MAGENTA "{}" CANE_RESET,

		// 	symbol_kind_to_str(kind),
		// 	expected_lhs,
		// 	lhs,
		// 	expected_rhs,
		// 	rhs,
		// 	out
		// );

		auto remap = [&](SymbolKind kind,

						 TypeKind expected_lhs,
						 TypeKind expected_rhs,

						 TypeKind out,
						 SymbolKind op) {
			if (node == nullptr or node->kind != kind) {
				return false;
			}

			if (lhs != expected_lhs || rhs != expected_rhs) {
				// If the types don't match, it just means this overload of
				// the operator isn't the correct one but we might have one
				// handled later.
				return false;
			}

			node->type = out;
			node->op = op;

			return true;
		};

		// clang-format off
	#define CANE_TYPE_REMAP(symbol, lhs_type, rhs_type, out_type, out_symbol) \
		remap( \
			SymbolKind::symbol, \
			TypeKind::lhs_type, \
			TypeKind::rhs_type, \
			TypeKind::out_type, \
			SymbolKind::out_symbol \
		)

 	bool match =
 		/* Prefix/Unary */
 		CANE_TYPE_REMAP(Abs, Scalar, None, Scalar, AbsScalar) ||
 		CANE_TYPE_REMAP(Neg, Scalar, None, Scalar, NegScalar) ||

 		CANE_TYPE_REMAP(Invert, Rhythm, None, Rhythm, InvertRhythm) ||
 		CANE_TYPE_REMAP(Reverse, Rhythm, None, Rhythm, ReverseRhythm) ||

 		CANE_TYPE_REMAP(Reverse, Melody, None, Melody, ReverseMelody) ||

 		CANE_TYPE_REMAP(Incr, Scalar, None, Scalar, IncrScalar) ||
 		CANE_TYPE_REMAP(Decr, Scalar, None, Scalar, DecrScalar) ||

 		CANE_TYPE_REMAP(Coerce, Scalar, None, Melody, CoerceScalar) ||
 		CANE_TYPE_REMAP(Coerce, Melody, None, Melody, CoerceMelody) ||

 		/* Scalar */
 		CANE_TYPE_REMAP(Add, Scalar, Scalar, Scalar, AddScalarScalar) ||
 		CANE_TYPE_REMAP(Sub, Scalar, Scalar, Scalar, SubScalarScalar) ||
 		CANE_TYPE_REMAP(Mul, Scalar, Scalar, Scalar, MulScalarScalar) ||
 		CANE_TYPE_REMAP(Div, Scalar, Scalar, Scalar, DivScalarScalar) ||

 		CANE_TYPE_REMAP(LeftShift, Scalar, Scalar, Scalar, LeftShiftScalarScalar) ||
 		CANE_TYPE_REMAP(RightShift, Scalar, Scalar, Scalar, RightShiftScalarScalar) ||

 		CANE_TYPE_REMAP(LCM, Scalar, Scalar, Scalar, LCMScalarScalar) ||
 		CANE_TYPE_REMAP(GCD, Scalar, Scalar, Scalar, GCDScalarScalar) ||

 		CANE_TYPE_REMAP(Euclidean, Scalar, Scalar, Rhythm, EuclideanScalarScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Scalar, Scalar, Melody, ConcatenateScalarScalar) ||
		CANE_TYPE_REMAP(Random, Scalar, Scalar, Scalar, RandomScalarScalar) ||

 		/* Melody */
 		CANE_TYPE_REMAP(Map, Melody, Rhythm, Sequence, MapMelodyRhythm) ||

 		CANE_TYPE_REMAP(LeftShift, Melody, Scalar, Melody, LeftShiftMelodyScalar) ||
 		CANE_TYPE_REMAP(RightShift, Melody, Scalar, Melody, RightShiftMelodyScalar) ||

 		CANE_TYPE_REMAP(Add, Melody, Scalar, Melody, AddMelodyScalar) ||
 		CANE_TYPE_REMAP(Sub, Melody, Scalar, Melody, SubMelodyScalar) ||
 		CANE_TYPE_REMAP(Mul, Melody, Scalar, Melody, MulMelodyScalar) ||
 		CANE_TYPE_REMAP(Div, Melody, Scalar, Melody, DivMelodyScalar) ||

 		CANE_TYPE_REMAP(Repeat, Melody, Scalar, Melody, RepeatMelodyScalar) ||

 		CANE_TYPE_REMAP(Concatenate, Melody, Melody, Melody, ConcatenateMelodyMelody) ||
 		CANE_TYPE_REMAP(Concatenate, Melody, Scalar, Melody, ConcatenateMelodyScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Scalar, Melody, Melody, ConcatenateScalarMelody) ||

 		/* Rhythm */
 		CANE_TYPE_REMAP(Map, Rhythm, Melody, Sequence, MapRhythmMelody) ||

 		CANE_TYPE_REMAP(LeftShift, Rhythm, Scalar, Rhythm, LeftShiftRhythmScalar) ||
 		CANE_TYPE_REMAP(RightShift, Rhythm, Scalar, Rhythm, RightShiftRhythmScalar) ||

 		CANE_TYPE_REMAP(Repeat, Rhythm, Scalar, Rhythm, RepeatRhythmScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Rhythm, Rhythm, Rhythm, ConcatenateRhythmRhythm) ||

 		CANE_TYPE_REMAP(Or, Rhythm, Rhythm, Rhythm, OrRhythmRhythm) ||
 		CANE_TYPE_REMAP(Xor, Rhythm, Rhythm, Rhythm, XorRhythmRhythm) ||
 		CANE_TYPE_REMAP(And, Rhythm, Rhythm, Rhythm, AndRhythmRhythm) ||

 		/* Sequence */
 		CANE_TYPE_REMAP(Concatenate, Sequence, Sequence, Sequence, ConcatenateSequenceSequence) ||

 		CANE_TYPE_REMAP(Mul, Sequence, Scalar, Sequence, MulSequenceScalar) ||
 		CANE_TYPE_REMAP(Div, Sequence, Scalar, Sequence, DivSequenceScalar) ||

 		/* Pattern */
 		CANE_TYPE_REMAP(Send, Sequence, String, Pattern, SendSequenceString)
 	;
		// clang-format on

		if (match) {
			CANE_OKAY(
				"found a valid type mapping: `{}` {} `{}`", lhs, node->kind, rhs
			);
		}

		else {
			CANE_FAIL(
				"did not find a valid type mapping: `{}` {} `{}`",
				lhs,
				node->kind,
				rhs
			);
		}

		return match;

#undef CANE_TYPE_REMAP
	}

	[[nodiscard]] inline TypeKind pass_type_resolution_walk(
		Configuration cfg, TypeEnvironment& env, BoxNode node
	) {
		if (node == nullptr) {
			return TypeKind::None;
		}

		switch (node->kind) {
			// Literals
			case SymbolKind::Number: return TypeKind::Scalar;
			case SymbolKind::String: return TypeKind::String;

			case SymbolKind::Beat:
			case SymbolKind::Rest: return TypeKind::Rhythm;

			// Assignment
			case SymbolKind::Identifier:

			case SymbolKind::Assign: {
				CANE_UNREACHABLE();
			} break;

			case SymbolKind::Function: {
				// TODO: Fix this, not sure if this is actually correct in
				// all cases.

				// Should give the type of the return value of
				// the function.

				// Should assert that the `rhs` is valid.
				// Do we just visit the rhs?
				return node->rhs->type;
			} break;

			case SymbolKind::Call: {
				// TODO: We need to check if the argument on the right
				// matches the type of the function parameter.

				// TODO: Get parameter type of function.
				// Also need to assert that the node on the left _is_ a
				// function.

				// FIXME: This is broken and doesn't align with what we
				// expect.

				// TypeKind function = lhs;
				// TypeKind argument = rhs;

				// if (function != argument) {
				// 	cane::report(
				// 		ReportKind::Type, "mismatched type `{}`", node->kind
				// 	);
				// }

				// return function;
			} break;

			// Statements always return the type of their last expression.
			// TODO: We might be able to implement this as a trivial type
			// remapping once we have proper support for "patterns" since a
			// cane program should evaluate to a fully mapped list of
			// events.
			case SymbolKind::Statement: {
				CANE_UNUSED(pass_type_resolution_walk(cfg, env, node->lhs));

				TypeKind trailing =
					pass_type_resolution_walk(cfg, env, node->rhs);

				// We return the last expression's type.
				node->type = trailing;
				return trailing;
			} break;

			default: {
				// Attempt trivial cases.
				TypeKind lhs = pass_type_resolution_walk(cfg, env, node->lhs);
				TypeKind rhs = pass_type_resolution_walk(cfg, env, node->rhs);

				if (not type_remap_trivial(cfg, lhs, rhs, node)) {
					cane::report(
						ReportKind::Type,
						"no mapping for `{}` {} `{}`",
						lhs,
						node->kind,
						rhs
					);
				}
			} break;
		}

		return node->type;
	}

	///////////////
	// Evaluator //
	///////////////

	struct EvalEnvironment {
		std::mt19937_64 rng;
	};

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, EvalEnvironment env, BoxNode node);

	[[nodiscard]] inline Value pass_evaluator(Configuration cfg, BoxNode node) {
		CANE_FUNC();

		std::random_device rd;
		EvalEnvironment env { .rng = std::mt19937_64 { rd() } };

		return pass_evaluator_walk(cfg, env, node);
	}

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, EvalEnvironment env, BoxNode node) {
		if (node == nullptr) {
			return std::monostate {};  // Cons lists will enter this case.
		}

		// Trivial/special cases
		switch (node->kind) {
			case SymbolKind::String: {
				return node->sv;
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

			// TODO: Need to handle calls seperately since the type can be
			// anything and we don't have a static node->op to switch on.
			case SymbolKind::Call: {
				CANE_UNIMPLEMENTED();
			} break;

			case SymbolKind::Statement: {
				CANE_UNUSED(pass_evaluator_walk(cfg, env, node->lhs));
				return pass_evaluator_walk(cfg, env, node->rhs);
			} break;

			default: break;
		}

		Value lhs = pass_evaluator_walk(cfg, env, node->lhs);
		Value rhs = pass_evaluator_walk(cfg, env, node->rhs);

		switch (node->op) {
			// Unary Scalar
			case SymbolKind::AbsScalar: return lhs.absolute();
			case SymbolKind::NegScalar: return lhs.negate();

			case SymbolKind::IncrScalar: return lhs.get_scalar() + 1;
			case SymbolKind::DecrScalar: return lhs.get_scalar() - 1;

			// Binary Scalar
			case SymbolKind::AddScalarScalar: return lhs.add(rhs);
			case SymbolKind::SubScalarScalar: return lhs.sub(rhs);
			case SymbolKind::MulScalarScalar: return lhs.mul(rhs);
			case SymbolKind::DivScalarScalar: return lhs.div(rhs);

			case SymbolKind::LeftShiftScalarScalar: return lhs.shift_left(rhs);
			case SymbolKind::RightShiftScalarScalar:
				return lhs.shift_right(rhs);

			case SymbolKind::LCMScalarScalar: return lhs.lcm(rhs);
			case SymbolKind::GCDScalarScalar: return lhs.gcd(rhs);

			// Unary vectors
			case SymbolKind::CoerceScalar: return Melody { lhs.get_scalar() };
			case SymbolKind::CoerceMelody: return lhs;

			case SymbolKind::InvertRhythm: return lhs.invert<Rhythm>();
			case SymbolKind::ReverseRhythm: return lhs.reverse<Rhythm>();
			case SymbolKind::ReverseMelody: return lhs.reverse<Melody>();

			case SymbolKind::EuclideanScalarScalar: return lhs.euclidean(rhs);

			case SymbolKind::RandomScalarScalar:
				return lhs.choice(env.rng, rhs);

			// Rotate vectors
			case SymbolKind::LeftShiftMelodyScalar:
				return lhs.rotate_left<Melody>(rhs.get_scalar());

			case SymbolKind::RightShiftMelodyScalar:
				return lhs.rotate_right<Melody>(rhs.get_scalar());

			case SymbolKind::LeftShiftRhythmScalar:
				return lhs.rotate_left<Rhythm>(rhs.get_scalar());

			case SymbolKind::RightShiftRhythmScalar:
				return lhs.rotate_right<Rhythm>(rhs.get_scalar());

			// Arithmetic on vectors
			case SymbolKind::AddMelodyScalar:
				return lhs.seq_add(rhs.get_scalar());

			case SymbolKind::SubMelodyScalar:
				return lhs.seq_sub(rhs.get_scalar());

			case SymbolKind::MulMelodyScalar:
				return lhs.seq_mul(rhs.get_scalar());

			case SymbolKind::DivMelodyScalar:
				return lhs.seq_div(rhs.get_scalar());

			// Repeat
			case SymbolKind::RepeatMelodyScalar:
				return lhs.repeat<Melody>(rhs.get_scalar());

			case SymbolKind::RepeatRhythmScalar:
				return lhs.repeat<Rhythm>(rhs.get_scalar());

			// Concatenate
			case SymbolKind::ConcatenateScalarScalar: {
				Melody melody;

				melody.emplace_back(lhs.get_scalar());
				melody.emplace_back(rhs.get_scalar());

				return melody;
			} break;

			case SymbolKind::ConcatenateMelodyScalar: {
				auto melody = lhs.get_melody();
				melody.emplace_back(rhs.get_scalar());
				return melody;
			} break;

			case SymbolKind::ConcatenateScalarMelody: {
				auto melody = rhs.get_melody();
				melody.emplace_back(lhs.get_scalar());
				return melody;
			} break;

			case SymbolKind::ConcatenateMelodyMelody:
				return lhs.concatenate<Melody>(rhs);

			case SymbolKind::ConcatenateRhythmRhythm:
				return lhs.concatenate<Rhythm>(rhs);

			case SymbolKind::ConcatenateSequenceSequence:
				return lhs.concatenate<Sequence>(rhs);

			// Logical
			case SymbolKind::OrRhythmRhythm: return lhs.bit_or(rhs);
			case SymbolKind::XorRhythmRhythm: return lhs.bit_xor(rhs);
			case SymbolKind::AndRhythmRhythm: return lhs.bit_and(rhs);

			// Mapping
			case SymbolKind::MapRhythmMelody:
				return lhs.map_onto_rhythm(cfg, rhs);
			case SymbolKind::MapMelodyRhythm:
				return lhs.map_onto_melody(cfg, rhs);

			// Time divisions
			case SymbolKind::MulSequenceScalar: return lhs.timemul(rhs);
			case SymbolKind::DivSequenceScalar: return lhs.timediv(rhs);

			// Patterns
			case SymbolKind::SendSequenceString: return lhs.send(cfg, rhs);

			default: {
				cane::report(
					ReportKind::Internal, "unable to evaluate `{}`", node->kind
				);
			} break;
		}

		CANE_UNREACHABLE();
	}

}  // namespace cane

#endif
