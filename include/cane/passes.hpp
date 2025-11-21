#ifndef CANE_PASSES_HPP
#define CANE_PASSES_HPP

#include <memory>
#include <print>
#include <vector>

#include <cane/parse.hpp>
#include <cane/ops.hpp>

namespace cane {

	//////////////////////
	// AST Printer Pass //
	//////////////////////

	inline void pass_print_walker(
		std::shared_ptr<Node> node,
		std::vector<bool> bits = {},
		size_t depth = 0
	);

	namespace detail {
		inline std::string_view SIGIL_NULL = CANE_CSTR("⊗");
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
			std::shared_ptr<Node> node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_LHS, bits);

			bits.push_back(true);
			pass_print_walker(node->lhs, bits, depth);
			bits.pop_back();
		}

		inline void pass_print_indent_node_child_rhs(
			std::shared_ptr<Node> node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_RHS, bits);

			bits.push_back(false);
			pass_print_walker(node->rhs, bits, depth);
			bits.pop_back();
		}
	}  // namespace detail

	inline void pass_print(std::shared_ptr<Node> node) {
		CANE_FUNC();
		pass_print_walker(node);
	}

	inline void pass_print_walker(
		std::shared_ptr<Node> node, std::vector<bool> bits, size_t depth
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
			detail::pass_print_indent_node_child_lhs(node, bits, depth + 1);
			detail::pass_print_indent_node_child_rhs(node, bits, depth + 1);
		}
	}

	//////////////////
	// Type Checker //
	//////////////////

	inline TypeKind pass_semantic_analysis_walker(std::shared_ptr<Node> node);

	inline void pass_semantic_analysis(std::shared_ptr<Node> node) {
		CANE_FUNC();
		pass_semantic_analysis_walker(node);
	}

	// TODO:
	// 1. We need to store assigned types
	// 2. Function types
	inline bool type_remapper(
		std::shared_ptr<Node> node,

		TypeKind lhs,
		TypeKind rhs,

		SymbolKind kind,

		TypeKind expected_lhs,
		TypeKind expected_rhs,

		TypeKind out,
		SymbolKind op
	) {
		if (node == nullptr or node->kind != kind) {
			return false;
		}

		CANE_INFO(
			"attempt " CANE_COLOUR_BLUE "`{}`" CANE_RESET
			" [ `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
			"`, `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
			"` ] -> " CANE_COLOUR_MAGENTA "{}" CANE_RESET,

			symbol_kind_to_str(kind),
			expected_lhs,
			lhs,
			expected_rhs,
			rhs,
			out
		);

		if (lhs != expected_lhs || rhs != expected_rhs) {
			// If the types don't match, it just means this overload of the
			// operator isn't the correct one but we might have one handled
			// later.
			CANE_FAIL("└► " CANE_COLOUR_RED "failed!" CANE_RESET);
			return false;
		}

		CANE_OKAY("└► " CANE_COLOUR_YELLOW "success!" CANE_RESET);

		node->type = out;
		node->op = op;

		return true;
	}

	// TODO:
	// We need to figure out how to handle rhythms with beats/rests and how
	// to type check them. They really need to be a unified type.
	inline bool
	type_remap_trivial(TypeKind lhs, TypeKind rhs, std::shared_ptr<Node> node) {
#define CANE_TYPE_REMAP(symbol, lhs_type, rhs_type, out_type, out_symbol) \
	type_remapper( \
		node, \
\
		lhs, \
		rhs, \
\
		SymbolKind::symbol, \
\
		TypeKind::lhs_type, \
		TypeKind::rhs_type, \
\
		TypeKind::out_type, \
		SymbolKind::out_symbol \
	)

		// clang-format off
 	return
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

#undef CANE_TYPE_REMAP
	}

	inline TypeKind pass_semantic_analysis_walker(std::shared_ptr<Node> node) {
		if (node == nullptr) {
			return TypeKind::None;
		}

		// In the case of a UNARY remapping, rhs will match with NONE anyway so
		// we can always just compare both types.
		TypeKind lhs = pass_semantic_analysis_walker(node->lhs);
		TypeKind rhs = pass_semantic_analysis_walker(node->rhs);

		// Handle trivial cases for remapping first but otherwise fallback to
		// this switch where we handle them manually.
		if (not type_remap_trivial(lhs, rhs, node)) {
			switch (node->kind) {
				// Literals
				case SymbolKind::Number: return TypeKind::Scalar;
				case SymbolKind::String: return TypeKind::String;

				case SymbolKind::Beat:
				case SymbolKind::Rest: return TypeKind::Rhythm;

				// Assignment
				case SymbolKind::Identifier:
				case SymbolKind::Assign: {
					CANE_UNIMPLEMENTED();
				} break;

				// TODO: Fix this, not sure if this is actually correct in all
				// cases.
				case SymbolKind::Function: {
					return node->rhs->type;
				} break;

				case SymbolKind::Call: {
					TypeKind function =
						pass_semantic_analysis_walker(node->lhs);

					TypeKind argument =
						pass_semantic_analysis_walker(node->rhs);

					if (function != argument) {
						cane::report(
							ReportKind::Type,
							"mismatched type `{}`!",
							node->kind
						);
					}

					return function;
				} break;

				// Statements always return the type of their last expression.
				// TODO: We might be able to implement this as a trivial type
				// remapping once we have proper support for "patterns" since a
				// cane program should evaluate to a fully mapped list of
				// events.
				case SymbolKind::Statement: {
					pass_semantic_analysis_walker(node->lhs);

					TypeKind trailing =
						pass_semantic_analysis_walker(node->rhs);

					// we return the last expression's type.
					node->type = trailing;
					return trailing;
				} break;

				default: {
					cane::report(
						ReportKind::Type,
						"no mapping for `{}` {} `{}`!",
						lhs,
						node->kind,
						rhs
					);
				} break;
			}
		}

		return node->type;
	}

	///////////////
	// Evaluator //
	///////////////

	struct Environment {
		size_t bpm;
	};

	inline Value pass_evaluator_walker(
		Environment& env, std::mt19937_64& rng, std::shared_ptr<Node> node
	);

	inline Value pass_evaluator(Environment env, std::shared_ptr<Node> node) {
		CANE_FUNC();

		std::random_device rd;
		std::mt19937_64 rng(rd());

		return pass_evaluator_walker(env, rng, node);
	}

	inline Value pass_evaluator_walker(
		Environment& env, std::mt19937_64& rng, std::shared_ptr<Node> node
	) {
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
						"unable to parse integer `{}`!",
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

			case SymbolKind::Statement: {
				pass_evaluator_walker(env, rng, node->lhs);
				return pass_evaluator_walker(env, rng, node->rhs);
			} break;

			default: break;
		}

		Value lhs = pass_evaluator_walker(env, rng, node->lhs);
		Value rhs = pass_evaluator_walker(env, rng, node->rhs);

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

			case SymbolKind::RandomScalarScalar: return lhs.choice(rng, rhs);

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
				return lhs.map_onto_rhythm(env.bpm, rhs);
			case SymbolKind::MapMelodyRhythm:
				return lhs.map_onto_melody(env.bpm, rhs);

			// Time divisions
			case SymbolKind::MulSequenceScalar: return lhs.timemul(rhs);
			case SymbolKind::DivSequenceScalar: return lhs.timediv(rhs);

			// Patterns
			case SymbolKind::SendSequenceString: return lhs.send(rhs);

			default: {
				cane::report(
					ReportKind::Eval, "unable to evaluate `{}`!", node->op
				);
			} break;
		}

		CANE_UNREACHABLE();
	}

}  // namespace cane

#endif
