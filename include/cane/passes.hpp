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
	[[nodiscard]] inline std::tuple<bool, TypeKind, SymbolKind>
	type_remap_trivial(
		[[maybe_unused]] Configuration cfg,
		TypeKind lhs,
		TypeKind rhs,
		std::shared_ptr<Node> node
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

	///////////////
	// Evaluator //
	///////////////

	[[nodiscard]] inline Value
	pass_evaluator_walk(Configuration cfg, std::shared_ptr<Node> node) {
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
				CANE_UNUSED(pass_evaluator_walk(cfg, node->lhs));
				return pass_evaluator_walk(cfg, node->rhs);
			} break;

			default: break;
		}

		Value lhs = pass_evaluator_walk(cfg, node->lhs);
		Value rhs = pass_evaluator_walk(cfg, node->rhs);

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
