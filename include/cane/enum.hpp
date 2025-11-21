#ifndef CANE_ENUM_HPP
#define CANE_ENUM_HPP

#include <array>
#include <ostream>
#include <sstream>

namespace cane {

	////////////////
	// Precedence //
	////////////////

#define CANE_PRECEDENCE_KINDS \
	X(None, "None", 0) \
	X(Last, "Last", 0) \
	X(Incr, "Incr", 1)

#define X(x, y, z) x = z,
	enum class PrecedenceKind {
		CANE_PRECEDENCE_KINDS CANE_PRECEDENCE_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y, z) #x,
	inline std::array PRECEDENCE_KIND_TO_STR = { CANE_PRECEDENCE_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z) y,
	inline std::array PRECEDENCE_KIND_TO_STR_HUMAN = { CANE_PRECEDENCE_KINDS };
#undef X

	constexpr std::string_view precedence_kind_to_str(PrecedenceKind p) {
		return PRECEDENCE_KIND_TO_STR[static_cast<size_t>(p)];
	}

	constexpr std::string_view precedence_kind_to_str_human(PrecedenceKind p) {
		return PRECEDENCE_KIND_TO_STR_HUMAN[static_cast<size_t>(p)];
	}

	inline std::ostream& operator<<(std::ostream& os, PrecedenceKind p) {
		return (os << precedence_kind_to_str_human(p));
	}

	///////////////////
	// Associativity //
	///////////////////

#define CANE_ASSOCIATIVITY_KINDS \
	X(None, "None", 0) \
	X(Left, "Left", 1) \
	X(Right, "Right", 0)

#define X(x, y, z) x = z,
	enum class AssociativityKind {
		CANE_ASSOCIATIVITY_KINDS CANE_ASSOCIATIVITY_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y, z) #x,
	inline std::array ASSOCIATIVITY_KIND_TO_STR = { CANE_ASSOCIATIVITY_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z) y,
	inline std::array ASSOCIATIVITY_KIND_TO_STR_HUMAN = {
		CANE_ASSOCIATIVITY_KINDS
	};
#undef X

	constexpr std::string_view precedence_kind_to_str(AssociativityKind p) {
		return ASSOCIATIVITY_KIND_TO_STR[static_cast<size_t>(p)];
	}

	constexpr std::string_view
	precedence_kind_to_str_human(AssociativityKind p) {
		return ASSOCIATIVITY_KIND_TO_STR_HUMAN[static_cast<size_t>(p)];
	}

	inline std::ostream& operator<<(std::ostream& os, AssociativityKind p) {
		return (os << precedence_kind_to_str_human(p));
	}

	/////////////
	// Reports //
	/////////////

#define CANE_REPORT_KINDS \
	X(Generic, "generic") \
	X(Lexical, "lexical") \
	X(Syntax, "syntax") \
	X(Semantic, "semantic") \
	X(Type, "type") \
	X(Eval, "eval")

#define X(x, y) x,
	enum class ReportKind {
		CANE_REPORT_KINDS CANE_REPORT_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) #x,
	inline std::array REPORT_KIND_TO_STR = { CANE_REPORT_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) y,
	inline std::array REPORT_KIND_TO_STR_HUMAN = { CANE_REPORT_KINDS };
#undef X

	constexpr std::string_view report_kind_to_str(ReportKind x) {
		return REPORT_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view report_kind_to_str_human(ReportKind x) {
		return REPORT_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, ReportKind log) {
		return (os << report_kind_to_str_human(log));
	}

	/////////////
	// Symbols //
	/////////////

	// Fields: Name, String, Precedence, Associativity

#define CANE_SYMBOL_KINDS \
	/* Misc. */ \
	X(None, "none", None, None) \
	X(EndFile, "end of file", None, None) \
	X(Whitespace, "whitespace", None, None) \
	X(Comment, "comment", None, None) \
\
	X(Statement, "statement", None, None) \
	X(Function, "function", None, None) \
	X(Arrow, "->", None, None) \
	X(Semicolon, ";", None, None) \
\
	X(Number, "number", None, None) \
	X(String, "string", None, None) \
	X(Identifier, "identifier", None, None) \
	X(Beat, "!", None, None) \
	X(Rest, ".", None, None) \
\
	X(LeftParen, "(", None, None) \
	X(RightParen, ")", None, None) \
	X(LeftBrace, "{", None, None) \
	X(RightBrace, "}", None, None) \
	X(LeftBracket, "[", None, None) \
	X(RightBracket, "]", None, None) \
\
	/* Operators */ \
	X(Send, "~>", Incr, Left) \
	X(Map, "@", Incr, Left) \
\
	X(Concatenate, ",", Incr, Left) \
	X(Layer, "$", Last, Left) \
\
	X(Call, "call", Incr, Left) \
\
	X(Assign, "=>", Incr, Left) \
\
	X(Or, "or", Incr, Left) \
	X(Xor, "xor", Last, Left) \
	X(And, "and", Last, Left) \
\
	X(Repeat, "**", Incr, Left) \
\
	X(LeftShift, "<<", Incr, Left) \
	X(RightShift, ">>", Last, Left) \
\
	X(Invert, "~", Incr, Left) \
	X(Reverse, "'", Last, Left) \
\
	X(Add, "+", Incr, Left) \
	X(Sub, "-", Last, Left) \
\
	X(Mul, "*", Incr, Left) \
	X(Div, "/", Last, Left) \
\
	X(Euclidean, ":", Incr, Left) \
\
	X(LCM, "lcm", Incr, Left) \
	X(GCD, "gcd", Last, Left) \
\
	X(Random, "?", Incr, Left) \
\
	X(Abs, "+", Incr, Right) \
	X(Neg, "-", Last, Right) \
\
	X(Incr, "++", Incr, Left) \
	X(Decr, "--", Last, Left) \
\
	X(Coerce, "&", Incr, Right) \
\
	/* Type Annotations */ \
	X(AnnotationNumber, "number type", None, None) \
	X(AnnotationString, "string type", None, None) \
	X(AnnotationRhythm, "rhythm type", None, None) \
	X(AnnotationMelody, "melody type", None, None) \
	X(AnnotationSequence, "sequence type", None, None) \
	X(AnnotationPattern, "pattern type", None, None) \
\
	/* === Type Specific Symbols (Assigned during typechecking) === */ \
\
	/* PREFIX/UNARY */ \
	X(AbsScalar, "scalar abs", None, None) \
	X(NegScalar, "scalar neg", None, None) \
\
	X(InvertRhythm, "rhythm invert", None, None) \
	X(ReverseRhythm, "rhythm reverse", None, None) \
\
	X(ReverseMelody, "melody reverse", None, None) \
\
	X(IncrScalar, "scalar incr", None, None) \
	X(DecrScalar, "scalar decr", None, None) \
\
	/* SCALAR */ \
	X(AddScalarScalar, "scalar add", None, None) \
	X(SubScalarScalar, "scalar sub", None, None) \
	X(MulScalarScalar, "scalar mul", None, None) \
	X(DivScalarScalar, "scalar div", None, None) \
\
	X(LeftShiftScalarScalar, "scalar lshift", None, None) \
	X(RightShiftScalarScalar, "scalar rshift", None, None) \
\
	X(LCMScalarScalar, "scalar lcm", None, None) \
	X(GCDScalarScalar, "scalar gcd", None, None) \
\
	X(EuclideanScalarScalar, "scalar euclidean", None, None) \
	X(ConcatenateScalarScalar, "scalar concatenate", None, None) \
\
	X(RandomScalarScalar, "scalar random", None, None) \
\
	/* MELODY */ \
	X(CoerceScalar, "coerce scalar", None, None) \
	X(CoerceMelody, "coerce melody", None, None) \
	X(MapMelodyRhythm, "melody map", None, None) \
\
	X(LeftShiftMelodyScalar, "melody lshift", None, None) \
	X(RightShiftMelodyScalar, "melody rshift", None, None) \
\
	X(AddMelodyScalar, "melody add", None, None) \
	X(SubMelodyScalar, "melody sub", None, None) \
	X(MulMelodyScalar, "melody mul", None, None) \
	X(DivMelodyScalar, "melody div", None, None) \
\
	X(RepeatMelodyScalar, "melody repeat", None, None) \
\
	X(ConcatenateMelodyMelody, "melody concatenate", None, None) \
	X(ConcatenateMelodyScalar, "melody concatenate", None, None) \
	X(ConcatenateScalarMelody, "melody concatenate", None, None) \
\
	/* RHYTHM */ \
	X(MapRhythmMelody, "rhythm map", None, None) \
\
	X(LeftShiftRhythmScalar, "rhythm lshift", None, None) \
	X(RightShiftRhythmScalar, "rhythm rshift", None, None) \
\
	X(RepeatRhythmScalar, "rhythm repeat", None, None) \
	X(ConcatenateRhythmRhythm, "rhythm concatenate", None, None) \
\
	X(OrRhythmRhythm, "rhythm or", None, None) \
	X(XorRhythmRhythm, "rhythm xor", None, None) \
	X(AndRhythmRhythm, "rhythm and", None, None) \
\
	/* SEQUENCE */ \
	X(ConcatenateSequenceSequence, "sequence concatenate", None, None) \
\
	X(MulSequenceScalar, "sequence mul", None, None) \
	X(DivSequenceScalar, "sequence div", None, None) \
\
	/* PATTERN */ \
	X(SendSequenceString, "sequence send", None, None)

#define X(x, y, z, w) x,
	enum class SymbolKind {
		CANE_SYMBOL_KINDS CANE_SYMBOL_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y, z, w) #x,
	inline std::array SYMBOL_KIND_TO_STR = { CANE_SYMBOL_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z, w) y,
	inline std::array SYMBOL_KIND_TO_STR_HUMAN = { CANE_SYMBOL_KINDS };
#undef X

	constexpr std::string_view symbol_kind_to_str(SymbolKind x) {
		return SYMBOL_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view symbol_kind_to_str_human(SymbolKind x) {
		return SYMBOL_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, SymbolKind log) {
		return (os << symbol_kind_to_str_human(log));
	}

	namespace detail {
#define X(symbol, str, prec, ass) \
	handle_op(SymbolKind::symbol, PrecedenceKind::prec, AssociativityKind::ass);

		constexpr decltype(auto) generate_binding_power_table() {
			std::array<
				std::pair<size_t, size_t>,
				static_cast<size_t>(SymbolKind::CANE_SYMBOL_TOTAL)>
				table;
			size_t current_precedence_level = 0;

			auto handle_op = [&](SymbolKind sym,
								 PrecedenceKind prec,
								 AssociativityKind ass) {
				current_precedence_level += static_cast<size_t>(prec);

				size_t op_prec =
					prec == PrecedenceKind::None ? 0 : current_precedence_level;

				size_t op_ass = ass == AssociativityKind::None ?
					0 :
					static_cast<size_t>(ass);

				table.at(static_cast<size_t>(sym)) = {
					op_prec,
					op_prec + op_ass,
				};
			};

			CANE_SYMBOL_KINDS

			return table;
		}

#undef X
	}  // namespace detail

	constexpr std::pair<size_t, size_t> binding_power(SymbolKind kind) {
		constexpr auto table = detail::generate_binding_power_table();
		return table.at(static_cast<size_t>(kind));
	}

	///////////
	// Types //
	///////////

#define CANE_TYPE_KINDS \
	X(None, "none") \
\
	X(Scalar, "scalar") \
	X(String, "string") \
\
	X(Melody, "melody") \
	X(Rhythm, "rhythm") \
	X(Sequence, "sequence") \
	X(Pattern, "pattern") \
	X(Function, "function")

#define X(x, y) x,
	enum class TypeKind {
		CANE_TYPE_KINDS CANE_TYPE_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) #x,
	inline std::array TYPE_KIND_TO_STR = { CANE_TYPE_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) y,
	inline std::array TYPE_KIND_TO_STR_HUMAN = { CANE_TYPE_KINDS };
#undef X

	constexpr std::string_view type_kind_to_str(TypeKind x) {
		return TYPE_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view type_kind_to_str_human(TypeKind x) {
		return TYPE_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, TypeKind log) {
		return (os << type_kind_to_str_human(log));
	}

	////////////
	// Events //
	////////////

#define CANE_EVENT_KINDS \
	X(Beat, "beat") \
	X(Rest, "rest")

#define X(x, y) x,
	enum class EventKind {
		CANE_EVENT_KINDS CANE_EVENT_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) #x,
	inline std::array EVENT_KIND_TO_STR = { CANE_EVENT_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) y,
	inline std::array EVENT_KIND_TO_STR_HUMAN = { CANE_EVENT_KINDS };
#undef X

	constexpr std::string_view event_kind_to_str(EventKind x) {
		return EVENT_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view event_kind_to_str_human(EventKind x) {
		return EVENT_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, EventKind ev) {
		return (os << event_kind_to_str_human(ev));
	}

}  // namespace cane

#define CANE_FORMATTER_DEF(type) \
	template <> \
	struct std::formatter<type>: std::formatter<std::string_view> { \
		auto format(type x, format_context& ctx) const { \
			std::ostringstream ss; \
			ss << x; \
			return std::formatter<string_view>::format(ss.str(), ctx); \
		} \
	};

CANE_FORMATTER_DEF(cane::PrecedenceKind);
CANE_FORMATTER_DEF(cane::AssociativityKind);
CANE_FORMATTER_DEF(cane::ReportKind);
CANE_FORMATTER_DEF(cane::SymbolKind);
CANE_FORMATTER_DEF(cane::TypeKind);
CANE_FORMATTER_DEF(cane::EventKind);

#endif
