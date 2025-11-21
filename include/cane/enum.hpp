#ifndef CANE_ENUM_HPP
#define CANE_ENUM_HPP

#include <array>
#include <ostream>
#include <sstream>

namespace cane {

	///////////
	// Opfix //
	///////////

#define CANE_OPFIX_KINDS \
	X(None, "none") \
\
	X(Primary, "primary") \
	X(Literal, "literal") \
\
	X(Prefix, "prefix") \
	X(Infix, "infix") \
	X(Postfix, "postfix")

#define X(x, y) x,
	enum class OpfixKind {
		CANE_OPFIX_KINDS Total
	};
#undef X

	// Maps the enum const direct to a string
#define X(x, y) #x,
	inline std::array OPFIX_KIND_TO_STR = { CANE_OPFIX_KINDS };
#undef X

	// Map the enum const to a human readable string
#define X(x, y) y,
	inline std::array OPFIX_KIND_TO_STR_HUMAN = { CANE_OPFIX_KINDS };
#undef X

	constexpr std::string_view opfix_kind_to_str(OpfixKind x) {
		return OPFIX_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view opfix_kind_to_str_human(OpfixKind x) {
		return OPFIX_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, OpfixKind log) {
		return (os << opfix_kind_to_str_human(log));
	}

	////////////////
	// Precedence //
	////////////////

#define CANE_PRECEDENCE_KINDS \
	X(None, "none", 0) \
\
	X(Last, "last", 1) \
	X(Incr, "incr", 1)

#define X(x, y, z) x = z,
	enum class PrecedenceKind {
		CANE_PRECEDENCE_KINDS Total
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
	X(None, "none", 1) \
\
	X(Left, "left", 1) \
	X(Right, "right", 0)

#define X(x, y, z) x = z,
	enum class AssociativityKind {
		CANE_ASSOCIATIVITY_KINDS Total
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
	// Symbols //
	/////////////

	// Fields: Name, String, Precedence, Associativity

#define CANE_SYMBOL_KINDS \
	/* Misc. */ \
	X(None, "none", None, None, None) \
	X(EndFile, "end of file", None, None, None) \
	X(Whitespace, "whitespace", None, None, None) \
	X(Comment, "comment", None, None, None) \
\
	X(Statement, "statement", None, None, None) \
	X(Arrow, "->", None, None, None) \
	X(Semicolon, ";", None, None, None) \
\
	X(Number, "number", Literal, None, None) \
	X(String, "string", Literal, None, None) \
	X(Beat, "!", Literal, None, None) \
	X(Rest, ".", Literal, None, None) \
\
	X(Function, "function", Primary, None, None) \
	X(Identifier, "identifier", Primary, None, None) \
\
	X(LeftParen, "(", Primary, None, None) \
	X(RightParen, ")", None, None, None) \
	X(LeftBrace, "{", None, None, None) \
	X(RightBrace, "}", None, None, None) \
	X(LeftBracket, "[", None, None, None) \
	X(RightBracket, "]", None, None, None) \
\
	/* Operators */ \
	X(Send, "~>", Infix, Incr, Left) \
	X(Map, "@", Infix, Incr, Left) \
\
	X(Concatenate, ",", Infix, Incr, Left) \
	X(Layer, "$", Infix, Last, Left) \
\
	X(Call, "call", Infix, Incr, Left) \
\
	X(Assign, "=>", Infix, Incr, Left) \
\
	X(Or, "or", Infix, Incr, Left) \
	X(Xor, "xor", Infix, Last, Left) \
	X(And, "and", Infix, Last, Left) \
\
	X(Repeat, "**", Infix, Incr, Left) \
\
	X(LeftShift, "<<", Infix, Incr, Left) \
	X(RightShift, ">>", Infix, Last, Left) \
\
	X(Invert, "~", Prefix, Incr, Left) \
	X(Reverse, "'", Prefix, Last, Left) \
\
	X(Add, "+", Infix, Incr, Left) \
	X(Sub, "-", Infix, Last, Left) \
\
	X(Mul, "*", Infix, Incr, Left) \
	X(Div, "/", Infix, Last, Left) \
\
	X(Euclidean, ":", Infix, Incr, Left) \
\
	X(LCM, "lcm", Infix, Incr, Left) \
	X(GCD, "gcd", Infix, Last, Left) \
\
	X(Random, "?", Infix, Incr, Left) \
\
	X(Abs, "+", Prefix, Incr, Right) \
	X(Neg, "-", Prefix, Last, Right) \
\
	X(Incr, "++", Postfix, Incr, Left) \
	X(Decr, "--", Postfix, Last, Left) \
\
	X(Coerce, "&", Prefix, Incr, Right) \
\
	/* Type Annotations */ \
	X(AnnotationNumber, "number type", None, None, None) \
	X(AnnotationString, "string type", None, None, None) \
	X(AnnotationRhythm, "rhythm type", None, None, None) \
	X(AnnotationMelody, "melody type", None, None, None) \
	X(AnnotationSequence, "sequence type", None, None, None) \
	X(AnnotationPattern, "pattern type", None, None, None) \
\
	/* === Type Specific Symbols (Assigned during typechecking) === */ \
\
	/* PREFIX/UNARY */ \
	X(AbsScalar, "scalar abs", None, None, None) \
	X(NegScalar, "scalar neg", None, None, None) \
\
	X(InvertRhythm, "rhythm invert", None, None, None) \
	X(ReverseRhythm, "rhythm reverse", None, None, None) \
\
	X(ReverseMelody, "melody reverse", None, None, None) \
\
	X(IncrScalar, "scalar incr", None, None, None) \
	X(DecrScalar, "scalar decr", None, None, None) \
\
	/* SCALAR */ \
	X(AddScalarScalar, "scalar add", None, None, None) \
	X(SubScalarScalar, "scalar sub", None, None, None) \
	X(MulScalarScalar, "scalar mul", None, None, None) \
	X(DivScalarScalar, "scalar div", None, None, None) \
\
	X(LeftShiftScalarScalar, "scalar lshift", None, None, None) \
	X(RightShiftScalarScalar, "scalar rshift", None, None, None) \
\
	X(LCMScalarScalar, "scalar lcm", None, None, None) \
	X(GCDScalarScalar, "scalar gcd", None, None, None) \
\
	X(EuclideanScalarScalar, "scalar euclidean", None, None, None) \
	X(ConcatenateScalarScalar, "scalar concatenate", None, None, None) \
\
	X(RandomScalarScalar, "scalar random", None, None, None) \
\
	/* MELODY */ \
	X(CoerceScalar, "coerce scalar", None, None, None) \
	X(CoerceMelody, "coerce melody", None, None, None) \
	X(MapMelodyRhythm, "melody map", None, None, None) \
\
	X(LeftShiftMelodyScalar, "melody lshift", None, None, None) \
	X(RightShiftMelodyScalar, "melody rshift", None, None, None) \
\
	X(AddMelodyScalar, "melody add", None, None, None) \
	X(SubMelodyScalar, "melody sub", None, None, None) \
	X(MulMelodyScalar, "melody mul", None, None, None) \
	X(DivMelodyScalar, "melody div", None, None, None) \
\
	X(RepeatMelodyScalar, "melody repeat", None, None, None) \
\
	X(ConcatenateMelodyMelody, "melody concatenate", None, None, None) \
	X(ConcatenateMelodyScalar, "melody concatenate", None, None, None) \
	X(ConcatenateScalarMelody, "melody concatenate", None, None, None) \
\
	/* RHYTHM */ \
	X(MapRhythmMelody, "rhythm map", None, None, None) \
\
	X(LeftShiftRhythmScalar, "rhythm lshift", None, None, None) \
	X(RightShiftRhythmScalar, "rhythm rshift", None, None, None) \
\
	X(RepeatRhythmScalar, "rhythm repeat", None, None, None) \
	X(ConcatenateRhythmRhythm, "rhythm concatenate", None, None, None) \
\
	X(OrRhythmRhythm, "rhythm or", None, None, None) \
	X(XorRhythmRhythm, "rhythm xor", None, None, None) \
	X(AndRhythmRhythm, "rhythm and", None, None, None) \
\
	/* SEQUENCE */ \
	X(ConcatenateSequenceSequence, "sequence concatenate", None, None, None) \
\
	X(MulSequenceScalar, "sequence mul", None, None, None) \
	X(DivSequenceScalar, "sequence div", None, None, None) \
\
	/* PATTERN */ \
	X(SendSequenceString, "sequence send", None, None, None)

#define X(a, b, c, d, e) a,
	enum class SymbolKind {
		CANE_SYMBOL_KINDS Total
	};
#undef X

// Maps the enum const direct to a string
#define X(a, b, c, d, e) #a,
	inline std::array SYMBOL_KIND_TO_STR = { CANE_SYMBOL_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(a, b, c, d, e) b,
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
#define X(symbol, str, opfix, prec, ass) \
	handle_op(SymbolKind::symbol, PrecedenceKind::prec, AssociativityKind::ass);

		// Generates a mapping from symbol to binding power which determines
		// operator precedence and associativity.
		constexpr decltype(auto) generate_binding_power_table() {
			std::array<
				std::pair<size_t, size_t>,
				static_cast<size_t>(SymbolKind::Total)>
				table;

			size_t current_precedence_level = 0;

			auto handle_op = [&](SymbolKind sym,
								 PrecedenceKind prec,
								 AssociativityKind ass) {
				current_precedence_level += static_cast<size_t>(prec);

				table.at(static_cast<size_t>(sym)) = {
					current_precedence_level,
					current_precedence_level + static_cast<size_t>(ass),
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

	/////////////////
	// Classifiers //
	/////////////////

	// is_literal
	constexpr bool is_literal(SymbolKind kind) {
#define X(symbol, str, opfix, prec, ass) \
	(OpfixKind::opfix == OpfixKind::Literal && kind == SymbolKind::symbol) ||
		bool is = CANE_SYMBOL_KINDS false;
		return is;
#undef X
	}

	// is_primary
	constexpr bool is_primary(SymbolKind kind) {
#define X(symbol, str, opfix, prec, ass) \
	(OpfixKind::opfix == OpfixKind::Primary && kind == SymbolKind::symbol) ||
		bool is = CANE_SYMBOL_KINDS false;
		return is;
#undef X
	}

	// is_prefix
	constexpr bool is_prefix(SymbolKind kind) {
#define X(symbol, str, opfix, prec, ass) \
	(OpfixKind::opfix == OpfixKind::Prefix && kind == SymbolKind::symbol) ||
		bool is = CANE_SYMBOL_KINDS false;
		return is;
#undef X
	}

	// is_infix
	constexpr bool is_infix(SymbolKind kind) {
#define X(symbol, str, opfix, prec, ass) \
	(OpfixKind::opfix == OpfixKind::Infix && kind == SymbolKind::symbol) ||
		bool is = CANE_SYMBOL_KINDS false;
		return is;
#undef X
	}

	// is_postfix
	constexpr bool is_postfix(SymbolKind kind) {
#define X(symbol, str, opfix, prec, ass) \
	(OpfixKind::opfix == OpfixKind::Postfix && kind == SymbolKind::symbol) ||
		bool is = CANE_SYMBOL_KINDS false;
		return is;
#undef X
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
		CANE_TYPE_KINDS Total
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
		CANE_EVENT_KINDS Total
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
		CANE_REPORT_KINDS Total
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

CANE_FORMATTER_DEF(cane::OpfixKind);
CANE_FORMATTER_DEF(cane::PrecedenceKind);
CANE_FORMATTER_DEF(cane::AssociativityKind);
CANE_FORMATTER_DEF(cane::SymbolKind);
CANE_FORMATTER_DEF(cane::TypeKind);
CANE_FORMATTER_DEF(cane::EventKind);
CANE_FORMATTER_DEF(cane::ReportKind);

#endif
