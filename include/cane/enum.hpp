#ifndef CANE_ENUM_HPP
#define CANE_ENUM_HPP

#include <array>
#include <ostream>

namespace cane {

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

#define CANE_SYMBOL_KINDS \
	X(None, "none") \
\
	/* Misc. */ \
	X(EndFile, "end of file") \
	X(Whitespace, "whitespace") \
	X(Comment, "comment") \
\
	/* Raw Symbols (later converted to operator symbols) */ \
	X(Ampersand, "ampersand `&`") \
	X(Backslash, "backslash `\\`") \
	X(Backtick, "backtick ```") \
	X(Comma, "comma `,`") \
	X(Dot, "dot `.`") \
	X(Exclaim, "exclaim `!`") \
	X(Colon, "colon `:`") \
	X(Semicolon, "semicolon `;`") \
	X(Quote, "quote `'`") \
	X(At, "at `@`") \
	X(Stars, "stars `**`") \
	X(Tilda, "tilda `~`") \
	X(Question, "question `?`") \
\
	X(Fatarrow, "fat arrow `=>`") \
	X(Wigglearrow, "wiggle arrow `~>`") \
	X(Arrow, "arrow `->`") \
\
	/* AST */ \
	X(Coerce, "coerce") \
	X(Concatenate, "concatenate") \
	X(Call, "call") \
	X(Assign, "assign") \
	X(Send, "send") \
	X(Function, "function") \
	X(Repeat, "repeat") \
	X(Map, "map") \
	X(Invert, "invert") \
	X(Reverse, "reverse") \
	X(Euclidean, "euclidean") \
\
	X(Abs, "abs") \
	X(Neg, "neg") \
\
	X(LeftShift, "lshift") \
	X(RightShift, "rshift") \
\
	X(LeftChevron, "lchevron `<`") \
	X(RightChevron, "rchevron `>`") \
\
	/* Operators */ \
	X(LCM, "lcm") \
	X(GCD, "gcd") \
\
	X(Random, "random") \
\
	X(Or, "or") \
	X(Xor, "xor") \
	X(And, "and") \
\
	X(Add, "add `+`") \
	X(Sub, "sub `-`") \
	X(Mul, "mul `*`") \
	X(Div, "div `/`") \
\
	/* Grouping */ \
	X(LeftParen, "lparen `(`") \
	X(RightParen, "rparen `)`") \
	X(LeftBrace, "lbrace `{`") \
	X(RightBrace, "rbrace `}`") \
	X(LeftBracket, "lbracket `[`") \
	X(RightBracket, "rbracket `]`") \
\
	/* Cons Lists */ \
	X(Statement, "statement") \
	X(Layer, "layer") \
\
	/* Atoms */ \
	X(Number, "number") \
	X(String, "string") \
	X(Identifier, "identifier") \
	X(Rhythm, "rhythm") \
	X(Melody, "melody") \
	X(Beat, "beat") \
	X(Rest, "rest") \
\
	/* Annotations */ \
	X(AnnotationNumber, "number (type)") \
	X(AnnotationString, "string (type)") \
	X(AnnotationRhythm, "rhythm (type)") \
	X(AnnotationMelody, "melody (type)") \
	X(AnnotationSequence, "sequence (type)") \
	X(AnnotationPattern, "pattern (type)") \
\
	/* === Type Specific Symbols (Assigned during typechecking) === */ \
\
	/* PREFIX */ \
	X(AbsScalar, "scalar abs") \
	X(NegScalar, "scalar neg") \
\
	X(InvertRhythm, "rhythm invert") \
	X(ReverseRhythm, "rhythm reverse") \
\
	X(ReverseMelody, "melody reverse") \
\
	/* SCALAR */ \
	X(AddScalarScalar, "scalar add") \
	X(SubScalarScalar, "scalar sub") \
	X(MulScalarScalar, "scalar mul") \
	X(DivScalarScalar, "scalar div") \
\
	X(LshiftScalarScalar, "scalar lshift") \
	X(RshiftScalarScalar, "scalar rshift") \
\
	X(LcmScalarScalar, "scalar lcm") \
	X(GcdScalarScalar, "scalar gcd") \
\
	X(EuclideanScalarScalar, "scalar euclidean") \
	X(ConcatenateScalarScalar, "scalar concatenate") \
\
	X(RandomScalarScalar, "scalar random") \
\
	/* MELODY */ \
	X(MapMelodyRhythm, "melody map") \
\
	X(LshiftMelodyScalar, "melody lshift") \
	X(RshiftMelodyScalar, "melody rshift") \
\
	X(AddMelodyScalar, "melody add") \
	X(SubMelodyScalar, "melody sub") \
	X(MulMelodyScalar, "melody mul") \
	X(DivMelodyScalar, "melody div") \
\
	X(RepeatMelodyScalar, "melody repeat") \
	X(ConcatenateMelodyMelody, "melody concatenate") \
\
	/* RHYTHM */ \
	X(MapRhythmMelody, "rhythm map") \
\
	X(LshiftRhythmScalar, "rhythm lshift") \
	X(RshiftRhythmScalar, "rhythm rshift") \
\
	X(RepeatRhythmScalar, "rhythm repeat") \
	X(ConcatenateRhythmRhythm, "rhythm concatenate") \
\
	X(OrRhythmRhythm, "rhythm or") \
	X(XorRhythmRhythm, "rhythm xor") \
	X(AndRhythmRhythm, "rhythm and") \
\
	/* SEQUENCE */ \
	X(ConcatenateSequenceSequence, "sequence concatenate") \
\
	X(MulSequenceScalar, "sequence mul") \
	X(DivSequenceScalar, "sequence div")

#define X(x, y) x,
	enum class SymbolKind {
		CANE_SYMBOL_KINDS CANE_SYMBOL_TOTAL
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) #x,
	inline std::array SYMBOL_KIND_TO_STR = { CANE_SYMBOL_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) y,
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

	///////////
	// Types //
	///////////

#define CANE_TYPE_KINDS \
	X(TypeNone, "none") \
\
	X(TypeScalar, "scalar") \
	X(TypeString, "string") \
\
	X(TypeMelody, "melody") \
	X(TypeRhythm, "rhythm") \
	X(TypeSequence, "sequence") \
	X(TypePattern, "pattern") \
	X(TypeFunction, "function")

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

	///////////
	// Opfix //
	///////////

#define CANE_OPFIX_KINDS \
	X(OpfixPrefix, "prefix") \
	X(OpfixInfix, "infix") \
	X(OpfixPostfix, "postfix") \
\
	/* For use with AST passes since prefix/postfix have the same \
	 * representation. */ \
	X(OpfixUnary, "unary") \
	X(OpfixBinary, "binary")

#define X(x, y) x,
	enum class OpfixKind {
		CANE_OPFIX_KINDS CANE_OPFIX_TOTAL
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

}  // namespace cane

#endif
