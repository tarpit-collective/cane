#ifndef CANE_ENUM_HPP
#define CANE_ENUM_HPP

#include <array>
#include <optional>

#include <ostream>
#include <sstream>

// Mark as unused.
#define CANE_IMPL_UNUSED0()
#define CANE_IMPL_UNUSED1(a)          (void)(a)
#define CANE_IMPL_UNUSED2(a, b)       (void)(a), CANE_IMPL_UNUSED1(b)
#define CANE_IMPL_UNUSED3(a, b, c)    (void)(a), CANE_IMPL_UNUSED2(b, c)
#define CANE_IMPL_UNUSED4(a, b, c, d) (void)(a), CANE_IMPL_UNUSED3(b, c, d)
#define CANE_IMPL_UNUSED5(a, b, c, d, e) \
	(void)(a), CANE_IMPL_UNUSED4(b, c, d, e)

#define CANE_VA_NUM_ARGS_IMPL(_0, _1, _2, _3, _4, _5, N, ...) N
#define CANE_VA_NUM_ARGS(...) \
	CANE_VA_NUM_ARGS_IMPL(100, ##__VA_ARGS__, 5, 4, 3, 2, 1, 0)

#define CANE_UNUSED_IMPL_(nargs) CANE_IMPL_UNUSED##nargs
#define CANE_UNUSED_IMPL(nargs)  CANE_UNUSED_IMPL_(nargs)
#define CANE_UNUSED(...) \
	CANE_UNUSED_IMPL(CANE_VA_NUM_ARGS(__VA_ARGS__))(__VA_ARGS__)

// String macros
#define CANE_CSTR( \
	s \
) /* Convert c-string into a string_view before decaying to pointer. */ \
	std::string_view { \
		s, ((const char*)s) + (sizeof(s) - 1) \
	}

#define CANE_STR_IMPL_(x) #x
#define CANE_STR(x)       CANE_STR_IMPL_(x)

#define CANE_CAT_IMPL_(x, y) x##y
#define CANE_CAT(x, y)       CANE_CAT_IMPL_(x, y)

#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

// Location info
#define CANE_LINEINFO "[" __FILE__ ":" CANE_STR(__LINE__) "]"

#define CANE_LOCATION_FILE __FILE__
#define CANE_LOCATION_LINE CANE_STR(__LINE__)
#define CANE_LOCATION_FUNC __func__

// Utils
#define CANE_MAX(a, b) ((a > b) ? a : b)
#define CANE_MIN(a, b) ((a < b) ? a : b)

namespace cane {

	///////////
	// Opfix //
	///////////

#define CANE_OPFIX_KINDS \
	X(None, "none") \
\
	X(Operator, "operator") \
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
#define X(x, y) CANE_CSTR(#x),
	inline std::array OPFIX_KIND_TO_STR = { CANE_OPFIX_KINDS };
#undef X

	// Map the enum const to a human readable string
#define X(x, y) CANE_CSTR(y),
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
#define X(x, y, z) CANE_CSTR(#x),
	inline std::array PRECEDENCE_KIND_TO_STR = { CANE_PRECEDENCE_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z) CANE_CSTR(y),
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
#define X(x, y, z) CANE_CSTR(#x),
	inline std::array ASSOCIATIVITY_KIND_TO_STR = { CANE_ASSOCIATIVITY_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z) CANE_CSTR(y),
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
	X(Number, "number", Literal, None, None) \
	X(String, "string", Literal, None, None) \
\
	X(Beat, "!", Literal, None, None) \
	X(Rest, ".", Literal, None, None) \
\
	X(Identifier, "identifier", Literal, None, None) \
\
	X(LeftParen, "(", Primary, None, None) \
	X(RightParen, ")", None, None, None) \
\
	X(LeftBrace, "{", None, None, None) \
	X(RightBrace, "}", None, None, None) \
\
	X(LeftBracket, "[", None, None, None) \
	X(RightBracket, "]", None, None, None) \
\
	/* Operators */ \
	X(Block, ";", Infix, Incr, Right) \
	X(Send, "~>", Infix, Incr, Left) \
\
	X(Let, "let", Primary, Incr, Right) \
	X(Function, "function", Primary, Incr, Right) \
\
	X(Layer, "$", Infix, Last, Left) \
	X(Map, "@", Infix, Incr, Left) \
\
	X(Concatenate, ",", Infix, Incr, Left) \
\
	X(Call, "call", Infix, Incr, Left) \
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
	X(Head, "head", Prefix, Incr, Right) \
	X(Tail, "tail", Prefix, Last, Right) \
\
	X(Abs, "+", Prefix, Incr, Right) \
	X(Neg, "-", Prefix, Last, Right) \
\
	X(Incr, "++", Postfix, Incr, Left) \
	X(Decr, "--", Postfix, Last, Left) \
\
	X(Coerce, "&", Prefix, Incr, Right) \
\
	/* === Type Specific Symbols (Assigned during typechecking) === */ \
\
	/* PREFIX/UNARY */ \
	X(AbsScalar, "scalar abs", Operator, None, None) \
	X(NegScalar, "scalar neg", Operator, None, None) \
\
	X(InvertRhythm, "rhythm invert", Operator, None, None) \
	X(ReverseRhythm, "rhythm reverse", Operator, None, None) \
\
	X(ReverseMelody, "melody reverse", Operator, None, None) \
\
	X(IncrScalar, "scalar incr", Operator, None, None) \
	X(DecrScalar, "scalar decr", Operator, None, None) \
\
	/* SCALAR */ \
	X(AddScalarScalar, "scalar add", Operator, None, None) \
	X(SubScalarScalar, "scalar sub", Operator, None, None) \
	X(MulScalarScalar, "scalar mul", Operator, None, None) \
	X(DivScalarScalar, "scalar div", Operator, None, None) \
\
	X(LeftShiftScalarScalar, "scalar lshift", Operator, None, None) \
	X(RightShiftScalarScalar, "scalar rshift", Operator, None, None) \
\
	X(LCMScalarScalar, "scalar lcm", Operator, None, None) \
	X(GCDScalarScalar, "scalar gcd", Operator, None, None) \
\
	X(EuclideanScalarScalar, "scalar euclidean", Operator, None, None) \
	X(ConcatenateScalarScalar, "scalar concatenate", Operator, None, None) \
\
	X(RandomScalarScalar, "scalar random", Operator, None, None) \
\
	/* MELODY */ \
	X(CoerceScalar, "coerce scalar", Operator, None, None) \
	X(CoerceMelody, "coerce melody", Operator, None, None) \
\
	X(MapMelodyRhythm, "melody map", Operator, None, None) \
	X(MapScalarRhythm, "melody map", Operator, None, None) \
\
	X(LeftShiftMelodyScalar, "melody lshift", Operator, None, None) \
	X(RightShiftMelodyScalar, "melody rshift", Operator, None, None) \
\
	X(AddMelodyScalar, "melody add", Operator, None, None) \
	X(SubMelodyScalar, "melody sub", Operator, None, None) \
	X(MulMelodyScalar, "melody mul", Operator, None, None) \
	X(DivMelodyScalar, "melody div", Operator, None, None) \
\
	X(AddMelodyMelody, "melody add", Operator, None, None) \
	X(SubMelodyMelody, "melody sub", Operator, None, None) \
	X(MulMelodyMelody, "melody mul", Operator, None, None) \
	X(DivMelodyMelody, "melody div", Operator, None, None) \
\
	X(RepeatMelodyScalar, "melody repeat", Operator, None, None) \
\
	X(ConcatenateMelodyMelody, "melody concatenate", Operator, None, None) \
	X(ConcatenateMelodyScalar, "melody concatenate", Operator, None, None) \
	X(ConcatenateScalarMelody, "melody concatenate", Operator, None, None) \
\
	X(HeadMelody, "melody head", Operator, None, None) \
	X(TailMelody, "melody tail", Operator, None, None) \
\
	/* RHYTHM */ \
	X(MapRhythmMelody, "rhythm map", Operator, None, None) \
	X(MapRhythmScalar, "rhythm map", Operator, None, None) \
\
	X(LeftShiftRhythmScalar, "rhythm lshift", Operator, None, None) \
	X(RightShiftRhythmScalar, "rhythm rshift", Operator, None, None) \
\
	X(RepeatRhythmScalar, "rhythm repeat", Operator, None, None) \
	X(ConcatenateRhythmRhythm, "rhythm concatenate", Operator, None, None) \
\
	X(OrRhythmRhythm, "rhythm or", Operator, None, None) \
	X(XorRhythmRhythm, "rhythm xor", Operator, None, None) \
	X(AndRhythmRhythm, "rhythm and", Operator, None, None) \
\
	X(HeadRhythm, "rhythm head", Operator, None, None) \
	X(TailRhythm, "rhythm tail", Operator, None, None) \
\
	/* SEQUENCE */ \
	X(ConcatenateSequenceSequence, \
	  "sequence concatenate", \
	  Operator, \
	  None, \
	  None) \
	X(LayerSequenceSequence, "sequence layer", Operator, None, None) \
\
	X(MulSequenceScalar, "sequence mul", Operator, None, None) \
	X(DivSequenceScalar, "sequence div", Operator, None, None) \
\
	X(HeadSequence, "sequence head", Operator, None, None) \
	X(TailSequence, "sequence tail", Operator, None, None) \
\
	X(SendSequenceString, "sequence send", Operator, None, None) \
\
	/* PATTERN */ \
	X(HeadPattern, "pattern head", Operator, None, None) \
	X(TailPattern, "pattern tail", Operator, None, None) \
\
	X(LayerPatternPattern, "pattern layer", Operator, None, None) \
	X(LayerPatternSequence, "pattern layer", Operator, None, None) \
	X(LayerSequencePattern, "pattern layer", Operator, None, None) \
\
	X(ConcatenatePatternPattern, "pattern concatenate", Operator, None, None) \
	X(ConcatenatePatternSequence, "pattern concatenate", Operator, None, None) \
	X(ConcatenateSequencePattern, "pattern concatenate", Operator, None, None) \
\
	X(SendPatternString, "pattern send", Operator, None, None)

#define X(a, b, c, d, e) a,
	enum class SymbolKind {
		CANE_SYMBOL_KINDS Total
	};
#undef X

// Maps the enum const direct to a string
#define X(a, b, c, d, e) CANE_CSTR(#a),
	inline std::array SYMBOL_KIND_TO_STR = { CANE_SYMBOL_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(a, b, c, d, e) CANE_CSTR(b),
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

	struct BindingPower {
		size_t left;
		size_t right;
	};

	namespace detail {
#define X(symbol, str, opfix, prec, ass) \
	handle_op(SymbolKind::symbol, PrecedenceKind::prec, AssociativityKind::ass);

		// Generates a mapping from symbol to binding power which determines
		// operator precedence and associativity.
		constexpr decltype(auto) generate_binding_power_table() {
			std::array<BindingPower, static_cast<size_t>(SymbolKind::Total)>
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

	constexpr std::optional<BindingPower> binding_power(SymbolKind kind) {
		constexpr auto table = detail::generate_binding_power_table();

		if (size_t i = static_cast<size_t>(kind); i < table.size()) {
			return table.at(i);
		}

		return std::nullopt;
	}

	/////////////////
	// Classifiers //
	/////////////////

	constexpr bool is_opfix(OpfixKind opfix, SymbolKind kind) {
#define X(symbol, str, fix, prec, ass) \
	(opfix == OpfixKind::fix && kind == SymbolKind::symbol) ||
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
#define X(x, y) CANE_CSTR(#x),
	inline std::array TYPE_KIND_TO_STR = { CANE_TYPE_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) CANE_CSTR(y),
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
	X(Rest, "rest", 0) \
	X(Beat, "beat", 1)

#define X(x, y, z) x = z,
	enum class EventKind : uint8_t {
		CANE_EVENT_KINDS Total
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y, z) CANE_CSTR(#x),
	inline std::array EVENT_KIND_TO_STR = { CANE_EVENT_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y, z) CANE_CSTR(y),
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
	X(Internal, "internal") \
	X(Generic, "generic") \
	X(Lexical, "lexical") \
	X(Syntactical, "syntax") \
	X(Semantic, "semantic") \
	X(Type, "type") \
	X(Eval, "eval")

#define X(x, y) x,
	enum class ReportKind {
		CANE_REPORT_KINDS Total
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) CANE_CSTR(#x),
	inline std::array REPORT_KIND_TO_STR = { CANE_REPORT_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) CANE_CSTR(y),
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

	//////////////
	// Bindings //
	//////////////

#define CANE_BINDING_KINDS \
	X(Parameter, "parameter") \
	X(Binding, "binding")

#define X(x, y) x,
	enum class BindingKind {
		CANE_BINDING_KINDS Total
	};
#undef X

// Maps the enum const direct to a string
#define X(x, y) CANE_CSTR(#x),
	inline std::array BINDING_KIND_TO_STR = { CANE_BINDING_KINDS };
#undef X

// Map the enum const to a human readable string
#define X(x, y) CANE_CSTR(y),
	inline std::array BINDING_KIND_TO_STR_HUMAN = { CANE_BINDING_KINDS };
#undef X

	constexpr std::string_view binding_kind_to_str(BindingKind x) {
		return BINDING_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view binding_kind_to_str_human(BindingKind x) {
		return BINDING_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, BindingKind log) {
		return (os << binding_kind_to_str_human(log));
	}

	/////////////
	// Logging //
	/////////////

#define CANE_RESET "\x1b[0m"
#define CANE_BOLD  "\x1b[1m"

#define CANE_COLOUR_BLACK   "\x1b[30m"
#define CANE_COLOUR_RED     "\x1b[31m"
#define CANE_COLOUR_GREEN   "\x1b[32m"
#define CANE_COLOUR_YELLOW  "\x1b[33m"
#define CANE_COLOUR_BLUE    "\x1b[34m"
#define CANE_COLOUR_MAGENTA "\x1b[35m"
#define CANE_COLOUR_CYAN    "\x1b[36m"
#define CANE_COLOUR_WHITE   "\x1b[37m"

#define CANE_COLOUR_INFO CANE_COLOUR_WHITE
#define CANE_COLOUR_WARN CANE_COLOUR_BLUE
#define CANE_COLOUR_FAIL CANE_COLOUR_RED
#define CANE_COLOUR_OKAY CANE_COLOUR_GREEN
#define CANE_COLOUR_EXPR CANE_COLOUR_MAGENTA
#define CANE_COLOUR_FUNC CANE_COLOUR_BLUE
#define CANE_COLOUR_HERE CANE_COLOUR_YELLOW

#define CANE_LOG_KINDS \
	X(Info, ".", "info", CANE_COLOUR_INFO) \
	X(Warn, "*", "warn", CANE_COLOUR_WARN) \
	X(Fail, "!", "fail", CANE_COLOUR_FAIL) \
	X(Okay, "^", "okay", CANE_COLOUR_OKAY) \
	X(Expr, "=", "expr", CANE_COLOUR_EXPR) \
	X(Func, ">", "func", CANE_COLOUR_FUNC) \
	X(Here, "/", "here", CANE_COLOUR_HERE)

#define X(x, y, z, w) x,
	enum class LogKind {
		CANE_LOG_KINDS
	};
#undef X

#define X(x, y, z, w) CANE_CSTR(y),
	inline std::array LOG_KIND_TO_STR = { CANE_LOG_KINDS };
#undef X

#define X(x, y, z, w) CANE_CSTR(z),
	inline std::array LOG_KIND_TO_STR_HUMAN = { CANE_LOG_KINDS };
#undef X

#define X(x, y, z, w) CANE_CSTR(w),
	inline std::array LOG_KIND_TO_STR_COLOUR = { CANE_LOG_KINDS };
#undef X

	constexpr std::string_view log_kind_to_str(LogKind x) {
		return LOG_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view log_kind_to_str_human(LogKind x) {
		return LOG_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	constexpr std::string_view log_kind_to_str_colour(LogKind x) {
		return LOG_KIND_TO_STR_COLOUR[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, LogKind log) {
		return (os << log_kind_to_str_human(log));
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
CANE_FORMATTER_DEF(cane::BindingKind);
CANE_FORMATTER_DEF(cane::LogKind);

#endif
