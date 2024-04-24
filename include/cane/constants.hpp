#ifndef CANE_CONSTANTS_HPP
#define CANE_CONSTANTS_HPP

namespace cane {

constexpr size_t CHANNEL_MIN     = 1u;
constexpr size_t CHANNEL_MAX     = 16u;
constexpr size_t CHANNEL_DEFAULT = 1u;

constexpr size_t NOTE_MIN     = 0u;
constexpr size_t NOTE_MAX     = 127u;
constexpr size_t NOTE_DEFAULT = 60u; // Middle C

constexpr size_t VELOCITY_MIN     = 0u;
constexpr size_t VELOCITY_MAX     = 127u;
constexpr size_t VELOCITY_DEFAULT = 127u;

constexpr size_t BPM_MIN     = 1u;
constexpr size_t BPM_MAX     = 9999u;
constexpr size_t BPM_DEFAULT = 120u;

constexpr uint64_t DURATION_DEFAULT = 60'000 / BPM_DEFAULT;

constexpr uint64_t MILLI  = 1000;
constexpr uint64_t SECOND = 1000 * MILLI;
constexpr uint64_t MINUTE = 60 * SECOND;


constexpr auto ACTIVE_SENSING_INTERVAL = 250 * MILLI;  // ms

constexpr auto ALL_SOUND_OFF = 120;
constexpr auto ALL_RESET_CC  = 121;
constexpr auto LOCAL_CONTROL = 122;
constexpr auto ALL_NOTES_OFF = 123;

constexpr auto OMNI_MODE_OFF = 124;
constexpr auto OMNI_MODE_ON  = 125;
constexpr auto MONO_MODE_ON  = 126;
constexpr auto POLY_MODE_ON  = 127;

constexpr auto LOCAL_CONTROL_ON  = 127;
constexpr auto LOCAL_CONTROL_OFF = 0;


#define MIDI \
	X(NOTE_OFF_1,   0b10000000) \
	X(NOTE_OFF_2,   0b10000001) \
	X(NOTE_OFF_3,   0b10000010) \
	X(NOTE_OFF_4,   0b10000011) \
	X(NOTE_OFF_5,   0b10000100) \
	X(NOTE_OFF_6,   0b10000101) \
	X(NOTE_OFF_7,   0b10000110) \
	X(NOTE_OFF_8,   0b10000111) \
	X(NOTE_OFF_9,   0b10001000) \
	X(NOTE_OFF_10,  0b10001001) \
	X(NOTE_OFF_11,  0b10001010) \
	X(NOTE_OFF_12,  0b10001011) \
	X(NOTE_OFF_13,  0b10001100) \
	X(NOTE_OFF_14,  0b10001101) \
	X(NOTE_OFF_15,  0b10001110) \
	X(NOTE_OFF_16,  0b10001111) \
	\
	X(NOTE_ON_1,    0b10010000) \
	X(NOTE_ON_2,    0b10010001) \
	X(NOTE_ON_3,    0b10010010) \
	X(NOTE_ON_4,    0b10010011) \
	X(NOTE_ON_5,    0b10010100) \
	X(NOTE_ON_6,    0b10010101) \
	X(NOTE_ON_7,    0b10010110) \
	X(NOTE_ON_8,    0b10010111) \
	X(NOTE_ON_9,    0b10011000) \
	X(NOTE_ON_10,   0b10011001) \
	X(NOTE_ON_11,   0b10011010) \
	X(NOTE_ON_12,   0b10011011) \
	X(NOTE_ON_13,   0b10011100) \
	X(NOTE_ON_14,   0b10011101) \
	X(NOTE_ON_15,   0b10011110) \
	X(NOTE_ON_16,   0b10011111) \
	\
	X(NOTE_ON,      0b10010000) \
	X(NOTE_OFF,     0b10000000) \
	X(START,        0b11111010) \
	X(STOP,         0b11111100) \
	X(ACTIVE_SENSE, 0b11111110) \
	X(TIMING_CLOCK, 0b11111000) \
	X(CHANNEL_MODE, 0b10110000)

	#define X(name, value) name,
		enum class Midi { MIDI };
	#undef X

	#define X(name, value) value,
		constexpr std::array MIDI_TO_INT = { MIDI };
	#undef X

	#define X(name, value) #name##_sv,
		constexpr std::array MIDI_TO_STRING = { MIDI };
	#undef X

	#define X(name, value) m == Midi::name ? value:
		constexpr decltype(auto) midi2int(Midi m) {
			return MIDI 0;
		}
	#undef X

	constexpr decltype(auto) midi2str(Midi m) {
		return MIDI_TO_STRING[(int)m];
	}

	#define X(name, value) x == value ? midi2str(Midi::name) :
		constexpr decltype(auto) int2midi(uint8_t x) {
			return MIDI "NONE"_sv;
		}
	#undef X

#undef MIDI


#define SYMBOL_TYPES \
	X(NONE,       "none") \
	X(TERMINATOR, "eof") \
	\
	/* Special */ \
	X(IDENT, "ident") \
	X(INT,   "int") \
	\
	X(WITH, "$") \
	X(REF,  "\\") \
	\
	/* Grouping */ \
	X(LPAREN, "(") \
	X(RPAREN, ")") \
	\
	/* Keywords */ \
	X(GLOBAL_BPM,  "bpm") \
	X(GLOBAL_NOTE, "note") \
	\
	X(LET, "let") \
	X(PAT, "pat") \
	\
	/* Sequence */ \
	X(SEP,  ":") \
	X(SKIP, ".") \
	X(BEAT, "!") \
	X(SUS,  "=") \
	\
	/* Sequence Operators */ \
	X(CHAIN, "=>") \
	\
	X(SEND,   "~>") \
	X(MAP,    "map") \
	X(MAPALL, "mapall") \
	X(VEL,    "vel") \
	X(BPM,    "@") \
	\
	X(CAR, "car") \
	X(CDR, "cdr") \
	X(DBG, "?") \
	\
	X(UP,   "up") \
	X(DOWN, "down") \
	\
	X(FILL, "fill") \
	\
	X(LEN_OF,  "len") \
	X(BEAT_OF, "beats") \
	X(SKIP_OF, "skips") \
	\
	X(ROTL, "<") \
	X(ROTR, ">") \
	X(REP,  "**") \
	\
	X(OR,  "|") \
	X(AND, "&") \
	X(XOR, "^") \
	X(CAT, ",") \
	\
	X(INVERT, "~") \
	X(REV,    "'") \
	\
	/* Literal Operators */ \
	X(ADD, "+") \
	X(SUB, "-") \
	X(MUL, "*") \
	X(DIV, "/")

	#define X(name, str) name,
		enum class Symbols { SYMBOL_TYPES };
	#undef X

	#define X(name, str) str##_sv,
		constexpr View SYMBOL_TO_STRING[] = { SYMBOL_TYPES };
	#undef X

	constexpr decltype(auto) sym2str(Symbols s) {
		return SYMBOL_TO_STRING[(int)s];
	}

#undef SYMBOL_TYPES

inline std::ostream& operator<<(std::ostream& os, Symbols s) {
	return (os << sym2str(s));
}

#define STEPS \
	X(SKIP, Symbols::SKIP, CANE_BLUE) \
	X(BEAT, Symbols::BEAT, CANE_YELLOW) \
	X(SUS,  Symbols::SUS,  CANE_YELLOW)

	#define X(name, sym, colour) name,
		enum Steps { STEPS };
	#undef X

	#define X(name, sym, colour) sym2str(sym),
		constexpr View STEP_TO_STRING[] = { STEPS };
	#undef X

	#define X(name, sym, colour) #name##_sv,
		constexpr View STEP_TO_NAME[] = { STEPS };
	#undef X

	#define X(name, str, colour) colour,
		constexpr View STEP_TO_COLOUR[] = { STEPS };
	#undef X

	constexpr decltype(auto) step2colour(uint8_t s) {
		return STEP_TO_COLOUR[s];
	}

	constexpr decltype(auto) step2str(uint8_t s) {
		return STEP_TO_STRING[s];
	}

	constexpr decltype(auto) step2name(uint8_t s) {
		return STEP_TO_NAME[s];
	}

	#define X(name, sym, colour) s == sym ? name:
		constexpr decltype(auto) sym2step(Symbols s) {
			return STEPS 0;
		}
	#undef X

#undef STEPS

}

#endif

