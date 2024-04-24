#ifndef CANE_TYPES_HPP
#define CANE_TYPES_HPP

namespace cane {

struct Token {
	cane::View view = sym2str(Symbols::NONE);
	cane::Symbols kind = Symbols::NONE;
};

struct Event {
	uint64_t duration;
	uint8_t note;
	uint8_t velocity;
	uint8_t kind;

	constexpr Event(uint8_t kind_):
		duration(DURATION_DEFAULT),
		note(NOTE_DEFAULT),
		velocity(VELOCITY_DEFAULT),
		kind(kind_) {}

	constexpr Event(uint64_t duration_, uint8_t note_, uint8_t vel_, uint8_t kind_):
		duration(duration_),
		note(note_),
		velocity(vel_),
		kind(kind_) {}
};

struct MidiEvent {
	uint64_t time;
	std::array<uint8_t, 3> data;

	constexpr MidiEvent(uint64_t time_, uint8_t status, uint8_t note, uint8_t velocity):
		time(time_), data({status, note, velocity}) {}
};

enum {
	SEQ_NONE,
	SEQ_NOTE      = 1 << 0,
	SEQ_DURATION  = 1 << 1,
};

struct Sequence: public std::vector<Event> {
	uint8_t flags = SEQ_NONE;
	Sequence(): std::vector<Event>::vector() {}
};

struct Timeline: public std::vector<MidiEvent> {
	uint64_t duration = 0u;
	Timeline(): std::vector<MidiEvent>::vector() {}
};

enum class HandlerKind {
	Error,
	Warning,
	Notice,
};

using Handler = void(*)(HandlerKind, Phases, View, View, std::string);

struct Context {
	std::unordered_map<View, double> constants;
	std::unordered_map<View, Sequence> chains;
	std::unordered_map<View, Timeline> patterns;

	std::unordered_set<View> symbols;

	Timeline tl;
	uint64_t time = 0u;

	Handler handler;

	inline Context(Handler&& handler_):
		handler(handler_) {}
};

inline void default_handler(HandlerKind kind, Phases phase, View original, View sv, std::string str) {
	switch (kind) {
		case HandlerKind::Error: {
			report_error(std::cerr, phase, original, sv, str);
		} break;

		case HandlerKind::Warning: {
			report_warning(std::cerr, phase, original, sv, str);
		} break;

		case HandlerKind::Notice: {
			report_notice(std::cerr, phase, original, sv, str);
		} break;
	}
}

inline std::ostream& operator<<(std::ostream& os, Sequence& s) {
	for (auto& [dur, note, vel, kind]: s)
		print(os, step2colour(kind), step2str(kind));

	return print(os, CANE_RESET);
}


inline std::ostream& operator<<(std::ostream& os, Timeline& tl) {
	constexpr auto longest = *std::max_element(MIDI_TO_STRING.begin(), MIDI_TO_STRING.end(), [] (auto& lhs, auto& rhs) {
		return lhs.size() < rhs.size();
	});

	for (MidiEvent& ev: tl) {
		View sv = int2midi(ev.data[0]);
		std::string padding(longest.size() - sv.size(), ' ');

		// French flag
		print(os, CANE_BLUE, sv, padding, CANE_RESET " ");

		print(os, "[ ", CANE_BOLD, (int)ev.data[1], " ");
		print(os, (int)ev.data[2], CANE_RESET " ] ");

		print(os, CANE_RED, ev.time / MILLI, cane::STR_MILLI_SUFFIX, CANE_RESET);
		println(os);
	}

	return os;
}

}

#endif
