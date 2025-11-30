#ifndef CANE_VALUE_HPP
#define CANE_VALUE_HPP

#include <cstdint>

#include <chrono>
#include <ostream>

#include <unordered_map>

#include <cane/def.hpp>
#include <cane/util.hpp>

namespace cane {

	////////////
	// Config //
	////////////

	struct Configuration {
		size_t bpm;
		std::unordered_map<std::string_view, uint8_t> channel_bindings;
	};

	///////////
	// Types //
	///////////

	using TimeUnit = std::chrono::microseconds;

	constexpr auto MINUTE =
		std::chrono::duration_cast<TimeUnit>(std::chrono::minutes { 1 });

	struct Event {
		// Meta
		size_t key;
		EventKind kind;

		// Timing information
		TimeUnit duration;

		// Event information
		uint8_t channel;
		uint8_t note;
		uint8_t velocity;
	};

	using Scalar = int64_t;
	using String = std::string;

	// using Rhythm = std::vector<EventKind>;
	// using Melody = std::vector<Scalar>;
	// using Sequence = std::vector<Event>;

	struct Sequence: public std::vector<Event> {
		using std::vector<Event>::vector;
	};

	struct Rhythm: public std::vector<EventKind> {
		using std::vector<EventKind>::vector;
	};

	struct Melody: public std::vector<Scalar> {
		using std::vector<Scalar>::vector;
	};

	///////////
	// Value //
	///////////

	using Value =
		std::variant<std::monostate, Scalar, String, Rhythm, Melody, Sequence>;

	constexpr std::ostream& operator<<(std::ostream& os, cane::Value value) {
		value |
			cane::match {
				[&](auto x) { os << x; },
				[&](std::monostate) { os << "(empty)"; },
			};

		return os;
	}

	constexpr std::ostream& operator<<(std::ostream& os, Event ev) {
		os << "Event { ";
		os << "key: " << ev.key << ", ";
		os << "kind: " << ev.kind << ", ";
		os << "duration: "
		   << std::chrono::duration_cast<std::chrono::milliseconds>(ev.duration)
		   << ", ";
		os << "channel: " << static_cast<int>(ev.channel) << ", ";
		os << "note: " << static_cast<int>(ev.note) << ", ";
		os << "velocity: " << static_cast<int>(ev.velocity);
		os << " }";

		return os;
	}

	constexpr std::ostream& operator<<(std::ostream& os, Sequence seq) {
		return format_container(os, seq);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Rhythm rhythm) {
		return format_container(os, rhythm);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Melody melody) {
		return format_container(os, melody);
	}

}  // namespace cane

CANE_FORMATTER_DEF(cane::Event);
CANE_FORMATTER_DEF(cane::Rhythm);
CANE_FORMATTER_DEF(cane::Melody);
CANE_FORMATTER_DEF(cane::Sequence);
CANE_FORMATTER_DEF(cane::Value);

#endif
