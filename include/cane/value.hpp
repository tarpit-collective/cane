#ifndef CANE_VALUE_HPP
#define CANE_VALUE_HPP

#include <cstdint>

#include <ranges>
#include <chrono>
#include <random>
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

	using Scalar = int64_t;
	using String = std::string_view;

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

	struct Pattern: public std::vector<Event> {
		using std::vector<Event>::vector;
	};

	struct Sequence: public std::vector<Event> {
		using std::vector<Event>::vector;
	};

	struct Rhythm: public std::vector<EventKind> {
		using std::vector<EventKind>::vector;
	};

	struct Melody: public std::vector<Scalar> {
		using std::vector<Scalar>::vector;
	};

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

	namespace detail {
		template <typename T>
		constexpr std::ostream& format_container(std::ostream& os, T x) {
			if (x.empty()) {
				return (os << "[]");
			}

			auto it = x.begin();
			os << '[' << *it++;

			for (; it != x.end(); ++it) {
				os << ", " << *it;
			}

			return (os << ']');
		}
	}  // namespace detail

	constexpr std::ostream& operator<<(std::ostream& os, Sequence seq) {
		os << "Sequence ";
		return detail::format_container(os, seq);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Pattern pat) {
		os << "Pattern ";
		return detail::format_container(os, pat);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Rhythm rhythm) {
		os << "Rhythm ";
		return detail::format_container(os, rhythm);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Melody melody) {
		os << "Melody ";
		return detail::format_container(os, melody);
	}
}  // namespace cane

CANE_FORMATTER_DEF(cane::Event);
CANE_FORMATTER_DEF(cane::Sequence);
CANE_FORMATTER_DEF(cane::Pattern);
CANE_FORMATTER_DEF(cane::Rhythm);
CANE_FORMATTER_DEF(cane::Melody);

namespace cane {

	///////////
	// Value //
	///////////

	using Value = std::variant<
		std::monostate,
		Scalar,
		String,
		Rhythm,
		Melody,
		Sequence,
		Pattern>;

	// struct Value: public VariantType {
	// 	using VariantType::variant;

	// 	decltype(auto) random(std::mt19937_64& rng, Value other) {
	// 		// TODO: Handle negative numbers
	// 		auto lhs = std::get<Scalar>(*this);
	// 		auto rhs = std::get<Scalar>(other);

	// 		std::uniform_int_distribution<std::mt19937_64::result_type> dist(
	// 			lhs, rhs
	// 		);

	// 		return dist(rng);
	// 	}

	// 	decltype(auto) choice(std::mt19937_64& rng, Value other) {
	// 		auto lhs = std::get<Scalar>(*this);
	// 		auto rhs = std::get<Scalar>(other);

	// 		std::bernoulli_distribution dist(0.5);

	// 		if (dist(rng)) {
	// 			return lhs;
	// 		}

	// 		return rhs;
	// 	}

	// 	template <typename X, typename Y>
	// 	decltype(auto) cycle(Value other) {
	// 		auto lhs_value = *this;
	// 		auto rhs_value = other;

	// 		auto lhs = std::get<X>(lhs_value);
	// 		auto rhs = std::get<Y>(rhs_value);

	// 		auto repeat_n = lhs.size() / rhs.size();
	// 		auto cycled = rhs_value.repeat<Y>(repeat_n + 1);

	// 		CANE_OKAY(
	// 			"melody = {}, rhythm = {}, repeat_n = {}",
	// 			lhs.size(),
	// 			rhs.size(),
	// 			repeat_n
	// 		);

	// 		cycled.erase(cycled.begin() + lhs.size(), cycled.end());

	// 		return cycled;
	// 	}

	// 	decltype(auto)
	// 	generate_sequence(Configuration cfg, Melody melody, Rhythm rhythm) {
	// 		Sequence seq;

	// 		TimeUnit duration = MINUTE / cfg.bpm;

	// 		size_t key = 0;

	// 		for (auto [note, beat]: std::views::zip(melody, rhythm)) {
	// 			CANE_OKAY("{} -> {}", note, beat);

	// 			seq.emplace_back(key, beat, duration, 0, note, 127);
	// 			key++;
	// 		}

	// 		return seq;
	// 	}

	// 	decltype(auto) map_onto_melody(Configuration cfg, Value other) {
	// 		// TODO: Map the notes to all indices, including rests
	// 		CANE_FUNC();

	// 		auto melody = std::get<Melody>(*this);
	// 		auto rhythm = cycle<Melody, Rhythm>(other);

	// 		return generate_sequence(cfg, melody, rhythm);
	// 	}

	// 	decltype(auto) map_onto_rhythm(Configuration cfg, Value other) {
	// 		// TODO: Map notes only to beats, not rests.
	// 		// - Set up the notes array here by interspersing 0s in places where
	// 		// we have rests
	// 		CANE_FUNC();

	// 		auto rhythm = std::get<Rhythm>(*this);
	// 		auto melody = cycle<Rhythm, Melody>(other);

	// 		return generate_sequence(cfg, melody, rhythm);
	// 	}

	// 	decltype(auto) timemul(Value other) {
	// 		auto seq = std::get<Sequence>(*this);
	// 		auto scalar = std::get<Scalar>(other);

	// 		for (auto& ev: seq) {
	// 			ev.duration *= scalar;
	// 		}

	// 		return seq;
	// 	}

	// 	decltype(auto) timediv(Value other) {
	// 		auto seq = std::get<Sequence>(*this);
	// 		auto scalar = std::get<Scalar>(other);

	// 		for (auto& ev: seq) {
	// 			ev.duration /= scalar;
	// 		}

	// 		return seq;
	// 	}

	constexpr std::ostream& operator<<(std::ostream& os, cane::Value value) {
		value |
			cane::match {
				[&](auto x) { os << x; },
				[&](std::monostate) { os << "(empty)"; },
			};

		return os;
	}
}  // namespace cane

CANE_FORMATTER_DEF(cane::Value);

#endif
