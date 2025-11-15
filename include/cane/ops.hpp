#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

#include <cstdint>

#include <ranges>
#include <chrono>
#include <random>

#include <cane/util.hpp>

namespace cane {

	///////////
	// Types //
	///////////

	using Unit = std::chrono::microseconds;

	// TODO: What do we need for an event?
	// - Timestamp
	// - Note/Event type
	// - Velocity
	// - Channel (Device)

	using Scalar = int64_t;
	using String = std::string_view;

	struct Event {
		size_t duration;
		size_t wait;
		uint8_t note;
		uint8_t velocity;
	};

	struct Sequence: public std::vector<Event> {
		using std::vector<Event>::vector;
	};

	struct Rhythm: public std::vector<uint8_t> {
		using std::vector<uint8_t>::vector;
	};

	struct Melody: public std::vector<Scalar> {
		using std::vector<Scalar>::vector;
	};

	constexpr std::ostream& operator<<(std::ostream& os, Event ev) {
		return (
			os << "{ duration: " << ev.duration << ", wait: " << ev.wait
			   << ", note: " << static_cast<int>(ev.note)
			   << ", velocity: " << static_cast<int>(ev.velocity) << " }"
		);
	}

	constexpr std::ostream& operator<<(std::ostream& os, Sequence seq) {
		if (seq.empty()) {
			return (os << "[]");
		}

		auto it = seq.begin();
		os << '[' << *it++;

		for (; it != seq.end(); ++it) {
			os << ", " << *it;
		}

		return (os << ']');
	}

	constexpr std::ostream& operator<<(std::ostream& os, Rhythm rhythm) {
		if (rhythm.empty()) {
			return (os << "[]");
		}

		auto it = rhythm.begin();
		os << '[' << *it++;

		for (; it != rhythm.end(); ++it) {
			os << ", " << *it;
		}

		return (os << ']');
	}

	constexpr std::ostream& operator<<(std::ostream& os, Melody melody) {
		if (melody.empty()) {
			return (os << "[]");
		}

		auto it = melody.begin();
		os << '[' << *it++;

		for (; it != melody.end(); ++it) {
			os << ", " << *it;
		}

		return (os << ']');
	}
}  // namespace cane

template <>
struct std::formatter<cane::Event>: std::formatter<std::string_view> {
	auto format(cane::Event ev, format_context& ctx) const {
		std::ostringstream ss;
		ss << ev;
		return std::formatter<string_view>::format(ss.str(), ctx);
	}
};

template <>
struct std::formatter<cane::Sequence>: std::formatter<std::string_view> {
	auto format(cane::Sequence seq, format_context& ctx) const {
		std::ostringstream ss;
		ss << seq;
		return std::formatter<string_view>::format(ss.str(), ctx);
	}
};

template <>
struct std::formatter<cane::Rhythm>: std::formatter<std::string_view> {
	auto format(cane::Rhythm rhythm, format_context& ctx) const {
		std::ostringstream ss;
		ss << rhythm;
		return std::formatter<string_view>::format(ss.str(), ctx);
	}
};

template <>
struct std::formatter<cane::Melody>: std::formatter<std::string_view> {
	auto format(cane::Melody melody, format_context& ctx) const {
		std::ostringstream ss;
		ss << melody;
		return std::formatter<string_view>::format(ss.str(), ctx);
	}
};

namespace cane {

	///////////
	// Value //
	///////////

	struct Value:
			public std::variant<
				std::monostate,
				Scalar,
				String,
				Rhythm,
				Melody,
				Sequence> {
		using std::
			variant<std::monostate, Scalar, String, Rhythm, Melody, Sequence>::
				variant;

		decltype(auto) get_scalar() {
			return std::get<Scalar>(*this);
		}

		decltype(auto) get_rhythm() {
			return std::get<Rhythm>(*this);
		}

		decltype(auto) get_melody() {
			return std::get<Melody>(*this);
		}

		decltype(auto) get_sequence() {
			return std::get<Sequence>(*this);
		}

		template <typename T>
		decltype(auto) repeat(size_t n) {
			auto seq = std::get<T>(*this);

			// Copy sequence N times to the end of itself.
			// turns i.e. `[a b c]` where N=3 into `[a b c a b c a b c]`.

			if (n == 0) {
				return seq;
			}

			size_t count = seq.size();
			seq.reserve(seq.capacity() + n * count);

			while (--n) {
				std::copy_n(seq.begin(), count, std::back_inserter(seq));
			}

			return seq;
		}

		template <typename T>
		decltype(auto) reverse() {
			auto seq = std::get<T>(*this);
			std::reverse(seq.begin(), seq.end());
			return seq;
		}

		template <typename T>
		decltype(auto) rotate_left(size_t n) {
			auto seq = std::get<T>(*this);
			std::rotate(seq.begin(), seq.begin() + (n % seq.size()), seq.end());
			return seq;
		}

		template <typename T>
		decltype(auto) rotate_right(size_t n) {
			auto seq = std::get<T>(*this);

			std::rotate(
				seq.rbegin(), seq.rbegin() + (n % seq.size()), seq.rend()
			);

			return seq;
		}

		template <typename T>
		decltype(auto) concatenate(Value other) {
			auto lhs = std::get<T>(*this);
			auto rhs = std::get<T>(other);

			lhs.insert(lhs.end(), rhs.begin(), rhs.end());

			return lhs;
		}

		template <typename T>
		decltype(auto) invert() {
			auto seq = std::get<T>(*this);

			std::transform(
				seq.cbegin(), seq.cend(), seq.begin(), std::logical_not<> {}
			);

			return seq;
		}

		decltype(auto) bit_or(Value other) {
			auto lhs = std::get<Rhythm>(*this);
			auto rhs = std::get<Rhythm>(other);

			std::transform(
				rhs.cbegin(),
				rhs.cend(),
				lhs.begin(),
				lhs.begin(),
				std::bit_or<> {}
			);

			return lhs;
		}

		decltype(auto) bit_and(Value other) {
			auto lhs = std::get<Rhythm>(*this);
			auto rhs = std::get<Rhythm>(other);

			std::transform(
				rhs.cbegin(),
				rhs.cend(),
				lhs.begin(),
				lhs.begin(),
				std::bit_and<> {}
			);

			return lhs;
		}

		decltype(auto) bit_xor(Value other) {
			auto lhs = std::get<Rhythm>(*this);
			auto rhs = std::get<Rhythm>(other);

			std::transform(
				rhs.cbegin(),
				rhs.cend(),
				lhs.begin(),
				lhs.begin(),
				std::bit_xor<> {}
			);

			return lhs;
		}

		decltype(auto) seq_add(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(), melody.end(), melody.begin(), [&](auto x) {
					return x + n;
				}
			);

			return melody;
		}

		decltype(auto) seq_sub(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(), melody.end(), melody.begin(), [&](auto x) {
					return x - n;
				}
			);

			return melody;
		}

		decltype(auto) seq_mul(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(), melody.end(), melody.begin(), [&](auto x) {
					return x * n;
				}
			);

			return melody;
		}

		decltype(auto) seq_div(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(), melody.end(), melody.begin(), [&](auto x) {
					return x / n;
				}
			);

			return melody;
		}

		// Generate a new sequence using the first element of another sequence.
		template <typename T>
		decltype(auto) head() {
			auto seq = std::get<T>(*this);

			auto it = seq.begin();

			seq.insert(seq.begin(), *it);
			seq.erase(seq.begin() + 1, seq.end());

			return seq;
		}

		// Generate a new sequence with everything but the first element.
		template <typename T>
		decltype(auto) tail() {
			auto seq = std::get<T>(*this);

			if (seq.empty()) {
				return seq;
			}

			seq.erase(seq.begin());
			return seq;
		}

		template <typename T>
		decltype(auto) length() {
			auto seq = std::get<T>(*this);
			return seq.size();
		}

		decltype(auto) beats() {
			auto rhythm = std::get<Rhythm>(*this);

			return std::count_if(rhythm.begin(), rhythm.end(), [](auto x) {
				return x;
			});
		}

		decltype(auto) rests() {
			auto rhythm = std::get<Rhythm>(*this);

			return std::count_if(rhythm.begin(), rhythm.end(), [](auto x) {
				return not x;
			});
		}

		decltype(auto) euclidean(Value other) {
			auto beats = std::get<Scalar>(*this);
			auto steps = std::get<Scalar>(other);

			Rhythm rhythm;

			if (beats > steps) {
				cane::die("not enough steps");
			}

			for (int64_t i = 0; i != steps; ++i) {
				rhythm.emplace_back(((i * beats) % steps) < beats);
			}

			return rhythm;
		}

		decltype(auto) absolute() {
			auto scalar = std::get<Scalar>(*this);
			return std::abs(scalar);
		}

		decltype(auto) negate() {
			auto scalar = std::get<Scalar>(*this);
			return -(scalar);
		}

		decltype(auto) add(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs + rhs;
		}

		decltype(auto) sub(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs - rhs;
		}

		decltype(auto) mul(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs * rhs;
		}

		decltype(auto) div(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs / rhs;
		}

		decltype(auto) shift_left(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs << rhs;
		}

		decltype(auto) shift_right(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return lhs >> rhs;
		}

		decltype(auto) lcm(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return std::lcm(lhs, rhs);
		}

		decltype(auto) gcd(Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			return std::gcd(lhs, rhs);
		}

		decltype(auto) random(std::mt19937_64& rng, Value other) {
			// TODO: Handle negative numbers
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			std::uniform_int_distribution<std::mt19937_64::result_type> dist(
				lhs, rhs
			);

			return dist(rng);
		}

		decltype(auto) choice(std::mt19937_64& rng, Value other) {
			auto lhs = std::get<Scalar>(*this);
			auto rhs = std::get<Scalar>(other);

			std::bernoulli_distribution dist(0.5);

			if (dist(rng)) {
				return lhs;
			}

			return rhs;
		}

		template <typename X, typename Y>
		decltype(auto) cycle(Value other) {
			auto lhs_value = *this;
			auto rhs_value = other;

			auto lhs = std::get<X>(lhs_value);
			auto rhs = std::get<Y>(rhs_value);

			auto repeat_n = lhs.size() / rhs.size();
			auto cycled = rhs_value.repeat<Y>(repeat_n + 1);

			CANE_OKAY(
				"melody = {}, rhythm = {}, repeat_n = {}",
				lhs.size(),
				rhs.size(),
				repeat_n
			);

			cycled.erase(cycled.begin() + lhs.size(), cycled.end());

			return cycled;
		}

		decltype(auto)
		generate_sequence(size_t bpm, Melody melody, Rhythm rhythm) {
			Sequence seq;

			size_t ms_per_note = 60'000 / bpm;

			for (auto [note, beat]: std::views::zip(melody, rhythm)) {
				CANE_OKAY("{} -> {}", note, beat);
				seq.emplace_back(ms_per_note, 0, note, 127);
			}

			return seq;
		}

		decltype(auto) map_onto_melody(size_t bpm, Value other) {
			// TODO: Map the notes to all indices, including rests
			CANE_FUNC();

			auto melody = std::get<Melody>(*this);
			auto rhythm = cycle<Melody, Rhythm>(other);

			return generate_sequence(bpm, melody, rhythm);
		}

		decltype(auto) map_onto_rhythm(size_t bpm, Value other) {
			// TODO: Map notes only to beats, not rests.
			CANE_FUNC();

			auto rhythm = std::get<Rhythm>(*this);
			auto melody = cycle<Rhythm, Melody>(other);

			return generate_sequence(bpm, melody, rhythm);
		}

		decltype(auto) timemul(Value other) {
			auto seq = std::get<Sequence>(*this);
			auto scalar = std::get<Scalar>(other);

			for (auto& ev: seq) {
				ev.duration *= scalar;
			}

			return seq;
		}

		decltype(auto) timediv(Value other) {
			auto seq = std::get<Sequence>(*this);
			auto scalar = std::get<Scalar>(other);

			for (auto& ev: seq) {
				ev.duration /= scalar;
			}

			return seq;
		}
	};

	constexpr std::ostream& operator<<(std::ostream& os, cane::Value value) {
		value |
			cane::match {
				[&](auto x) { os << x; },
				[&](std::monostate) { os << "(empty)"; },
			};

		return os;
	}

	// Identifies repeating pattern in a sequence
	// and attempts to minify it so we don't spam
	// the stdout for large sequences.
	// inline decltype(auto) sequence_minify(Sequence seq) {
	// 	for (size_t f = 1; f != seq.size(); ++f) {
	// 		if (seq.size() % f != 0) {
	// 			continue;
	// 		}

	// 		bool all_eq = false;
	// 		for (size_t i = 1; i != (seq.size() / f); ++i) {
	// 			all_eq = std::equal(
	// 				seq.begin(), seq.begin() + f, seq.begin() + (f * i)
	// 			);

	// 			if (not all_eq) {
	// 				break;
	// 			}
	// 		}

	// 		if (all_eq) {
	// 			seq.erase(seq.begin() + f, seq.end());
	// 			return seq;
	// 		}
	// 	}

	// 	return seq;
	// }

}  // namespace cane

template <>
struct std::formatter<cane::Value>: std::formatter<std::string_view> {
	auto format(cane::Value value, format_context& ctx) const {
		std::ostringstream ss;
		ss << value;
		return std::formatter<string_view>::format(ss.str(), ctx);
	}
};

#endif
