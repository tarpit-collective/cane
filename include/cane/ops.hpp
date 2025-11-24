#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

#include <cstdint>

#include <ranges>
#include <chrono>
#include <random>
#include <ostream>

#include <cane/def.hpp>
#include <cane/util.hpp>

namespace cane {

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

	struct Value:
			public std::variant<
				std::monostate,
				Scalar,
				String,
				Rhythm,
				Melody,
				Sequence,
				Pattern> {
		using std::variant<
			std::monostate,
			Scalar,
			String,
			Rhythm,
			Melody,
			Sequence,
			Pattern>::variant;

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

		decltype(auto) get_pattern() {
			return std::get<Pattern>(*this);
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

			std::transform(seq.cbegin(), seq.cend(), seq.begin(), [](auto x) {
				return x == EventKind::Beat ? EventKind::Rest : EventKind::Beat;
			});

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
				[](auto lhs, auto rhs) {
					return (static_cast<uint8_t>(lhs) |
							static_cast<uint8_t>(rhs)) ?
						EventKind::Beat :
						EventKind::Rest;
				}
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
				[](auto lhs, auto rhs) {
					return (static_cast<uint8_t>(lhs) &
							static_cast<uint8_t>(rhs)) ?
						EventKind::Beat :
						EventKind::Rest;
				}
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
				[](auto lhs, auto rhs) {
					return (static_cast<uint8_t>(lhs) ^
							static_cast<uint8_t>(rhs)) ?
						EventKind::Beat :
						EventKind::Rest;
				}
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
				return x == EventKind::Beat;
			});
		}

		decltype(auto) rests() {
			auto rhythm = std::get<Rhythm>(*this);

			return std::count_if(rhythm.begin(), rhythm.end(), [](auto x) {
				return x == EventKind::Rest;
			});
		}

		decltype(auto) euclidean(Value other) {
			auto beats = std::get<Scalar>(*this);
			auto steps = std::get<Scalar>(other);

			Rhythm rhythm;

			if (beats > steps) {
				cane::report(
					ReportKind::Eval,
					"more beats({}) than steps({})",
					beats,
					steps
				);
			}

			for (int64_t i = 0; i != steps; ++i) {
				int64_t val = ((i * beats) % steps) < beats;
				rhythm.emplace_back(val ? EventKind::Beat : EventKind::Rest);
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

			TimeUnit duration = MINUTE / bpm;

			size_t key = 0;

			for (auto [note, beat]: std::views::zip(melody, rhythm)) {
				CANE_OKAY("{} -> {}", note, beat);

				seq.emplace_back(key, beat, duration, 0, note, 127);
				key++;
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
			// - Set up the notes array here by interspersing 0s in places where
			// we have rests
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

		decltype(auto) send(Value other) {
			auto seq = std::get<Sequence>(*this);
			auto string = std::get<String>(other);

			Pattern pat;

			CANE_OKAY("send: {}", string);

			for (auto x: seq) {
				// TODO: Look up environment using string and set channel
				// appropriately.
				x.channel = 6;  // FIX: Temporary value for testing.
				pat.emplace_back(x);
			}

			// TODO: Optimise sequence by joining neighbouring rests

			return pat;
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
