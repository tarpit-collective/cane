#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

#include <chrono>
#include <random>
#include <cstdint>

#include <cane/util.hpp>

namespace cane {

	///////////
	// Types //
	///////////

	using Timestamp = std::chrono::microseconds;

	// TODO: What do we need for an event?
	// - Timestamp
	// - Note/Event type
	// - Velocity
	// - Channel (Device)

	using Scalar = int64_t;
	using String = std::string_view;

	struct Event {
		Timestamp time;
	};

	struct Sequence: public std::vector<Event> {
		using std::vector<Event>::vector;
	};

	struct Rhythm: public std::vector<bool> {
		using std::vector<bool>::vector;
	};

	struct Melody: public std::vector<Scalar> {
		using std::vector<Scalar>::vector;
	};

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
				melody.begin(),
				melody.end(),
				melody.begin(),
				[&](auto x) { return x + n; }
			);

			return melody;
		}

		decltype(auto) seq_sub(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(),
				melody.end(),
				melody.begin(),
				[&](auto x) { return x - n; }
			);

			return melody;
		}

		decltype(auto) seq_mul(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(),
				melody.end(),
				melody.begin(),
				[&](auto x) { return x * n; }
			);

			return melody;
		}

		decltype(auto) seq_div(size_t n) {
			auto melody = std::get<Melody>(*this);

			std::transform(
				melody.begin(),
				melody.end(),
				melody.begin(),
				[&](auto x) { return x / n; }
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
	};

	////////////////
	// Operations //
	////////////////

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

#endif
