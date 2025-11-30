#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

#include <cstddef>
#include <functional>
#include <algorithm>
#include <random>
#include <ranges>

#include <cane/value.hpp>

namespace cane {

	namespace detail {
		inline EventKind bool_to_event(bool b) {
			std::array v {
				EventKind::Rest,
				EventKind::Beat,
			};

			return v.at(b);
		}

		inline EventKind bit_not_event(EventKind e) {
			return bool_to_event(not static_cast<bool>(e));
		}

		inline EventKind bit_and_event(EventKind lhs, EventKind rhs) {
			return bool_to_event(
				static_cast<bool>(lhs) and static_cast<bool>(rhs)
			);
		}

		inline EventKind bit_or_event(EventKind lhs, EventKind rhs) {
			return bool_to_event(
				static_cast<bool>(lhs) or static_cast<bool>(rhs)
			);
		}

		inline EventKind bit_xor_event(EventKind lhs, EventKind rhs) {
			return bool_to_event(
				static_cast<bool>(lhs) xor static_cast<bool>(rhs)
			);
		}
	}  // namespace detail

	// Misc.
	template <typename T>
	inline size_t beats(const T& v) {
		return std::ranges::count_if(
			v, std::bind_front(std::equal_to {}, EventKind::Beat)
		);
	}

	template <typename T>
	inline size_t rests(const T& v) {
		return std::ranges::count_if(
			v, std::bind_front(std::equal_to {}, EventKind::Rest)
		);
	}

	template <typename T>
	inline T euclidean(size_t beats, size_t steps) {
		return std::views::iota(0) | std::views::take(steps) |
			std::views::transform([&](const auto& i) {
				   return detail::bool_to_event(((i * beats) % steps) < beats);
			   }) |
			std::ranges::to<T>();
	}

	// Identifies repeating pattern in a sequence
	// and attempts to minify it so we don't spam
	// the stdout for large sequences.
	template <typename T>
	inline T minify(T seq) {
		for (size_t f = 1; f != seq.size(); ++f) {
			if (seq.size() % f != 0) {
				continue;
			}

			bool all_eq = false;
			for (size_t i = 1; i != (seq.size() / f); ++i) {
				all_eq = std::equal(
					seq.begin(), seq.begin() + f, seq.begin() + (f * i)
				);

				if (not all_eq) {
					break;
				}
			}

			if (all_eq) {
				seq.erase(seq.begin() + f, seq.end());
				return seq;
			}
		}

		return seq;
	}

	// Accessors
	template <typename T>
	inline T head(T v, size_t n = 1) {
		return v | std::views::take(n) | std::ranges::to<T>();
	}

	template <typename T>
	inline T tail(T v, size_t n = 1) {
		return v | std::views::drop(n) | std::ranges::to<T>();
	}

	// Transformations
	template <typename T>
	inline T repeat(T v, size_t n) {
		// Copy sequence N times to the end of itself.
		// turns i.e. `[a b c]` where n = 3 into `[a b c a b c a b c]`.

		return std::views::repeat(v, n) | std::views::join |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T reverse(T v) {
		return v | std::views::reverse | std::ranges::to<T>();
	}

	template <typename T>
	inline T rotate_left(T v, size_t n) {
		std::ranges::rotate(v, v.begin() + (n % v.size()));
		return v;
	}

	template <typename T>
	inline T rotate_right(T v, size_t n) {
		auto seq = v | std::views::reverse;
		std::ranges::rotate(seq, seq.begin() + (n % seq.size()));
		return v;
	}

	template <typename T>
	inline T concatenate(T lhs, T rhs) {
		// TODO: C++26 std::views::concat
		lhs.insert(lhs.end(), rhs.begin(), rhs.end());
		return lhs;
	}

	template <typename T>
	inline T join(T lhs, T rhs) {
		// We need to find the maximum key in the left sequence and then
		// transpose the key of every event in the right sequence.

		auto timestamp =
			std::ranges::max(lhs | std::views::transform(&Event::timestamp));

		// FIXME: Can we use a projection for accessing &Event::key?
		std::ranges::transform(rhs, std::back_inserter(lhs), [&](auto x) {
			x.timestamp += timestamp;
			return x;
		});

		return lhs;
	}

	// Logic
	template <typename T>
	inline T bit_not(T v) {
		return v | std::views::transform(detail::bit_not_event) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T bit_and(T lhs, T rhs) {
		std::ranges::transform(lhs, rhs, lhs.begin(), detail::bit_and_event);
		return lhs;
	}

	template <typename T>
	inline T bit_or(T lhs, T rhs) {
		std::ranges::transform(lhs, rhs, lhs.begin(), detail::bit_or_event);
		return lhs;
	}

	template <typename T>
	inline T bit_xor(T lhs, T rhs) {
		std::ranges::transform(lhs, rhs, lhs.begin(), detail::bit_xor_event);
		return lhs;
	}

	// Arithmetic
	template <typename T>
	inline T vector_add(T lhs, T rhs) {
		return std::views::zip_transform(std::plus {}, lhs, rhs) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_sub(T lhs, T rhs) {
		return std::views::zip_transform(std::minus {}, lhs, rhs) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_mul(T lhs, T rhs) {
		return std::views::zip_transform(std::multiplies {}, lhs, rhs) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_div(T lhs, T rhs) {
		return std::views::zip_transform(std::divides {}, lhs, rhs) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_add(T v, size_t scalar) {
		return v |
			std::views::transform(std::bind_front(std::plus {}, scalar)) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_sub(T v, size_t scalar) {
		return v |
			std::views::transform(std::bind_front(std::minus {}, scalar)) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_mul(T v, size_t scalar) {
		return v |
			std::views::transform(std::bind_front(std::multiplies {}, scalar)) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_div(T v, size_t scalar) {
		return v |
			std::views::transform(std::bind_front(std::divides {}, scalar)) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline int64_t random(int64_t min, int64_t max) {
		// TODO: Handle negative numbers

		std::random_device rd;
		std::mt19937 rng;

		std::uniform_int_distribution dist(min, max);

		return dist(rng);
	}

	template <typename T>
	inline int64_t choice(T lhs, T rhs) {
		std::random_device rd;
		std::mt19937 rng;

		std::bernoulli_distribution dist(0.5f);

		return dist(rng) ? lhs : rhs;
	}

	template <typename T>
	inline T timemul(T v, size_t scalar) {
		for (auto& ev: v) {
			ev.duration *= scalar;
		}

		return v;
	}

	template <typename T>
	inline T timediv(T v, size_t scalar) {
		for (auto& ev: v) {
			ev.duration /= scalar;
		}

		return v;
	}

	template <typename T>
	inline T cycle(T v, size_t length) {
		// Cycle elements of `v` such that the length of the container matches
		// `length`.

		return std::views::repeat(v) | std::views::join |
			std::views::take(length) | std::ranges::to<T>();
	}

	template <typename S, typename R, typename M>
	inline S map(TimeUnit duration, R rhythm, M melody) {
		size_t length = std::max(rhythm.size(), melody.size());

		rhythm = cycle(rhythm, length);
		melody = cycle(melody, length);

		S seq;
		TimeUnit timestamp { 0 };

		for (auto [key, step, note]:
			 std::views::zip(std::views::iota(0), rhythm, melody)) {
			seq.emplace_back(step, timestamp, duration, 0, note, 127);

			timestamp += duration;
		}

		return seq;
	}

	template <typename T, typename D>
	inline T map_duration(T v, D duration) {
		for (auto& event: v) {
			event.timestamp = duration;
			event.duration = duration;
		}

		return v;
	}

}  // namespace cane

#endif
