#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

#include <cstddef>
#include <variant>
#include <algorithm>
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
		return std::ranges::count_if(v, [](auto x) {
			return x == EventKind::Beat;
		});
	}

	template <typename T>
	inline size_t rests(const T& v) {
		return std::ranges::count_if(v, [](auto x) {
			return x == EventKind::Rest;
		});
	}

	template <typename T>
	inline T euclidean(size_t beats, size_t steps) {
		auto v = std::views::iota(0) | std::views::take(steps) |
			std::ranges::to<T>();

		std::ranges::transform(
			v.begin(), v.end(), v.begin(), [&](const auto& i) {
				return detail::bool_to_event(((i * beats) % steps) < beats);
			}
		);

		return v;
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
		// turns i.e. `[a b c]` where N=3 into `[a b c a b c a b c]`.

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
		return lhs | std::views::transform(rhs, lhs.begin(), std::plus {}) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_sub(T lhs, T rhs) {
		return lhs | std::views::transform(rhs, lhs.begin(), std::minus {}) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_mul(T lhs, T rhs) {
		return lhs |
			std::views::transform(rhs, lhs.begin(), std::multiplies {}) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T vector_div(T lhs, T rhs) {
		return lhs | std::views::transform(rhs, lhs.begin(), std::divides {}) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_add(T v, size_t scalar) {
		return v |
			std::views::transform([&](const auto& x) { return x + scalar; }) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_sub(T v, size_t scalar) {
		return v |
			std::views::transform([&](const auto& x) { return x - scalar; }) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_mul(T v, size_t scalar) {
		return v |
			std::views::transform([&](const auto& x) { return x * scalar; }) |
			std::ranges::to<T>();
	}

	template <typename T>
	inline T scalar_div(T v, size_t scalar) {
		return v |
			std::views::transform([&](const auto& x) { return x / scalar; }) |
			std::ranges::to<T>();
	}

}  // namespace cane

#endif
