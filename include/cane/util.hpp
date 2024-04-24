#ifndef CANE_UTIL_HPP
#define CANE_UTIL_HPP

// Macros
namespace cane {
	#define CANE_STR_IMPL(x) #x
	#define CANE_STR(x) CANE_STR_IMPL(x)

	#define CANE_CAT_IMPL(x, y) x##y
	#define CANE_CAT(x, y) CANE_CAT_IMPL(x, y)

	#define CANE_VAR(x) CANE_CAT(var_, CANE_CAT(x, CANE_CAT(__LINE__, _)))

	#define CANE_TRACE __FILE__ ":" CANE_STR(__LINE__)
}

// Utilities
namespace cane {
	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) or std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) and std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool none(T&& first, Ts&&... rest) {
		return ((not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool cmp_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}

	constexpr auto hash_bytes(const char* begin, const char* const end) {
		size_t offset_basis = 0;
		size_t prime = 0;

		if constexpr(sizeof(size_t) == 4) {
			offset_basis = 2'166'136'261ul;
			prime = 16'777'619ul;
		}

		else if constexpr(sizeof(size_t) == 8) {
			offset_basis = 14'695'981'039'346'656'037ul;
			prime = 1'099'511'628'211ul;
		}

		size_t hash = offset_basis;

		while (begin != end) {
			hash = (hash ^ static_cast<size_t>(*begin)) * prime;
			begin++;
		}

		return hash;
	}
}

#endif

