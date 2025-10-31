#ifndef CANE_UTIL_HPP
#define CANE_UTIL_HPP

#include <stdexcept>
#include <type_traits>
#include <utility>
#include <optional>

#include <sstream>
#include <fstream>

#include <cane/macro.hpp>
#include <cane/log.hpp>

// Concepts
namespace cane {
	template <typename T, typename U>
	concept Same = std::is_same_v<T, U>;
}

namespace cane {
	// Return absolute difference between 2 pointers regardless of order.
	template <typename T>
	static size_t cane_ptrdiff(T* a, T* b) {
		return b > a ? b - a : a - b;
	}
}

// Fatal errors
namespace cane {
	class Fatal: public std::runtime_error {  // Fatal error type that is caught
											  // at the highest level.
		using runtime_error::runtime_error;
	};

	template <typename T, typename... Ts>
	[[noreturn]] inline void die(T&& arg, Ts&&... args) {
		std::stringstream ss;

		// Contains location information
		if constexpr (std::is_same_v<T, LogInfo>) {
			cane::log(ss, LogKind::Fail, arg, std::forward<Ts>(args)...);
		}

		else {
			cane::log(
				ss,
				LogKind::Fail,
				std::nullopt,
				std::forward<T>(arg),
				std::forward<Ts>(args)...
			);
		}

		throw Fatal { ss.str() };
	}

// DIE macro includes location information whereas direct call does not.
#define CANE_DIE(...) \
	do { \
		[CANE_VAR(func) = \
			 CANE_LOCATION_FUNC]<typename... Ts>(Ts&&... CANE_VAR(args)) { \
			cane::die( \
				cane::LogInfo { \
					CANE_LOCATION_FILE, CANE_LOCATION_LINE, CANE_VAR(func) }, \
				std::forward<Ts>(CANE_VAR(args))... \
			); \
		}(__VA_ARGS__); \
	} while (0)

#define CANE_UNREACHABLE() \
	do { \
		cane::die( \
			cane::LogInfo { \
				CANE_LOCATION_FILE, CANE_LOCATION_LINE, CANE_LOCATION_FUNC }, \
			"unreachable!" \
		); \
	} while (0)

}  // namespace cane

// Utilities
namespace cane {
	// Comparisons
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
		return (
			(not std::forward<T>(first) and not std::forward<Ts>(rest)) and ...
		);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_all(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) and ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_any(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) == std::forward<Ts>(rest)) or ...);
	}

	template <typename T, typename... Ts>
	[[nodiscard]] constexpr bool eq_none(T&& first, Ts&&... rest) {
		return ((std::forward<T>(first) != std::forward<Ts>(rest)) and ...);
	}

	// Trim surrounding whitespace
	inline std::string_view trim(std::string_view s) {
		auto it = s.begin();
		while (it != s.end() and isspace(*it)) {
			++it;
		}

		auto rit = s.rbegin();
		while (rit != s.rend() and isspace(*rit)) {
			++rit;
		}

		return { it, rit.base() };
	}
}  // namespace cane

// IO
namespace cane {
	inline std::string read_file(std::filesystem::path path) {
		try {
			std::filesystem::path cur = path;

			while (std::filesystem::is_symlink(cur)) {
				std::filesystem::path tmp = std::filesystem::read_symlink(cur);

				if (tmp == cur) {
					die("symlink '{}' resolves to itself", path.string());
				}

				cur = tmp;
			}

			if (std::filesystem::is_directory(cur) or
				std::filesystem::is_other(cur)) {
				die("'{}' is not a file", path.string());
			}

			if (not std::filesystem::exists(cur)) {
				die("file '{}' not found", path.string());
			}

			std::ifstream is(cur);

			if (not is.is_open()) {
				die("cannot read '{}'", path.string());
			}

			std::stringstream ss;
			ss << is.rdbuf();

			return ss.str();
		}

		catch (const std::filesystem::filesystem_error&) {
			die("cannot read '{}'", path.string());
		}

		die("unknown error when trying to read '{}'", path.string());
	}
}  // namespace cane

#endif
