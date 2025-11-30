#ifndef CANE_UTIL_HPP
#define CANE_UTIL_HPP

#include <type_traits>
#include <utility>
#include <variant>

#include <sstream>
#include <fstream>

#include <cane/log.hpp>

namespace cane {

	// Some helpers for working with std::variant.
	template <class... Ts>
	struct overloads: Ts... {
		using Ts::operator()...;
	};

	template <typename... Fs>
	struct match: Fs... {
		using Fs::operator()...;
	};

	template <class... Ts>
	match(Ts...) -> match<Ts...>;

	template <typename... Ts, typename... Fs>
	constexpr decltype(auto)
	operator|(const std::variant<Ts...>& v, const match<Fs...>& match) {
		return std::visit(match, v);
	}

	template <typename... Ts, typename... Fs>
	constexpr decltype(auto)
	operator|(std::variant<Ts...>& v, const match<Fs...>& match) {
		return std::visit(match, v);
	}

}  // namespace cane

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
					cane::report(
						ReportKind::Generic,
						"symlink '{}' resolves to itself",
						path.string()
					);
				}

				cur = tmp;
			}

			if (std::filesystem::is_directory(cur) or
				std::filesystem::is_other(cur)) {
				cane::report(
					ReportKind::Generic, "'{}' is not a file", path.string()
				);
			}

			if (not std::filesystem::exists(cur)) {
				cane::report(
					ReportKind::Generic, "file '{}' not found", path.string()
				);
			}

			std::ifstream is(cur);

			if (not is.is_open()) {
				cane::report(
					ReportKind::Generic, "cannot read '{}'", path.string()
				);
			}

			std::stringstream ss;
			ss << is.rdbuf();

			return ss.str();
		}

		catch (const std::filesystem::filesystem_error&) {
			cane::report(
				ReportKind::Generic, "cannot read '{}'", path.string()
			);
		}

		cane::report(
			ReportKind::Generic,
			"unknown error when trying to read '{}'",
			path.string()
		);
	}

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
}  // namespace cane

#endif
