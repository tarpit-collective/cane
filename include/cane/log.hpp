#ifndef CANE_LOG_HPP
#define CANE_LOG_HPP

#include <cstddef>

#include <iostream>
#include <print>
#include <filesystem>

#include <array>
#include <string_view>

#include <cane/macro.hpp>

namespace cane {

// ANSI Colours
#define CANE_RESET "\x1b[0m"
#define CANE_BOLD  "\x1b[1m"

#define CANE_COLOUR_BLACK   "\x1b[30m"
#define CANE_COLOUR_RED     "\x1b[31m"
#define CANE_COLOUR_GREEN   "\x1b[32m"
#define CANE_COLOUR_YELLOW  "\x1b[33m"
#define CANE_COLOUR_BLUE    "\x1b[34m"
#define CANE_COLOUR_MAGENTA "\x1b[35m"
#define CANE_COLOUR_CYAN    "\x1b[36m"
#define CANE_COLOUR_WHITE   "\x1b[37m"

#define CANE_COLOUR_INFO CANE_COLOUR_WHITE
#define CANE_COLOUR_WARN CANE_COLOUR_BLUE
#define CANE_COLOUR_FAIL CANE_COLOUR_RED
#define CANE_COLOUR_OKAY CANE_COLOUR_GREEN
#define CANE_COLOUR_EXPR CANE_COLOUR_MAGENTA
#define CANE_COLOUR_FUNC CANE_COLOUR_BLUE
#define CANE_COLOUR_HERE CANE_COLOUR_YELLOW

// Logging
#define CANE_LOG_KINDS \
	X(Info, ".", "INFO", CANE_COLOUR_INFO) \
	X(Warn, "*", "WARN", CANE_COLOUR_WARN) \
	X(Fail, "!", "FAIL", CANE_COLOUR_FAIL) \
	X(Okay, "^", "OKAY", CANE_COLOUR_OKAY) \
	X(Expr, "=", "EXPR", CANE_COLOUR_EXPR) \
	X(Func, ">", "FUNC", CANE_COLOUR_FUNC) \
	X(Here, "/", "HERE", CANE_COLOUR_HERE)

#define X(x, y, z, w) x,
	enum class LogKind {
		CANE_LOG_KINDS
	};
#undef X

#define X(x, y, z, w) CANE_CSTR(y),
	inline std::array LOG_KIND_TO_STR = { CANE_LOG_KINDS };
#undef X

#define X(x, y, z, w) CANE_CSTR(z),
	inline std::array LOG_KIND_TO_STR_HUMAN = { CANE_LOG_KINDS };
#undef X

#define X(x, y, z, w) CANE_CSTR(w),
	inline std::array LOG_KIND_TO_STR_COLOUR = { CANE_LOG_KINDS };
#undef X

	constexpr std::string_view log_kind_to_str(LogKind x) {
		return LOG_KIND_TO_STR[static_cast<size_t>(x)];
	}

	constexpr std::string_view log_kind_to_str_human(LogKind x) {
		return LOG_KIND_TO_STR_HUMAN[static_cast<size_t>(x)];
	}

	constexpr std::string_view log_kind_to_str_colour(LogKind x) {
		return LOG_KIND_TO_STR_COLOUR[static_cast<size_t>(x)];
	}

	inline std::ostream& operator<<(std::ostream& os, LogKind log) {
		return (os << log_kind_to_str_human(log));
	}

	////////////
	// Logger //
	////////////

	struct LogInfo {
		std::string_view file;
		std::string_view line;
		std::string_view func;
	};

	namespace detail {
		// Case where we have a format string and additional argument.
		template <typename T, typename... Ts>
		inline decltype(auto)
		log_fmt(std::ostream& os, std::string_view fmt, T&& arg, Ts&&... args) {
			// TODO: C++26 has std::runtime_format
			std::print(
				os, "{}", std::vformat(fmt, std::make_format_args(arg, args...))
			);
		}

		// First argument is not a string but we still want to print it so we
		// supply default format string.
		template <typename T>
		inline decltype(auto) log_fmt(std::ostream& os, T&& arg) {
			std::print(os, "{}", std::forward<T>(arg));
		}
	}  // namespace detail

	template <typename... Ts>
	inline decltype(auto)
	log(std::ostream& os,
		LogKind kind,
		std::optional<LogInfo> info,
		Ts&&... args) {
		std::print(
			os,
			CANE_BOLD "{}[{}]" CANE_RESET
					  " "
					  "{}[{}]" CANE_RESET,
			log_kind_to_str_colour(kind),
			log_kind_to_str(kind),
			log_kind_to_str_colour(kind),
			log_kind_to_str_human(kind)
		);

		if (info.has_value()) {
			auto [filename, line, func] = info.value();

			auto path = std::filesystem::relative(filename).native();

			// Check if any combination of filename or line is empty.
			if (not filename.empty() && not line.empty()) {
				std::print(os, " [{}:{}]", path, line);
			}

			else if (not filename.empty() && line.empty()) {
				std::print(os, " [{}]", path);
			}

			else if (filename.empty() && not line.empty()) {
				std::print(os, " [{}]", line);
			}

			// Check if function name is empty.
			if (not func.empty()) {
				if (func == "operator()") {
					func = "<lambda>";
				}

				std::print(os, " `{}`", func);
			}
		}

		if constexpr (sizeof...(Ts) > 0) {
			std::print(os, ": ");
			detail::log_fmt(os, std::forward<Ts>(args)...);
		}

		std::print(os, "\n");
	}

	template <typename... Ts>
	inline decltype(auto)
	log(LogKind kind, std::optional<LogInfo> info, Ts&&... args) {
		return log(std::cerr, kind, info, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline decltype(auto) log(LogKind kind, Ts&&... args) {
		return log(std::cerr, kind, std::nullopt, std::forward<Ts>(args)...);
	}

	template <typename T>
	inline decltype(auto) inspect(
		std::ostream& os,
		std::optional<LogInfo> info,
		std::string_view expr_str,
		T&& expr
	) {
		log(os, LogKind::Expr, info, "{} = {}", expr_str, expr);
		return expr;
	}

// Log with file location info included.
#define CANE_LOG(...) \
	do { \
		[CANE_VAR(func) = CANE_LOCATION_FUNC]<typename... CANE_VAR(Ts)>( \
			cane::LogKind CANE_VAR(kind), CANE_VAR(Ts)&&... CANE_VAR(args) \
		) { \
			cane::log( \
				std::cerr, \
				CANE_VAR(kind), \
				cane::LogInfo { \
					CANE_LOCATION_FILE, CANE_LOCATION_LINE, CANE_VAR(func) }, \
				std::forward<CANE_VAR(Ts)>(CANE_VAR(args))... \
			); \
		}(__VA_ARGS__); \
	} while (0)

// Unfortunately these convenience macros require at least one argument due to
// quirk of __VA_ARGS__.
#define CANE_INFO(...) \
	do { \
		CANE_LOG(cane::LogKind::Info, __VA_ARGS__); \
	} while (0)

#define CANE_WARN(...) \
	do { \
		CANE_LOG(cane::LogKind::Warn, __VA_ARGS__); \
	} while (0)

#define CANE_FAIL(...) \
	do { \
		CANE_LOG(cane::LogKind::Fail, __VA_ARGS__); \
	} while (0)

#define CANE_OKAY(...) \
	do { \
		CANE_LOG(cane::LogKind::Okay, __VA_ARGS__); \
	} while (0)

// Log entry to a function
#define CANE_FUNC() \
	do { \
		CANE_LOG(cane::LogKind::Func); \
	} while (0)

// Evaluate expression and return its result while also printing both. (Useful
// for debugging something inside a complex expression without needing to create
// temporary variables)
#define CANE_INSPECT(expr) \
	[&, CANE_VAR(func) = CANE_LOCATION_FUNC]() { \
		return cane::inspect( \
			std::cerr, \
			cane::LogInfo { \
				CANE_LOCATION_FILE, CANE_LOCATION_LINE, CANE_VAR(func) }, \
			CANE_STR((expr)), \
			(expr) \
		); \
	}()

// The printf debuggers dream
#define CANE_WHEREAMI() \
	do { \
		CANE_LOG( \
			cane::LogKind::Here, \
			CANE_COLOUR_MAGENTA \
			"Y" CANE_COLOUR_RED "O" CANE_COLOUR_YELLOW "U" CANE_RESET \
			" " CANE_COLOUR_GREEN "A" CANE_COLOUR_BLUE "R" CANE_COLOUR_MAGENTA \
			"E" CANE_RESET " " CANE_COLOUR_MAGENTA "H" CANE_COLOUR_RED \
			"E" CANE_COLOUR_RED "R" CANE_COLOUR_YELLOW "E" CANE_RESET \
		); \
	} while (0)

}  // namespace cane

#endif
