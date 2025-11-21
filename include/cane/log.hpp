#ifndef CANE_LOG_HPP
#define CANE_LOG_HPP

#include <stdexcept>
#include <iostream>
#include <print>
#include <filesystem>
#include <string_view>

#include <cane/def.hpp>

namespace cane {

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
			cane::LogKind CANE_VAR(kind), CANE_VAR(Ts) &&... CANE_VAR(args) \
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

	////////////
	// Report //
	////////////

	class Fatal: public std::runtime_error {  // Fatal error type that is caught
											  // at the highest level.
		using runtime_error::runtime_error;
	};

	template <typename... Ts>
	[[noreturn]] inline void report(ReportKind kind, Ts&&... args) {
		std::stringstream ss;

		std::print(
			ss,
			CANE_BOLD CANE_COLOUR_FAIL "[{}] {} error: " CANE_RESET,
			log_kind_to_str(LogKind::Fail),
			kind
		);

		detail::log_fmt(ss, std::forward<Ts>(args)...);
		std::print(ss, "\n");

		throw Fatal { ss.str() };
	}

	template <typename... Ts>
	[[noreturn]] inline void die(LogInfo info, Ts&&... args) {
		std::stringstream ss;
		cane::log(ss, LogKind::Fail, info, std::forward<Ts>(args)...);
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

#define CANE_UNIMPLEMENTED() \
	do { \
		cane::die( \
			cane::LogInfo { \
				CANE_LOCATION_FILE, CANE_LOCATION_LINE, CANE_LOCATION_FUNC }, \
			"unimplemented!" \
		); \
	} while (0)

}  // namespace cane

#endif
