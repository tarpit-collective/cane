#ifndef CANE_REPORT_HPP
#define CANE_REPORT_HPP

namespace cane {
	#define PHASES \
		X(INTERNAL,  internal) \
		X(ENCODING,  encoding) \
		X(LEXICAL,   lexical) \
		X(SYNTACTIC, syntactic) \
		X(SEMANTIC,  semantic)

		#define X(name, str) name,
			enum class Phases: int { PHASES };
		#undef X

		#define X(name, str) #str##_sv,
			constexpr View PHASE_TO_STRING[] = { PHASES };
		#undef X

		constexpr decltype(auto) phase2str(Phases p) {
			return PHASE_TO_STRING[(int)p];
		}

	#undef PHASES

	inline std::ostream& operator<<(std::ostream& os, Phases p) {
		return (os << phase2str(p));
	}

	#define REPORTS \
		X(ERROR,   error,   CANE_RED) \
		X(WARNING, warning, CANE_BLUE) \
		X(NOTICE,  notice,  CANE_YELLOW)

		#define X(name, str, colour) name,
			enum class Reports: int { REPORTS };
		#undef X

		#define X(name, str, colour) #str##_sv,
			constexpr View REPORT_TO_STRING[] = { REPORTS };
		#undef X

		#define X(name, str, colour) colour,
			constexpr View REPORT_TO_COLOUR[] = { REPORTS };
		#undef X

		constexpr decltype(auto) report2str(Reports r) {
			return REPORT_TO_STRING[(int)r];
		}

		constexpr decltype(auto) report2colour(Reports r) {
			return REPORT_TO_COLOUR[(int)r];
		}

	#undef REPORTS

	inline std::ostream& operator<<(std::ostream& os, Reports r) {
		return (os << report2str(r));
	}

	template <Reports R = Reports::ERROR, typename... Ts>
	inline std::ostream& general_report(std::ostream& os, Ts&&... args) {
		auto highlight = report2colour(R);
		fmt(os, "%" CANE_BOLD "%" CANE_RESET " =>" CANE_RESET " ", highlight, report2str(R));
		return fmtln(os, std::forward<Ts>(args)...);
	}

	struct Error {};

	// Check if an interval overlaps with another.
	template <typename T>
	constexpr bool overlapping_intervals(T a_begin, T a_end, T b_begin, T b_end) {
		return a_begin <= b_end and a_end >= b_begin;
	}

	template <Reports R = Reports::ERROR>
	inline std::ostream& report(
		std::ostream& os,
		Phases phase,
		const View src,
		View sv,
		std::string str
	) {
		if (not overlapping_intervals(src.begin, src.end, sv.begin, sv.end))
			return report(os, Phases::INTERNAL, ""_sv, ""_sv, "`sv` does not exist in the range of `src`"_sv);

		const auto focused_line = extend_to_line(src, sv);

		const auto before = cane::before(focused_line, sv);
		const auto after = cane::after(focused_line, sv);

		const auto column_n = length(before) + 1;
		const auto line_n = count_lines(encompass(src, sv)) + 1;

		auto highlight = report2colour(R);

		fmt(
			os,
			"%" CANE_BOLD "% %: " CANE_RESET "%:% " CANE_BOLD "=>" CANE_RESET " ",
			highlight, phase2str(phase), report2str(R), line_n, column_n
		);

		println(os, str);
		println(os);

		if (sv.empty())
			println(os, "  " CANE_CYAN ">" CANE_RESET "  ", highlight, "EOF", CANE_RESET);

		else {
			View line = cane::take_while(sv, [] (View sv) { return sv != "\n"_sv; });
			View chr = take(sv);

			if (sv.empty())
				println(os, "  " CANE_CYAN ">" CANE_RESET "  ", before, highlight, line);

			else
				println(os, "  " CANE_CYAN "|" CANE_RESET "  ", before, highlight, line);

			while (not sv.empty()) {
				View line = cane::take_while(sv, [] (View sv) { return sv != "\n"_sv; });

				if (not sv.empty()) {
					View chr = take(sv);
					println(os, "  " CANE_CYAN "|" CANE_RESET "  ", highlight, line);
				}

				else
					println(os, "  " CANE_CYAN ">" CANE_RESET "  ", highlight, line, CANE_RESET, after);
			}
		}

		return println(os);
	}

	template <typename... Ts>
	inline std::ostream& report_error(std::ostream& os, Ts&&... args) {
		return report<Reports::ERROR>(os, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& report_warning(std::ostream& os, Ts&&... args) {
		return report<Reports::WARNING>(os, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& report_notice(std::ostream& os, Ts&&... args) {
		return report<Reports::NOTICE>(os, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	[[noreturn]] inline void general_error(Ts&&... args) {
		general_report<Reports::ERROR>(std::cerr, std::forward<Ts>(args)...);
		throw Error {};
	}

	template <typename... Ts>
	inline void general_warning(Ts&&... args) {
		general_report<Reports::WARNING>(std::cerr, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline void general_notice(Ts&&... args) {
		general_report<Reports::NOTICE>(std::cerr, std::forward<Ts>(args)...);
	}
}

#endif
