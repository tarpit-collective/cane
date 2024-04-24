#ifndef CANE_PRINT_HPP
#define CANE_PRINT_HPP

// I/O
namespace cane {
	template <typename... Ts>
	inline std::ostream& print(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ...);
	}

	template <typename... Ts>
	inline std::ostream& println(std::ostream& os, Ts&&... args) {
		return ((os << std::forward<Ts>(args)), ..., (os << '\n'));
	}

	namespace detail {
		inline std::ostream& fmt(std::ostream& os, View& f) {
			while (not f.empty()) {
				View before       = take_while(f, [] (View sv) { return sv != "%"_sv; });
				View placeholders = take_while(f, [] (View sv) { return sv == "%"_sv; });

				print(os, before);

				for (size_t i = 0; i != placeholders.size() / 2; ++i)
					print(os, "%");

				if (placeholders.size() % 2 == 1)
					print(os, "%");
			}

			return print(os, f);
		}

		template <typename T, typename... Ts>
		inline std::ostream& fmt(std::ostream& os, View& f, T&& arg, Ts&&... args) {
			View before       = take_while(f, [] (View sv) { return sv != "%"_sv; });
			View placeholders = take_while(f, [] (View sv) { return sv == "%"_sv; });

			print(os, before);

			for (size_t i = 0; i != placeholders.size() / 2; ++i)
				print(os, "%");

			if (placeholders.size() % 2 == 1) {
				print(os, arg);
				detail::fmt(os, f, std::forward<Ts>(args)...);
			}

			else
				detail::fmt(os, f, std::forward<T>(arg), std::forward<Ts>(args)...);

			if (f.empty())
				return os;

			return os;
		}
	}

	template <typename... Ts>
	inline std::ostream& fmt(std::ostream& os, View f, Ts&&... args) {
		return detail::fmt(os, f, std::forward<Ts>(args)...);
	}

	template <typename... Ts>
	inline std::ostream& fmtln(std::ostream& os, View f, Ts&&... args) {
		return (fmt(os, f, std::forward<Ts>(args)...) << '\n');
	}
}

#endif

