#ifndef CANE_VIEW_HPP
#define CANE_VIEW_HPP

namespace cane {
	namespace detail {
		// Find the length of a null terminated string.
		// The reason for this function's existence is the fact
		// that the standard provided std::strlen is not constexpr.
		constexpr size_t length(const char* const str) {
			const char *s = str;
			while (*s) ++s;
			return s - str;
		}
	}

	struct View {
		const char* begin = nullptr;
		const char* end   = nullptr;

		constexpr View() {}

		constexpr View(const char* begin_, const char* end_):
			begin(begin_), end(end_) {}

		constexpr View(const char* ptr):
			begin(ptr), end(ptr + detail::length(ptr)) {}

		operator std::string()      const { return { begin, size() }; }
		operator std::string_view() const { return { begin, size() }; }

		// Return size by getting the absolute difference between
		// begin and end pointers.
		[[nodiscard]] constexpr size_t size() const {
			return
				((end - begin) * (end > begin)) +  // end > begin => end - begin
				((begin - end) * (begin > end));   // begin > end => begin - end
		}

		// If the size of the view is 0, it means the pointers
		// are equal and so we are at the end.
		[[nodiscard]] constexpr bool empty() const {
			return begin == end;
		}
	};

	using cp = uint32_t;

	// Check if 2 Views are equal.
	// We perform a series of checks ranging from least
	// expensive to most expensive.
	// 1. Compare pointers
	// 2. Compare lengths
	// 3. Compare characters
	constexpr bool operator==(View lhs, View rhs) {
		// Compare the pointers.
		if (lhs.begin == rhs.begin and lhs.end == rhs.end)
			return true;

		// Compare the length.
		if (lhs.size() != rhs.size())
			return false;

		// Compare every character.
		for (size_t i = 0; i < lhs.size(); i++) {
			if (*(lhs.begin + i) != *(rhs.begin + i))
				return false;
		}

		return true;
	}

	constexpr bool operator!=(View lhs, View rhs) {
		return not (lhs == rhs);
	}

	inline std::ostream& operator<<(std::ostream& os, View v) {
		os.write(v.begin, v.size());
		return os;
	}
}

// Convert a string literal to a View.
// We can determine the size if it's a literal
// because it hasn't decayed and so the length
// is available to us.
constexpr cane::View operator""_sv(const char* const str, size_t n) {
	return { str, str + n };
}

// Hashing function for `View` so that we can insert
// it into unordered_map.
namespace std {
	template <> struct hash<cane::View> {
		constexpr size_t operator()(cane::View v) const {
			return cane::hash_bytes(v.begin, v.end);
		}
	};
}

namespace cane {
	[[nodiscard]] constexpr size_t length(View);
	[[nodiscard]] constexpr bool validate(View);

	[[nodiscard]] constexpr const char* cp_next(const char*);
	[[nodiscard]] constexpr const char* cp_prev(const char*);

	[[nodiscard]] constexpr uint8_t cp_length(const char*);
	[[nodiscard]] constexpr cp cp_decode(const char*, size_t);

	[[nodiscard]] constexpr cp decode(View);

	[[nodiscard]] constexpr View before(View, View);
	[[nodiscard]] constexpr View after(View, View);
	[[nodiscard]] constexpr View encompass(View, View);

	[[nodiscard]] constexpr View peek(View);
	[[nodiscard]] constexpr View next(View);
	[[nodiscard]] constexpr View take(View&);

	template <typename F>
	[[nodiscard]] constexpr View take_while(View&, const F&);

	template <typename F>
	constexpr size_t count(View, const F&);
	constexpr size_t count_lines(View);


	constexpr size_t length(View sv) {
		size_t length = 0;

		for (; not sv.empty(); sv = next(sv))
			length++;

		return length;
	}

	namespace detail {
		// Validate UTF-8 string.
		// https://bjoern.hoehrmann.de/utf-8/decoder/dfa/
		constexpr inline uint8_t INTERNAL_UTF_TABLE__[] = {
			// The first part of the table maps bytes to character classes that
			// to reduce the size of the transition table and create bitmasks.
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
			7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
			8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
			10,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 3, 3,  11,6, 6, 6, 5, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,

			// The second part is a transition table that maps a combination
			// of a state of the automaton and a character class to a state.
			0,  12, 24, 36, 60, 96, 84, 12, 12, 12, 48, 72,  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
			12, 0,  12, 12, 12, 12, 12, 0,  12, 0,  12, 12,  12, 24, 12, 12, 12, 12, 12, 24, 12, 24, 12, 12,
			12, 12, 12, 12, 12, 12, 12, 24, 12, 12, 12, 12,  12, 24, 12, 12, 12, 12, 12, 12, 12, 24, 12, 12,
			12, 12, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12,  12, 36, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12,
			12, 36, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
		};

		constexpr inline auto CANE_UTF_VALID   = 0;
		constexpr inline auto CANE_UTF_INVALID = 1;
	}

	// Validate a UTF-8 encoded string.
	constexpr bool validate(View sv) {
		cp state = detail::CANE_UTF_VALID;

		for (; state != detail::CANE_UTF_INVALID and not sv.empty(); sv = next(sv)) {
			cp type = detail::INTERNAL_UTF_TABLE__[decode(peek(sv))];
			state = detail::INTERNAL_UTF_TABLE__[256 + state + type];
		}

		return state == detail::CANE_UTF_VALID;
	}

	// Efficiently calculate the number of bytes in
	// a UTF-8 encoded codepoint.
	// We use the first byte to determine the
	// number of bytes in the codepoint.
	constexpr uint8_t cp_length(const char* ptr) {
		return 1u + (
			((*ptr & 0b1110'0000) == 0b1100'0000) * 1u +
			((*ptr & 0b1111'0000) == 0b1110'0000) * 2u +
			((*ptr & 0b1111'1000) == 0b1111'0000) * 3u);
	}

	// Return ptr advanced by one codepoint.
	constexpr const char* cp_next(const char* ptr) {
		return ptr + cp_length(ptr);
	}

	// Return ptr reversed by one codepoint.
	constexpr const char* cp_prev(const char* ptr) {
		// Walk backwards one byte at a time while we
		// see continuation bytes.
		// 10xxxxxx = 0x80
		// 01xxxxxx = 0x40
		// We want the first bit to be set but _not_
		// the second bit.
		do
			--ptr;
		while ((*ptr & 0b1000'0000) and not (*ptr & 0b0100'0000));

		return ptr;
	}

	// Decode a codepoint and return it as a br_u32.
	constexpr cp cp_decode(const char* ptr, size_t sz) {
		constexpr auto loop = [] (const char* ptr, size_t sz) {
			// The table below has a row for each of the 4 possible sizes of codepoint.
			// The first column is the size mask
			uint8_t masks[] = {
			//  v-------------------masks--------------------v     |    v----shifts----v
				0b11111111, 0b00000000, 0b00000000, 0b00000000, /* | */  0u,  0u, 0u, 0u, // 1 byte(s)
				0b00011111, 0b00111111, 0b00000000, 0b00000000, /* | */  6u,  0u, 0u, 0u, // 2 byte(s)
				0b00001111, 0b00111111, 0b00111111, 0b00000000, /* | */ 12u,  6u, 0u, 0u, // 3 byte(s)
				0b00000111, 0b00111111, 0b00111111, 0b00111111, /* | */ 18u, 12u, 6u, 0u, // 4 byte(s)
			};

			size_t max_cp_sz = 4u;
			auto row = ((max_cp_sz * 2u) * (sz - 1u)); // Row in the table above to use.

			cp out = 0;

			// Loop through bytes in codepoint and perform 3 operations:
			// 1. We mask out either the starting bytes size specifier _or_ the continuation marker
			// 2. We shift the byte left by `i * 6u`
			// 3. We OR it with `out` to insert the byte in the correct location
			for (size_t i = 0; i != sz; i++)
				out |= (ptr[i] & masks[row + i]) << masks[row + 4u + i];

			return out;
		};

		switch (sz) {
			case 2: return loop(ptr, 2);
			case 3: return loop(ptr, 3);
			case 4: return loop(ptr, 4);
		}

		return loop(ptr, 1);
	}

	constexpr cp decode(View sv) {
		return cp_decode(sv.begin, cp_length(sv.begin));
	}

	// Character iteration.
	constexpr View next(View sv) {
		if (sv.empty())
			return { sv.begin, sv.begin };

		return { cp_next(sv.begin), sv.end };
	}

	constexpr View take(View& sv) {
		if (sv.empty())
			return { sv.begin, sv.begin };

		auto ptr = sv.begin;
		sv = next(sv);
		return { ptr, sv.begin };
	}

	constexpr View peek(View sv) {
		if (sv.empty())
			return { sv.begin, sv.begin };

		return { sv.begin, cp_next(sv.begin) };
	}

	template <typename F>
	constexpr View take_while(View& sv, const F& fn) {
		View out { sv.begin, sv.begin };

		while (not sv.empty() and fn(peek(sv)))
			out = encompass(out, take(sv));

		return out;
	}

	// Return the outer view minus the inner view.
	constexpr View before(View outer, View inner) {
		return { outer.begin, inner.begin };
	}

	constexpr View after(View outer, View inner) {
		return { inner.end, outer.end };
	}

	constexpr View encompass(View lhs, View rhs) {
		return { lhs.begin, rhs.end };
	}

	// Extend `inner` to be a view of the same line it's on.
	constexpr View extend_to_line(View outer, View inner) {
		if (inner.empty()) return inner;

		const auto& [sbegin, send] = outer;
		auto& [begin, end] = inner;

		// Walk backwards through the source until we hit the beginning of the string
		// or we hit the count of lines.
		while (begin >= sbegin and *begin != '\n')
			begin--;

		// Walk forwards through the source until we hit EOF or we hit the count
		// of lines.
		while (end <= send and *end != '\n')
			end++;

		if (*begin == '\n')
			begin++;

		return inner;
	}

	template <typename F> constexpr size_t count(View sv, const F& fn) {
		if (sv.empty()) return 0;

		size_t count = 0;

		while (not sv.empty()) {
			if (fn(peek(sv)))
				count++;

			sv = next(sv);
		}

		return count;
	}

	constexpr size_t count_lines(View sv) {
		return count(sv, [] (View sv) { return sv == "\n"_sv; });
	}

	// Decoders (all of these functions assume correct input)
	constexpr uint64_t b10_decode(View sv) {
		uint64_t n = 0;

		for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 10) + (*ptr - '0');

		return n;
	}

	constexpr uint64_t b16_decode(View sv) {
		uint64_t n = 0;
		sv = cane::next(cane::next(sv));  // skip `0x`

		// Interesting fact about ASCII:
		// A-Z and a-z are the same ranges minus a difference of a single bit.
		// We can exploit this peculiarity to make the ranges line up.
		// If we mask out the single bit that seperates the two ranges, they
		// become the same range. Another way to formulate this is to
		// mask out all but the lower 4 bits. This has the added benefit of
		// moving the ranges (now single range) into the 1-6 range. From
		// here, it's just a matter of adding 9 if the character is >= 'A'.
		for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 16) + (*ptr & 0xf) + (*ptr >> 6) * 9;

		return n;
	}

	constexpr uint64_t b2_decode(View sv) {
		uint64_t n = 0;
		sv = cane::next(cane::next(sv));  // skip `0b`

		for (auto ptr = sv.begin; ptr != sv.end; ++ptr)
		n = (n * 2) + (*ptr - '0');

		return n;
	}
}

#endif

