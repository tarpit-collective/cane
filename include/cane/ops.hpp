#ifndef CANE_OPS_HPP
#define CANE_OPS_HPP

namespace cane {

constexpr Event operator!(Event x) {
	x.kind = !x.kind;
	return x;
}

constexpr Event operator&(Event lhs, Event rhs) {
	return { static_cast<uint8_t>(lhs.kind & rhs.kind) };
}

constexpr Event operator|(Event lhs, Event rhs) {
	return { static_cast<uint8_t>(lhs.kind | rhs.kind) };
}

constexpr Event operator^(Event lhs, Event rhs) {
	return { static_cast<uint8_t>(lhs.kind ^ rhs.kind) };
}

constexpr bool operator==(Event lhs, Event rhs) {
	return lhs.kind == rhs.kind;
}

constexpr bool operator!=(Event lhs, Event rhs) {
	return not(lhs == rhs);
}

// Identifies repeating pattern in a sequence
// and attempts to minify it so we don't spam
// the stdout for large sequences.
inline decltype(auto) sequence_minify(Sequence seq) {
	for (size_t f = 1; f != seq.size(); ++f) {
		if (seq.size() % f != 0)
			continue;

		bool all_eq = false;
		for (size_t i = 1; i != (seq.size() / f); ++i) {
			all_eq = std::equal(seq.begin(), seq.begin() + f, seq.begin() + (f * i));

			if (not all_eq)
				break;
		}

		if (all_eq) {
			seq.erase(seq.begin() + f, seq.end());
			return seq;
		}
	}

	return seq;
}

inline decltype(auto) sequence_repeat(Sequence seq, size_t n = 1) {
	// Copy sequence N times to the end of itself.
	// turns i.e. `[a b c]` where N=3 into `[a b c a b c a b c]`.

	if (n == 0)
		return seq;

	size_t count = seq.size();
	seq.reserve(seq.capacity() + n * count);

	while (--n)
		std::copy_n(seq.begin(), count, std::back_inserter(seq));

	return seq;
}

inline decltype(auto) sequence_reverse(Sequence seq) {
	std::reverse(seq.begin(), seq.end());
	return seq;
}

inline decltype(auto) sequence_rotl(Sequence seq, size_t n = 1) {
	std::rotate(seq.begin(), seq.begin() + (n % seq.size()), seq.end());
	return seq;
}

inline decltype(auto) sequence_rotr(Sequence seq, size_t n = 1) {
	std::rotate(seq.rbegin(), seq.rbegin() + (n % seq.size()), seq.rend());
	return seq;
}

inline decltype(auto) sequence_invert(Sequence seq) {
	std::transform(seq.begin(), seq.end(), seq.begin(), std::logical_not<>{});
	return seq;
}

inline decltype(auto) sequence_cat(Sequence lhs, Sequence rhs) {
	lhs.insert(lhs.end(), rhs.begin(), rhs.end());
	return lhs;
}

inline decltype(auto) sequence_or(Sequence lhs, Sequence rhs) {
	std::transform(rhs.cbegin(), rhs.cend(), lhs.begin(), lhs.begin(), std::bit_or<>{});
	return lhs;
}

inline decltype(auto) sequence_and(Sequence lhs, Sequence rhs) {
	std::transform(rhs.cbegin(), rhs.cend(), lhs.begin(), lhs.begin(), std::bit_and<>{});
	return lhs;
}

inline decltype(auto) sequence_xor(Sequence lhs, Sequence rhs) {
	std::transform(rhs.cbegin(), rhs.cend(), lhs.begin(), lhs.begin(), std::bit_xor<>{});
	return lhs;
}

// Generate a new sequence using the first element of another sequence.
inline decltype(auto) sequence_car(Sequence seq) {
	auto it = seq.begin();

	seq.insert(seq.begin(), *it);
	seq.erase(seq.begin() + 1, seq.end());

	return seq;
}

// Generate a new sequence with everything but the first element.
inline decltype(auto) sequence_cdr(Sequence seq) {
	if (seq.size() > 1)
		seq.erase(seq.begin());

	return seq;
}

inline decltype(auto) sequence_len(const Sequence& seq) {
	return seq.size();
}

inline decltype(auto) sequence_beats(const Sequence& seq) {
	return std::count_if(seq.begin(), seq.end(), [] (auto& x) {
		return x.kind == BEAT;
	});
}

inline decltype(auto) sequence_skips(const Sequence& seq) {
	return std::count_if(seq.begin(), seq.end(), [] (auto& x) {
		return x.kind == SKIP;
	});
}

}

#endif
