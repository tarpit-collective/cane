#ifndef CANE_UNICODE_HPP
#define CANE_UNICODE_HPP

namespace cane {
	constexpr bool is_letter(uint32_t c) {
		return is_lu(c) or is_ll(c) or is_lt(c) or is_lm(c) or is_lo(c);
	}

	// things like accents.
	constexpr bool is_mark(uint32_t c) {
		return is_mn(c) or is_mc(c) or is_me(c);
	}

	constexpr bool is_number(uint32_t c) {
		return is_nd(c) or is_nl(c) or is_no(c);
	}

	constexpr bool is_punctuation(uint32_t c) {
		return is_pc(c) or is_pd(c) or is_ps(c) or is_pe(c) or is_pi(c) or is_pf(c) or is_po(c);
	}

	constexpr bool is_symbol(uint32_t c) {
		return is_sm(c) or is_sc(c) or is_sk(c) or is_so(c);
	}

	constexpr bool is_seperator(uint32_t c) {
		return is_zs(c) or is_zl(c) or is_zp(c);
	}

	constexpr bool is_control(uint32_t c) {
		return is_cc(c) or is_cf(c);
	}

	constexpr bool is_other(uint32_t c) {
		return is_cc(c) or is_cf(c) or is_cs(c) or is_co(c);
	}

	constexpr bool is_alphanumeric(uint32_t c) {
		return is_letter(c) or is_number(c);
	}

	constexpr bool is_visible(uint32_t c) {
		return is_alphanumeric(c) or is_symbol(c) or is_punctuation(c);
	}

	constexpr bool is_whitespace(uint32_t c) {
		return is_zs(c) or is_control(c);
	}

}

#endif
