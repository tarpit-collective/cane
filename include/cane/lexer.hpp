#ifndef CANE_LEXER_HPP
#define CANE_LEXER_HPP

namespace cane {

struct Lexer {
	Context& ctx;

	View original {};
	View src {};

	Token peek {};
	Token prev {};

	constexpr Lexer(cane::View src_, Context& ctx_):
		ctx(ctx_), original(src_), src(src_) {}

	template <typename F, typename... Ts>
	inline void expect(Context& ctx, F&& fn, View sv, Ts&&... args) {
		if (fn(peek))
			return;

		std::ostringstream ss;
		fmt(ss, std::forward<Ts>(args)...);

		ctx.handler(HandlerKind::Error, Phases::SYNTACTIC, original, sv, ss.str());

		throw Error {};
	}

	template <typename... Ts>
	[[noreturn]] inline void error(Context& ctx, Phases phase, View sv, Ts&&... args) {
		std::ostringstream ss;
		fmt(ss, std::forward<Ts>(args)...);
		ctx.handler(HandlerKind::Error, phase, original, sv, ss.str());
		throw Error {};
	}

	template <typename... Ts>
	inline void warning(Context& ctx, Phases phase, View sv, Ts&&... args) {
		std::ostringstream ss;
		fmt(ss, std::forward<Ts>(args)...);
		ctx.handler(HandlerKind::Warning, phase, original, sv, ss.str());
	}

	template <typename... Ts>
	inline void notice(Context& ctx, Phases phase, View sv, Ts&&... args) {
		std::ostringstream ss;
		fmt(ss, std::forward<Ts>(args)...);
		ctx.handler(HandlerKind::Notice, phase, original, sv, ss.str());
	}

	inline Token next() {
		Token tok {};

		auto& [sbegin, send] = src;
		auto& [view, kind] = tok;
		auto& [begin, end] = view;

		// Skip whitespace.
		View ws = cane::take_while(src, [] (View sv) {
			return cane::is_whitespace(decode(sv));
		});

		view = cane::peek(src);

		if (src.empty()) {
			kind = Symbols::TERMINATOR;
		}

		else if (cane::peek(src) == "#"_sv) {
			view = cane::take_while(src, [] (View sv) {
				return sv != "\n"_sv;
			});

			return next();
		}

		else if (view == "("_sv) { kind = Symbols::LPAREN; src = cane::next(src); }
		else if (view == ")"_sv) { kind = Symbols::RPAREN; src = cane::next(src); }

		else if (view == "!"_sv) { kind = Symbols::BEAT; src = cane::next(src); }
		else if (view == "."_sv) { kind = Symbols::SKIP; src = cane::next(src); }
		else if (view == ":"_sv) { kind = Symbols::SEP;  src = cane::next(src); }

		else if (view == "?"_sv) { kind = Symbols::DBG;    src = cane::next(src); }
		else if (view == "'"_sv) { kind = Symbols::REV;    src = cane::next(src); }
		else if (view == "|"_sv) { kind = Symbols::OR;     src = cane::next(src); }
		else if (view == "^"_sv) { kind = Symbols::XOR;    src = cane::next(src); }
		else if (view == ","_sv) { kind = Symbols::CAT;    src = cane::next(src); }

		else if (view == "+"_sv) { kind = Symbols::ADD; src = cane::next(src); }
		else if (view == "-"_sv) { kind = Symbols::SUB; src = cane::next(src); }
		else if (view == "/"_sv) { kind = Symbols::DIV; src = cane::next(src); }
		else if (view == "&"_sv) { kind = Symbols::AND; src = cane::next(src); }

		else if (view == "<"_sv) { kind = Symbols::ROTL; src = cane::next(src); }
		else if (view == ">"_sv) { kind = Symbols::ROTR; src = cane::next(src); }

		else if (view == "@"_sv)  { kind = Symbols::BPM;  src = cane::next(src); }
		else if (view == "$"_sv)  { kind = Symbols::WITH; src = cane::next(src); }
		else if (view == "\\"_sv) { kind = Symbols::REF;  src = cane::next(src); }

		else if (view == "~"_sv) {
			kind = Symbols::INVERT;
			src = cane::next(src);

			if (cane::peek(src) == ">"_sv) {
				kind = Symbols::SEND;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == "*"_sv) {
			kind = Symbols::MUL;
			src = cane::next(src);

			if (cane::peek(src) == "*"_sv) {
				kind = Symbols::REP;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (view == "="_sv) {
			kind = Symbols::SUS;
			src = cane::next(src);

			if (cane::peek(src) == ">"_sv) {
				kind = Symbols::CHAIN;
				view = encompass(view, cane::peek(src));
				src = cane::next(src);
			}
		}

		else if (cane::is_number(decode(cane::peek(src)))) {
			kind = Symbols::INT;
			view = cane::take_while(src, [] (View sv) {
				return cane::is_number(decode(sv));
			});
		}

		else if (cane::is_letter(decode(cane::peek(src))) or view == "_"_sv) {
			kind = Symbols::IDENT;
			view = cane::take_while(src, [] (View sv) {
				return cane::is_alphanumeric(decode(sv)) or sv == "_"_sv;
			});

			if      (view == "map"_sv)    kind = Symbols::MAP;
			if      (view == "mapall"_sv) kind = Symbols::MAPALL;
			else if (view == "vel"_sv)    kind = Symbols::VEL;
			else if (view == "len"_sv)    kind = Symbols::LEN_OF;
			else if (view == "let"_sv)    kind = Symbols::LET;
			else if (view == "pat"_sv)    kind = Symbols::PAT;
			else if (view == "car"_sv)    kind = Symbols::CAR;
			else if (view == "cdr"_sv)    kind = Symbols::CDR;
			else if (view == "up"_sv)     kind = Symbols::UP;
			else if (view == "down"_sv)   kind = Symbols::DOWN;
			else if (view == "fill"_sv)   kind = Symbols::FILL;
		}

		// If the kind is still NONE by this point, we can assume we didn't find
		// a valid token. The reason this check is disconnected from the above
		// if-else chain is because some checks are nested and only fail after
		// succeeding with the original check so we wouldn't fall through if this
		// check was connected.
		if (kind == Symbols::NONE)
			error(ctx, Phases::LEXICAL, view, STR_UNKNOWN_CHAR, view);

		Token out = peek;

		prev = peek;
		peek = tok;

		return out;
	}
};

}

#endif
