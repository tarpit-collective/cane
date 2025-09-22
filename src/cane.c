#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	cane_logger_t log = (cane_logger_t){
		.name = "toplevel",
		.dest = stderr,
		.level = CANE_PRIORITY_OKAY,
		.indent = 0,
	};

	cane_string_view_t sv = CANE_SV("or xor and not + - * / () {} [] <>");
	cane_lexer_t lx = cane_lexer_create(sv);

	cane_symbol_t sym;
	while (sym.kind != CANE_SYMBOL_ENDFILE) {
		cane_lexer_peek(&lx, &sym);
		CANE_LOG_OKAY(
			log,
			"kind = %s, str = %.*s",
			CANE_SYMBOL_KIND_TO_STR[sym.kind],
			cane_ptrdiff(sym.str.begin, sym.str.end),
			sym.str.begin
		);

		cane_lexer_take(&lx, &sym);
	}

	return 0;
}
