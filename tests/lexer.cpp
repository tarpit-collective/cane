#include <cane/util.hpp>
#include <cane/log.hpp>
#include <cane/lex.hpp>
#include <cane/parse.hpp>
#include <cane/passes.hpp>

int main(int, const char*[]) {
	try {
		cane::Lexer lx { CANE_CSTR("123 456") };

		do {
			auto symbol = lx.take_opt();

			if (symbol.has_value()) {
				auto [kind, sv] = symbol.value();
				std::println(
					stderr, "{} {}", cane::symbol_kind_to_str_human(kind), sv
				);
			}

			else {
				std::println(stderr, "no value");
			}
		} while (not lx.peek_is_kind(cane::SymbolKind::EndFile));
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
