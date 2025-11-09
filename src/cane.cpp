#include <cane/util.hpp>
#include <cane/log.hpp>
#include <cane/lex.hpp>
#include <cane/parse.hpp>
#include <cane/passes.hpp>

int main(int, const char*[]) {
	try {
		cane::Parser parser { CANE_CSTR("!.!.!.!.!.; 2 + 3 * 5 / 6") };
		// cane::Parser parser { CANE_CSTR("!.!.!.!.") };
		auto root = parser.parse();

		cane::pass_print(root);
		cane::pass_semantic_analysis(root);
		cane::pass_print(root);

		auto value = cane::pass_evaluator(root);

		std::println("value: {}", std::get<cane::Scalar>(value));
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
