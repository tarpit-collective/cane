#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		// cane::Parser parser { CANE_CSTR("1 2 3 4") };
		// cane::Parser parser { CANE_CSTR("!.!.!.!.!.; 2 + 3 * 5 / 6; 1 2 3 4")
		// }; cane::Parser parser { CANE_CSTR("!. .!!") };

		cane::Parser parser { argv[1] };

		auto root = parser.parse();

		cane::pass_print(root);
		cane::pass_semantic_analysis(root);
		cane::pass_print(root);

		CANE_OKAY("valid!");

		auto value = cane::pass_evaluator(root);
		std::println("{}", value);

		CANE_OKAY("done!");
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
