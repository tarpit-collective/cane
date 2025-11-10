#include <cane/cane.hpp>

int main(int, const char*[]) {
	try {
		// cane::Parser parser { CANE_CSTR("1 2 3 4") };
		// cane::Parser parser { CANE_CSTR("!.!.!.!.!.; 2 + 3 * 5 / 6; 1 2 3 4")
		// };
		cane::Parser parser { CANE_CSTR("!. .!!") };

		auto root = parser.parse();

		cane::pass_print(root);
		cane::pass_semantic_analysis(root);
		cane::pass_print(root);

		auto value = cane::pass_evaluator(root);

		// std::println("value: {}", value.get_melody());

		for (bool x: value.get_rhythm()) {
			std::print("{}", x ? '1' : '0');
		}

		std::println();
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
