#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		// TODO: Take bpm from commandline args
		cane::Environment env = { .bpm = 120 };

		cane::Parser parser { argv[1] };

		auto root = parser.parse();

		cane::pass_print(root);
		cane::pass_semantic_analysis(root);
		cane::pass_print(root);

		CANE_OKAY("valid!");

		auto value = cane::pass_evaluator(env, root);
		std::println("{}", value);

		CANE_OKAY("done!");
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
