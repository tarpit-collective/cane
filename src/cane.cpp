#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		cane::Configuration config = { .bpm = 120 };

		cane::Parser parser { argv[1] };

		auto root = parser.parse();

		cane::pass_print(root);
		cane::TypeKind type = cane::pass_semantic_analysis(config, root);
		CANE_OKAY("program type = `{}`", type);
		cane::pass_print(root);

		CANE_OKAY("valid!");

		auto value = cane::pass_evaluator(config, root);
		std::println("{}", value);

		if (std::holds_alternative<cane::Pattern>(value)) {
			CANE_OKAY("Events:");
			for (auto& x: value.get_pattern()) {
				std::println("{}", x);
			}
		}
	}

	catch (cane::Fatal e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
