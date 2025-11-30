#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		cane::Configuration cfg = { .bpm = 120, .channel_bindings = {
			{ "drums", 10 },
			{ "303", 12 },
		}, };

		auto value = cane::debug_parse_and_compile(
			argv[1],
			cfg,
			cane::pass_binding_resolution,
			cane::pass_type_resolution
		);

		if (std::holds_alternative<cane::Sequence>(value)) {
			for (auto e: std::get<cane::Sequence>(value)) {
				std::println("{}", e);
			}
		}
	}

	catch (const cane::Fatal& e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
