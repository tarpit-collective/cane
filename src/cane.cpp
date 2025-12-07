#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		cane::Configuration cfg = { .bpm = 120, .channel_bindings = {
			{ "drums", 10 },
			{ "303", 12 },
		}, };

		auto seq = cane::debug_parse_and_compile(
			argv[1], cfg, cane::pass_type_resolution
			// cane::pass_binding_resolution,
			// cane::pass_type_resolution
		);

		for (auto e: seq) {
			std::println("{}", e);
		}
	}

	catch (const cane::Fatal& e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
