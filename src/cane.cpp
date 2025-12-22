#include <cane/cane.hpp>

int main(int, const char* argv[]) {
	try {
		cane::Configuration cfg = { .bpm = 120, .channel_bindings = {
			{ "drums", 10 },
			{ "303", 12 },
		}, };

		auto src = cane::read_file(argv[1]);

		cane::Parser parser { src };
		auto root = parser.parse();

		if (not root.has_value()) {
			return EXIT_FAILURE;
		}

		root = cane::pipeline(cfg, root.value(), cane::pass_validate);
		// root = cane::pipeline(cfg, root.value(), cane::pass_print);
		// root = cane::pipeline(cfg, root.value(), cane::pass_type_resolution);
		root = cane::pipeline(cfg, root.value(), cane::pass_binding_resolution);
		// root = cane::pipeline(cfg, root.value(),
		// cane::pass_binding_resolution); root = cane::pipeline(cfg,
		// root.value(), cane::pass_trace); root = cane::pipeline(cfg,
		// root.value(), cane::pass_validate);
		// root = cane::pipeline(cfg, root.value(), cane::pass_print);

		// root = cane::pipeline(cfg, root.value(), cane::pass_type_resolution);
		root = cane::pipeline(cfg, root.value(), cane::pass_print);

		// auto seq =
		// 	cane::debug_parse_and_compile(src, cfg, cane::pass_type_resolution);

		// if (not seq.has_value()) {
		// 	cane::report(cane::ReportKind::Generic, "empty file");
		// }

		// for (auto e: seq.value()) {
		// 	std::println("{}", e);
		// }
	}

	catch (const cane::Fatal& e) {
		std::cerr << e.what();
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}
