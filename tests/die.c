#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	CANE_UNUSED(argc, argv);

	cane_logger_t log = (cane_logger_t){
		.name = "toplevel",
		.dest = NULL,
		.level = CANE_PRIORITY_INFO,
		.indent = 0,
	};

	CANE_DIE(log, "die");
}
