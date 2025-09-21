#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	cane_logger_t log = (cane_logger_t){
		.name = "toplevel",
		.dest = stderr,
		.level = CANE_PRIORITY_INFO,
		.indent = 0,
	};

	return 0;
}
