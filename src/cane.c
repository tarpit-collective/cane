#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	cane_logger_t log = (cane_logger_t){
		.name = "toplevel",
		.dest = stderr,
		.level = CANE_PRIORITY_OKAY,
		.indent = 0,
	};

	cane_string_view_t sv = CANE_SV("!!!");
	// cane_string_view_t sv = CANE_SV("(\\x 16 + 24) . 123");

	cane_ast_node_t* root = cane_parse(sv);
	cane_pass_print(root, cane_logger_create_default());

	return 0;
}
