#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	// cane_string_view_t sv = CANE_SV("!... !...");
	// cane_string_view_t sv = CANE_SV("(\\x 16 + 24) . 123");
	// cane_string_view_t sv = CANE_SV("{1 + 2, {3 * 4}, 5 - {{6}}}");
	cane_string_view_t sv = CANE_SV("{1, 2, 3, 4, 5, 6, 7, 8, 9}");

	cane_ast_node_t* root = cane_parse(sv);
	cane_pass_print(root);

	return 0;
}
