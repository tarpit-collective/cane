#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	CANE_UNUSED(argc, argv);

	// cane_string_view_t sv = CANE_SV("!... !...");
	// cane_string_view_t sv = CANE_SV("(\\x 16 + 24) . 123");
	// cane_string_view_t sv = CANE_SV("{1 + 2, {3 * 4}, 5 - {{6}}}");
	// cane_string_view_t sv = CANE_SV("{1, 2, 3, 4, 5, 6, 7, 8, 9}");
	// cane_string_view_t sv = CANE_SV("(1 + 2\n)");
	// cane_string_view_t sv = CANE_SV("(1 . 2) < 3");
	// cane_string_view_t sv = CANE_SV("\\x `number x `number");

	// cane_string_view_t sv = CANE_SV("[1, 2, 3]");
	// cane_string_view_t sv = CANE_SV("x(1 + 2)");
	// cane_string_view_t sv = CANE_SV("\"foo\"");

	const char* buf = "";
	int len = 0;

	cane_read_file("cane.cn", &buf, &len);

	cane_string_view_t sv;

	sv.begin = buf;
	sv.end = buf + len;

	cane_ast_node_t* root = cane_parse(sv);

	cane_pass_print(root);
	cane_pass_semantic_analysis(root);

	cane_file_t fp = cane_file_open(CANE_SV("cane.dot"));
	cane_pass_graphviz(root, fp);
	cane_file_close(fp);

	cane_value_t v = cane_pass_evaluator(root);

	CANE_LOG_OKAY("%d", v.number);

	return 0;
}
