
#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	CANE_UNUSED(argc, argv);

	// cane_string_view_t sv = CANE_SV("!... !...");
	// cane_string_view_t sv = CANE_SV("(\\x 16 + 24) . 123");
	// cane_string_view_t sv = CANE_SV("{1 + 2, {3 * 4}, 5 - {{6}}}");
	// cane_string_view_t sv = CANE_SV("{1, 2, 3, 4, 5, 6, 7, 8, 9}");
	// cane_string_view_t sv = CANE_SV("(1 + 2\n)");
	// cane_string_view_t sv = CANE_SV("(1 . 2) < 3");

	// // cane_string_view_t sv = CANE_SV("\\x `number x `number");

	// cane_ast_node_t* root = cane_parse(sv);

	// cane_pass_semantic_analysis(root);

	// cane_pass_graphviz(root, CANE_SV("cane.dot"));
	// cane_pass_print(root);

	cane_vector_t vec1 = cane_vector_create();

	for (int i = 0; i < 0x2000; i++) {
		CANE_LOG_INFO("%d, cap = %z, len = %z", i, vec1.capacity, vec1.length);
		cane_vector_push(&vec1, i);
	}

	cane_vector_t vec2 = cane_vector_create();

	for (int i = 0; i < 3; i++) {
		cane_vector_push(&vec2, i * 2);
	}

	cane_vector_cat(&vec1, &vec2);

	for (int i = 0; i < 6; i++) {
		CANE_LOG_OKAY("%u", vec1.data[i]);
	}

	// for (int i = 0; i < 3; i++) {
	// 	CANE_LOG_OKAY("%u", cane_vector_pop(&vec));
	// }
	//
	//

	cane_vector_free(&vec1);

	return 0;
}
