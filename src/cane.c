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

	// cane_string_view_t sv = CANE_SV("1 + 2 * 3");
	cane_string_view_t sv = CANE_SV("x(1 + 2)");
	cane_ast_node_t* root = cane_parse(sv);

	cane_pass_print(root);
	cane_pass_semantic_analysis(root);

	cane_file_t fp = cane_file_open(CANE_SV("cane.dot"));
	cane_pass_graphviz(root, fp);
	cane_file_close(fp);
	cane_vector_t vec1 = cane_vector_create();

	cane_vector_push(&vec1, 1);
	cane_vector_push(&vec1, 2);
	cane_vector_push(&vec1, 3);

	// cane_vector_remove(&vec1, 3);

	uint8_t buf[] = {4, 5, 6};
	cane_vector_insert_buf(&vec1, 1, buf, sizeof(buf));

	cane_vector_fill_span(&vec1, 1, 3, 9);

	// cane_vec_insert(&vec1, 1, 4);

	for (size_t i = 0; i < vec1.length; i++) {
		CANE_LOG_INFO("%d", vec1.data[i]);
		// cane_vec_push(&vec1, i);
	}

	// cane_vec_t vec2 = cane_vec_create();

	// for (int i = 0; i < 3; i++) {
	// 	cane_vec_push(&vec2, i * 2);
	// }

	// cane_vec_cat(&vec1, &vec2);

	// for (int i = 0; i < 6; i++) {
	// 	CANE_LOG_OKAY("%u", vec1.buf[i]);
	// }

	// for (int i = 0; i < 3; i++) {
	// 	CANE_LOG_OKAY("%u", cane_vec_pop(&vec));
	// }
	//
	//

	cane_vector_free(&vec1);

	return 0;
}
