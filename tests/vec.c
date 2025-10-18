
#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	CANE_UNUSED(argc, argv);

	cane_vector_t vec1 = cane_vector_create();

	// for (int i = 0; i < 0x2000; i++) {
	// 	CANE_LOG_INFO("%d, cap = %z, len = %z", i, vec1.capacity, vec1.length);
	// 	cane_vector_push(&vec1, i);
	// }

	// cane_vector_t vec2 = cane_vector_create();

	for (uint8_t i = 1; i <= 3; i++) {
		cane_vector_push(&vec1, i);
	}

	uint8_t expected[3] = {1, 2, 3};

	if (!cane_vector_compare_span(&vec1, 0, 3, expected)) {
		return EXIT_FAILURE;
	}

	uint8_t val;
	for (size_t i = 3; cane_vector_pop(&vec1, &val); i--) {
		CANE_ASSERT(i == val, "expected %zd", i);
	}
	// while (cane_vector_pop(&vec1, &val)) {

	// }

	// cane_vector_cat(&vec1, &vec2);

	// for (int i = 0; i < 6; i++) {
	// 	CANE_LOG_OKAY("%u", vec1.data[i]);
	// }

	cane_vector_free(&vec1);

	return 0;
}
