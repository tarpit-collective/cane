
#include <cane/cane.h>

int main(int argc, const char* argv[]) {
	CANE_UNUSED(argc, argv);

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

	cane_vector_free(&vec1);

	return 0;
}
