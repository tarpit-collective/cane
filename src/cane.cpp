#include <cane/util.hpp>
#include <cane/log.hpp>
#include <cane/lex.hpp>

int main(int, const char*[]) {
	CANE_LOG(cane::LogKind::Okay, "hello {}!", "jack");
	CANE_LOG(cane::LogKind::Warn, "hello {}!", "jack");
	CANE_LOG(cane::LogKind::Fail, "hello {}!", "jack");
	CANE_LOG(cane::LogKind::Info, "hello {}!", "jack");

	CANE_FUNC();

	CANE_INSPECT(123);

	CANE_WHEREAMI();

	return 0;
}
