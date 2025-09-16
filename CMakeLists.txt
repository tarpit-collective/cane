cmake_minimum_required(VERSION 3.5)
project(cane CXX)

add_executable(cane src/cane.cpp)
target_compile_features(cane PRIVATE cxx_std_20)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/deps/sanitizers-cmake/cmake" ${CMAKE_MODULE_PATH})
find_package(Sanitizers)
add_sanitizers(cane)

target_include_directories(cane PUBLIC deps/conflict/include)
target_include_directories(cane PUBLIC deps/fmt/include)
target_include_directories(cane PUBLIC deps/libremidi/include)
target_include_directories(cane PUBLIC deps/commandline/src/)
target_include_directories(cane PUBLIC include)

add_subdirectory(deps/libremidi/)
target_link_libraries(cane PRIVATE libremidi)

add_subdirectory(deps/fmt/)
target_link_libraries(cane PRIVATE fmt)

add_subdirectory(deps/commandline/)
target_link_libraries(cane PRIVATE commandline)

target_compile_options(cane PRIVATE
	$<$<CXX_COMPILER_ID:MSVC>:/W4>
	$<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic>
)
