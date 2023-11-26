cmake_minimum_required(VERSION 3.26)
project(pacemaker CXX)

find_package(PkgConfig REQUIRED)
pkg_check_modules(JACK REQUIRED jack)

add_executable(pacemaker src/pacemaker.cpp)
target_compile_features(pacemaker PRIVATE cxx_std_20)

target_link_libraries(pacemaker ${JACK_LIBRARIES})
target_include_directories(pacemaker PUBLIC ${JACK_INCLUDE_DIRS})
target_compile_options(pacemaker PUBLIC ${JACK_CFLAGS_OTHER})

target_include_directories(pacemaker PUBLIC deps/conflict/include)
target_include_directories(pacemaker PUBLIC deps/lexy/include)
target_include_directories(pacemaker PUBLIC include)

target_compile_options(pacemaker PRIVATE
	$<$<CXX_COMPILER_ID:MSVC>:/W4>
	$<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-Wall -Wextra -Wpedantic>
)