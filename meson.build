# https://mesonbuild.com/Builtin-options.html

project(
  'cane',
  'cpp',

  license: 'AGPL-3.0-only',
  version: '1.0',

  default_options: [
    'cpp_std=c++26',
    'buildtype=release',
    'strip=true',
    'warning_level=2',  # 3=pedantic
    'b_ndebug=if-release',
  ]
)

cane_name = meson.project_name()
cane_version = meson.project_version()

cane_description = 'MIDI Sequencing Language'

cane_hdr = files(
  'include/cane/cane.hpp',
)

cane_src = files(
  'src/cane.cpp',
)

cane_inc = include_directories('include/')

cane_deps = []  # Libraries
cane_opts = []  # Build options

meson_opts = []  # Meson options

# Set error limits.
if meson.get_compiler('cpp').get_id() == 'clang'
	add_project_arguments('-ferror-limit=2', language: 'cpp')
	add_project_arguments('-Wno-unused-function', language: 'cpp')

elif meson.get_compiler('cpp').get_id() == 'gcc'
	add_project_arguments('-fmax-errors=2', language: 'cpp')
	add_project_arguments('-Wno-unused-function', language: 'cpp')
endif

# Sanitizers.
if get_option('sanitizers')
	meson_opts += 'b_sanitize=address,undefined'
	cane_deps += meson.get_compiler('cpp').find_library('asan', required: true)
endif

# Build library
cane = executable(
	'cane',
	cane_src,

	include_directories: cane_inc,
	dependencies: cane_deps,

	override_options: meson_opts,
	c_args: cane_opts,

	install: true,
)

# Unit tests
if not meson.is_subproject()
	cane_tests = {
	  'pass': { 'path': 'tests/pass.cpp', 'environment': [], 'arguments': [], 'should_fail': false },
	  'fail': { 'path': 'tests/fail.cpp', 'environment': [], 'arguments': [], 'should_fail': true },

	  'die': { 'path': 'tests/die.cpp', 'environment': [], 'arguments': [], 'should_fail': true },
	  'assert': { 'path': 'tests/assert.cpp', 'environment': [], 'arguments': [], 'should_fail': true },
	}

	foreach name, fields: cane_tests
		path = fields['path']
		should_fail = fields['should_fail']
		environment = fields['environment']
		arguments = fields['arguments']

		exe = executable(
			name,
			path,

			include_directories: cane_inc,
			dependencies: cane_deps,

			override_options: meson_opts,
			c_args: cane_opts,

			install: false,
		)

		test(
			name,
			exe,

			depends: cane,

			should_fail: should_fail,
			verbose: false,

			args: arguments,
			env: environment,
		)
	endforeach
endif
