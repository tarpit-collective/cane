# https://mesonbuild.com/Builtin-options.html

project(
  'cane',
  'c',

  license: 'AGPL-3.0-only',
  version: '1.0',

  default_options: [
    'c_std=c17',
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
  'include/cane/cane.h',
  'include/cane/def.h',
  'include/cane/log.h',
  'include/cane/util.h',
  'include/cane/str.h',
  'include/cane/lex.h',
  'include/cane/parse.h',
)

cane_src = files(
  'src/cane.c',
)

cane_inc = include_directories('include/')

cane_deps = []  # Libraries
cane_opts = []  # C build options

meson_opts = []  # Meson options

# Set error limits.
if meson.get_compiler('c').get_id() == 'clang'
	add_project_arguments('-ferror-limit=2', language: 'c')

	add_project_arguments('-Wno-unused-function', language: 'c')
	add_project_arguments('-Wno-unused-variable', language: 'c')
	add_project_arguments('-Wno-unused-parameter', language: 'c')

elif meson.get_compiler('c').get_id() == 'gcc'
	add_project_arguments('-fmax-errors=2', language: 'c')

	add_project_arguments('-Wno-unused-function', language: 'c')
	add_project_arguments('-Wno-unused-variable', language: 'c')
	add_project_arguments('-Wno-unused-parameter', language: 'c')
endif

# Sanitizers.
if get_option('sanitizers')
	meson_opts += 'b_sanitize=address,undefined'
	cane_deps += meson.get_compiler('c').find_library('asan', required: true)
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
	  'pass': { 'path': 'tests/pass.c', 'environment': [], 'arguments': [], 'should_fail': false },
	  'fail': { 'path': 'tests/fail.c', 'environment': [], 'arguments': [], 'should_fail': true },

	  'die': { 'path': 'tests/die.c', 'environment': [], 'arguments': [], 'should_fail': true },
	  'assert': { 'path': 'tests/assert.c', 'environment': [], 'arguments': [], 'should_fail': true },
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
