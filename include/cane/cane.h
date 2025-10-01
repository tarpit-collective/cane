#ifndef CANE_H
#define CANE_H

#include <stdbool.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/lex.h>
#include <cane/parse.h>

//////////////////////
// AST Printer Pass //
//////////////////////

static void cane_pass_print(cane_ast_node_t* node, cane_logger_t log) {
	if (node == NULL) {
		return;
	}

	cane_symbol_kind_t kind = node->kind;
	cane_string_view_t sv = node->sv;

	const char* sv_begin = sv.begin;
	const char* sv_end = sv.end;

	int sv_length = cane_ptrdiff(sv_begin, sv_end);

	cane_ast_node_t* lhs = node->lhs;
	cane_ast_node_t* rhs = node->rhs;

	switch (kind) {
		// Literals
		case CANE_SYMBOL_CONST:
		case CANE_SYMBOL_PARAM:

		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER:

		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST: {
			CANE_LOG_OKAY(
				log, "%s %.*s", CANE_SYMBOL_TO_STR[kind], sv_length, sv_begin
			);
		} break;

		// Unary
		case CANE_SYMBOL_ABS:
		case CANE_SYMBOL_NEG:

			// case CANE_SYMBOL_TIMEMUL:
			// case CANE_SYMBOL_TIMEDIV:

		case CANE_SYMBOL_INVERT:
		case CANE_SYMBOL_REVERSE: {
			// Unary nodes only have a `rhs` child.
			CANE_LOG_OKAY(log, "%s", CANE_SYMBOL_TO_STR[kind]);

			cane_logger_t indented_log = log;
			indented_log.indent++;

			cane_pass_print(rhs, indented_log);
		} break;

		// Binary
		case CANE_SYMBOL_ADD:
		case CANE_SYMBOL_SUB:
		case CANE_SYMBOL_MUL:
		case CANE_SYMBOL_DIV:

		case CANE_SYMBOL_LCM:
		case CANE_SYMBOL_GCD:

		case CANE_SYMBOL_RHYTHM:
		case CANE_SYMBOL_MAP:
		case CANE_SYMBOL_REPEAT:

		case CANE_SYMBOL_LSHIFT:
		case CANE_SYMBOL_RSHIFT:

		case CANE_SYMBOL_OR:
		case CANE_SYMBOL_XOR:
		case CANE_SYMBOL_AND:

		case CANE_SYMBOL_CALL:
		case CANE_SYMBOL_CONCATENATE: {
			CANE_LOG_OKAY(log, "%s", CANE_SYMBOL_TO_STR[kind]);

			cane_logger_t indented_log = log;
			indented_log.indent++;

			cane_pass_print(lhs, indented_log);
			cane_pass_print(rhs, indented_log);
		} break;

		// Complex Nodes
		case CANE_SYMBOL_ASSIGN: {
			CANE_LOG_OKAY(log, "%s", CANE_SYMBOL_TO_STR[kind]);

			cane_logger_t indented_log = log;
			indented_log.indent++;

			cane_pass_print(lhs, indented_log);
			cane_pass_print(rhs, indented_log);
		} break;

		case CANE_SYMBOL_FUNCTION: {
			CANE_LOG_OKAY(log, "%s", CANE_SYMBOL_TO_STR[kind]);

			cane_logger_t indented_log = log;
			indented_log.indent++;

			cane_pass_print(lhs, indented_log);
			cane_pass_print(rhs, indented_log);

		} break;

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER:
		case CANE_SYMBOL_STATEMENT: {
			CANE_DIE(
				cane_logger_create_default(),
				"unimplemented %s",
				CANE_SYMBOL_TO_STR[kind]
			);
		} break;

		default: {
			CANE_DIE(
				cane_logger_create_default(),
				"unimplemented %s",
				CANE_SYMBOL_TO_STR[kind]
			);
		} break;
	}
}

#endif
