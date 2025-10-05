#ifndef CANE_H
#define CANE_H

#include <stdbool.h>

#include <cane/enum.h>
#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/str.h>
#include <cane/lex.h>
#include <cane/parse.h>

//////////////////////
// AST Printer Pass //
//////////////////////

static void cane_pass_print_walker(cane_ast_node_t* node, int depth);

static void cane_pass_print(cane_ast_node_t* node) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());
	cane_pass_print_walker(node, 0);
}

static void cane_pass_print_walker(cane_ast_node_t* node, int depth) {
	if (node == NULL) {
		printf("%*sNULL\n", depth * 4, "");
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
		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER: {
			printf(
				"%*s%s %.*s\n",
				depth * 4,
				"",
				CANE_SYMBOL_TO_STR[kind],
				sv_length,
				sv_begin
			);
		} break;

		// Unary
		case CANE_SYMBOL_ABS:
		case CANE_SYMBOL_NEG:

		case CANE_SYMBOL_INVERT:
		case CANE_SYMBOL_REVERSE: {
			// Unary nodes only have a `rhs` child.
			printf("%*s%s\n", depth * 4, "", CANE_SYMBOL_TO_STR[kind]);
			cane_pass_print_walker(rhs, depth + 1);
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
		case CANE_SYMBOL_CONCATENATE:

		case CANE_SYMBOL_ASSIGN:
		case CANE_SYMBOL_FUNCTION: {
			printf("%*s%s\n", depth * 4, "", CANE_SYMBOL_TO_STR[kind]);

			cane_pass_print_walker(lhs, depth + 1);
			cane_pass_print_walker(rhs, depth + 1);
		} break;

		// Lists
		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST:

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER:
		case CANE_SYMBOL_STATEMENT: {
			printf("%*s%s\n", depth * 4, "", CANE_SYMBOL_TO_STR[kind]);

			cane_pass_print_walker(lhs, depth + 1);
			cane_pass_print_walker(rhs, depth + 1);
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

//////////////////
// AST GraphViz //
//////////////////

static void cane_pass_graphviz_edge(
	FILE* fp, cane_ast_node_t* node, size_t parent, size_t self
) {
	cane_symbol_kind_t kind = CANE_SYMBOL_NONE;
	cane_string_view_t sv = CANE_SV("NULL");

	if (node != NULL) {
		kind = node->kind;
		sv = node->sv;
	}

	cane_string_view_info_t info = cane_string_view_ptr_length(sv);

	fprintf(
		fp,
		"  n%zu [label=\"kind = %s|sv = `%.*s`\"];\n",
		self,
		CANE_SYMBOL_TO_STR_HUMAN[kind],
		(int)info.length,
		info.ptr
	);

	if (parent != self) {
		fprintf(fp, "  n%zu -> n%zu;\n", parent, self);
	}
}

static void cane_pass_graphviz_walker(
	FILE* fp, cane_ast_node_t* node, size_t* id, size_t parent
);

static void
cane_pass_graphviz(cane_ast_node_t* node, cane_string_view_t filename) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	size_t length = cane_string_view_length(filename);

	// TODO: Make this not ugly.
	if (length > 256) {
		CANE_DIE(cane_logger_create_default(), "filename too long");
	}

	char buf[256] = {0};
	memcpy(&buf, filename.begin, length);

	FILE* fp = fopen(buf, "w+");

	size_t id = 0;

	fprintf(fp, "digraph {\n");
	fprintf(fp, "node [shape=Mrecord style=filled fillcolor=\"#bfbfbf\"];\n");
	cane_pass_graphviz_walker(fp, node, &id, 0);
	fprintf(fp, "}\n");
}

static void cane_pass_graphviz_walker(
	FILE* fp, cane_ast_node_t* node, size_t* id, size_t parent
) {
	size_t self = (*id)++;

	if (node == NULL) {
		cane_pass_graphviz_edge(fp, node, parent, self);
		return;
	}

	cane_symbol_kind_t kind = node->kind;

	cane_ast_node_t* lhs = node->lhs;
	cane_ast_node_t* rhs = node->rhs;

	switch (kind) {
		// Literals
		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER: {
			cane_pass_graphviz_edge(fp, node, parent, self);
		} break;

		// Unary
		case CANE_SYMBOL_ABS:
		case CANE_SYMBOL_NEG:

		case CANE_SYMBOL_INVERT:
		case CANE_SYMBOL_REVERSE: {
			cane_pass_graphviz_edge(fp, node, parent, self);
			cane_pass_graphviz_walker(fp, rhs, id, self);
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
		case CANE_SYMBOL_CONCATENATE:

		case CANE_SYMBOL_ASSIGN:
		case CANE_SYMBOL_FUNCTION: {
			cane_pass_graphviz_edge(fp, node, parent, self);
			cane_pass_graphviz_walker(fp, lhs, id, self);
			cane_pass_graphviz_walker(fp, rhs, id, self);
		} break;

		// Lists
		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST:

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER:
		case CANE_SYMBOL_STATEMENT: {
			cane_pass_graphviz_edge(fp, node, parent, self);
			cane_pass_graphviz_walker(fp, lhs, id, self);
			cane_pass_graphviz_walker(fp, rhs, id, self);
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
