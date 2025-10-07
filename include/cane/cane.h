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

	cane_type_kind_t type = CANE_TYPE_NONE;

	if (node != NULL) {
		kind = node->kind;
		sv = node->sv;

		type = node->type;
	}

	cane_string_view_info_t info = cane_string_view_ptr_length(sv);

	fprintf(
		fp,
		"  n%zu [label=\"kind = %s|sv = `%.*s`|type = %s\"];\n",
		self,
		CANE_SYMBOL_TO_STR_HUMAN[kind],
		(int)info.length,
		info.ptr,
		CANE_TYPE_KIND_TO_STR_HUMAN[type]
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
	// Try taking file pointer directly as an argument and wrapping fopen
	if (length > 256) {
		CANE_DIE(cane_logger_create_default(), "filename too long");
	}

	char buf[256] = {0};
	memcpy(&buf, filename.begin, length);

	FILE* fp = fopen(buf, "w+");

	size_t id = 0;

	fprintf(fp, "digraph {\n");
	fprintf(fp, "  node [shape=Mrecord style=filled fillcolor=\"#bfbfbf\"];\n");
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

//////////////////
// Type Checker //
//////////////////

static cane_type_kind_t cane_pass_semantic_analysis_walker(cane_ast_node_t* node
);

static void cane_pass_semantic_analysis(cane_ast_node_t* node) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_pass_semantic_analysis_walker(node);
}

// TODO:
// 1. We need to store assigned types
// 2. Function types

#define CANE_TYPE_REMAPPING \
	X(PREFIX, ABS, SCALAR, NONE, SCALAR) \
	X(PREFIX, NEG, SCALAR, NONE, SCALAR) \
\
	X(PREFIX, INVERT, RHYTHM, NONE, RHYTHM) \
	X(PREFIX, REVERSE, RHYTHM, NONE, RHYTHM) \
\
	X(PREFIX, REVERSE, MELODY, NONE, MELODY) \
\
	/* Scalar */ \
	X(INFIX, ADD, SCALAR, SCALAR, SCALAR) \
	X(INFIX, SUB, SCALAR, SCALAR, SCALAR) \
	X(INFIX, MUL, SCALAR, SCALAR, SCALAR) \
	X(INFIX, DIV, SCALAR, SCALAR, SCALAR) \
\
	X(INFIX, LSHIFT, SCALAR, SCALAR, SCALAR) \
	X(INFIX, RSHIFT, SCALAR, SCALAR, SCALAR) \
\
	X(INFIX, LCM, SCALAR, SCALAR, SCALAR) \
	X(INFIX, GCD, SCALAR, SCALAR, SCALAR) \
\
	X(INFIX, RHYTHM, SCALAR, SCALAR, SCALAR) \
	X(INFIX, CONCATENATE, SCALAR, SCALAR, MELODY) \
\
	/* Melody */ \
	X(INFIX, MAP, MELODY, RHYTHM, SEQUENCE) \
\
	X(INFIX, LSHIFT, MELODY, SCALAR, MELODY) \
	X(INFIX, RSHIFT, MELODY, SCALAR, MELODY) \
\
	X(INFIX, ADD, MELODY, SCALAR, MELODY) \
	X(INFIX, SUB, MELODY, SCALAR, MELODY) \
	X(INFIX, MUL, MELODY, SCALAR, MELODY) \
	X(INFIX, DIV, MELODY, SCALAR, MELODY) \
\
	X(INFIX, REPEAT, MELODY, SCALAR, MELODY) \
	X(INFIX, CONCATENATE, MELODY, MELODY, MELODY) \
\
	/* Rhythm */ \
	X(INFIX, MAP, RHYTHM, MELODY, SEQUENCE) \
\
	X(INFIX, LSHIFT, RHYTHM, SCALAR, RHYTHM) \
	X(INFIX, RSHIFT, RHYTHM, SCALAR, RHYTHM) \
\
	X(INFIX, REPEAT, RHYTHM, SCALAR, RHYTHM) \
	X(INFIX, CONCATENATE, RHYTHM, RHYTHM, RHYTHM) \
\
	X(INFIX, OR, RHYTHM, RHYTHM, RHYTHM) \
	X(INFIX, XOR, RHYTHM, RHYTHM, RHYTHM) \
	X(INFIX, AND, RHYTHM, RHYTHM, RHYTHM) \
\
	/* Sequence */ \
	X(INFIX, CONCATENATE, SEQUENCE, SEQUENCE, SEQUENCE) \
	X(INFIX, MUL, SEQUENCE, SCALAR, SEQUENCE) \
	X(INFIX, DIV, SEQUENCE, SCALAR, SEQUENCE)

static cane_type_kind_t cane_pass_semantic_analysis_walker(cane_ast_node_t* node
) {
	if (node == NULL) {
		return CANE_TYPE_NONE;
	}

	cane_symbol_kind_t kind = node->kind;

	cane_ast_node_t* lhs = node->lhs;
	cane_ast_node_t* rhs = node->rhs;

#define X(opfix, symbol, lhs_type, rhs_type, out) \
	if (CANE_SYMBOL_##symbol == kind) { \
		if (CANE_OPFIX_##opfix == CANE_OPFIX_PREFIX) { \
			cane_type_kind_t lhs_type_child = \
				cane_pass_semantic_analysis_walker(lhs); \
\
			if (CANE_TYPE_##lhs_type == lhs_type_child) { \
				CANE_LOG_WARN( \
					cane_logger_create_default(), \
					"%s `%s` %s -> %s", \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##lhs_type], \
					CANE_SYMBOL_TO_STR[kind], \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##rhs_type], \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##out] \
				); \
\
				node->type = CANE_TYPE_##out; \
				return CANE_TYPE_##out; \
			} \
			else { \
				CANE_DIE(cane_logger_create_default(), "mismatched types!"); \
			} \
		} \
\
		else if (CANE_OPFIX_##opfix == CANE_OPFIX_INFIX) { \
			cane_type_kind_t lhs_type_child = \
				cane_pass_semantic_analysis_walker(lhs); \
			cane_type_kind_t rhs_type_child = \
				cane_pass_semantic_analysis_walker(rhs); \
\
			if (CANE_TYPE_##lhs_type == lhs_type_child && \
				CANE_TYPE_##rhs_type == rhs_type_child) { \
				CANE_LOG_WARN( \
					cane_logger_create_default(), \
					"%s `%s` %s -> %s", \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##lhs_type], \
					CANE_SYMBOL_TO_STR[kind], \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##rhs_type], \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##out] \
				); \
\
				node->type = CANE_TYPE_##out; \
				return CANE_TYPE_##out; \
			} \
			else { \
				CANE_DIE( \
					cane_logger_create_default(), \
					"mismatched types: `%s` and `%s`!", \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##lhs_type], \
					CANE_TYPE_KIND_TO_STR_HUMAN[CANE_TYPE_##rhs_type] \
				); \
			} \
		} \
	}

	CANE_TYPE_REMAPPING;

	switch (kind) {
		case CANE_SYMBOL_NUMBER: {
			return CANE_TYPE_SCALAR;
		} break;

		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST: {
			return CANE_TYPE_RHYTHM;
		} break;

		case CANE_SYMBOL_STATEMENT: {
			// TODO: Statements should return the type of their last expression
			cane_type_kind_t lhs_type = cane_pass_semantic_analysis_walker(lhs);
			cane_type_kind_t rhs_type = cane_pass_semantic_analysis_walker(rhs);

			CANE_UNUSED(rhs_type);  // Always discard right hand type since we
									// return the last expression's type.

			node->type = lhs_type;
			return lhs_type;
		} break;

		default: {
			CANE_DIE(
				cane_logger_create_default(),
				"unimplemented %s",
				CANE_SYMBOL_TO_STR[kind]
			);
		} break;
	}

	return node->type;
}

#endif
