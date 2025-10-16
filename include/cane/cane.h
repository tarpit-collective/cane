#ifndef CANE_H
#define CANE_H

#include <stdbool.h>

#include <cane/enum.h>
#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/vec.h>
#include <cane/str.h>
#include <cane/lex.h>
#include <cane/parse.h>

//////////////////////
// AST Printer Pass //
//////////////////////

static void cane_pass_print_walker(cane_ast_node_t* node, int depth);

static void cane_pass_print(cane_ast_node_t* node) {
	CANE_FUNCTION_ENTER();
	cane_pass_print_walker(node, 0);
}

static void cane_pass_print_walker(cane_ast_node_t* node, int depth) {
	if (node == NULL) {
		printf("%*sNULL\n", depth * 4, "");
		return;
	}

	cane_symbol_kind_t kind = node->kind;
	cane_string_view_t sv = node->location.symbol;

	cane_location_t loc = node->location;

	const char* sv_begin = sv.begin;
	const char* sv_end = sv.end;

	int sv_length = cane_ptrdiff(sv_begin, sv_end);

	cane_ast_node_t* lhs = node->lhs;
	cane_ast_node_t* rhs = node->rhs;

	switch (kind) {
		// Literals
		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST:

		case CANE_SYMBOL_RHYTHM:
		case CANE_SYMBOL_MELODY:

		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_STRING:
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

		case CANE_SYMBOL_EUCLIDEAN:
		case CANE_SYMBOL_MAP:
		case CANE_SYMBOL_REPEAT:

		case CANE_SYMBOL_LSHIFT:
		case CANE_SYMBOL_RSHIFT:

		case CANE_SYMBOL_OR:
		case CANE_SYMBOL_XOR:
		case CANE_SYMBOL_AND:

		case CANE_SYMBOL_CALL:
		case CANE_SYMBOL_CONCATENATE:
		case CANE_SYMBOL_RANDOM:

		case CANE_SYMBOL_ASSIGN:
		case CANE_SYMBOL_SEND:
		case CANE_SYMBOL_FUNCTION: {
			printf("%*s%s\n", depth * 4, "", CANE_SYMBOL_TO_STR[kind]);

			cane_pass_print_walker(lhs, depth + 1);
			cane_pass_print_walker(rhs, depth + 1);
		} break;

		// Lists
		case CANE_SYMBOL_LAYER:
		case CANE_SYMBOL_STATEMENT: {
			printf("%*s%s\n", depth * 4, "", CANE_SYMBOL_TO_STR[kind]);

			cane_pass_print_walker(lhs, depth + 1);
			cane_pass_print_walker(rhs, depth + 1);
		} break;

		default: {
			cane_report_and_die(
				loc,
				CANE_REPORT_GENERIC,
				"unhandled case `%s`!",
				CANE_SYMBOL_TO_STR[kind]
			);
		} break;
	}
}

//////////////////
// AST GraphViz //
//////////////////

static void cane_pass_graphviz_edge(
	cane_file_t fp, cane_ast_node_t* node, size_t parent, size_t self
) {
	cane_symbol_kind_t kind = CANE_SYMBOL_NONE;
	cane_string_view_t sv = CANE_SV("NULL");

	cane_type_kind_t type = CANE_TYPE_NONE;

	if (node != NULL) {
		kind = node->kind;
		sv = node->location.symbol;

		type = node->type;
	}

	cane_string_view_info_t info = cane_string_view_info(sv);

	fprintf(
		fp,
		"  n%zu [label=\"kind = %s\nsv = `%.*s`\ntype = %s\"];\n",
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
	cane_file_t fp, cane_ast_node_t* node, size_t* id, size_t parent
);

static void cane_pass_graphviz(cane_ast_node_t* node, cane_file_t fp) {
	CANE_FUNCTION_ENTER();

	size_t id = 0;

	fprintf(fp, "digraph {\n");
	fprintf(fp, "  node [shape=box style=filled fillcolor=\"#bfbfbf\"];\n");

	cane_pass_graphviz_walker(fp, node, &id, 0);

	fprintf(fp, "}\n");
}

static void cane_pass_graphviz_walker(
	cane_file_t fp, cane_ast_node_t* node, size_t* id, size_t parent
) {
	size_t self = (*id)++;

	if (node == NULL) {
		cane_pass_graphviz_edge(fp, node, parent, self);
		return;
	}

	cane_symbol_kind_t kind = node->kind;

	cane_location_t loc = node->location;

	cane_ast_node_t* lhs = node->lhs;
	cane_ast_node_t* rhs = node->rhs;

	switch (kind) {
		// Literals
		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST:

		case CANE_SYMBOL_RHYTHM:
		case CANE_SYMBOL_MELODY:

		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_STRING:
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

		case CANE_SYMBOL_EUCLIDEAN:
		case CANE_SYMBOL_MAP:
		case CANE_SYMBOL_REPEAT:

		case CANE_SYMBOL_LSHIFT:
		case CANE_SYMBOL_RSHIFT:

		case CANE_SYMBOL_OR:
		case CANE_SYMBOL_XOR:
		case CANE_SYMBOL_AND:

		case CANE_SYMBOL_CALL:
		case CANE_SYMBOL_CONCATENATE:
		case CANE_SYMBOL_RANDOM:

		case CANE_SYMBOL_ASSIGN:
		case CANE_SYMBOL_SEND:
		case CANE_SYMBOL_FUNCTION: {
			cane_pass_graphviz_edge(fp, node, parent, self);
			cane_pass_graphviz_walker(fp, lhs, id, self);
			cane_pass_graphviz_walker(fp, rhs, id, self);
		} break;

		// Lists
		case CANE_SYMBOL_LAYER:
		case CANE_SYMBOL_STATEMENT: {
			cane_pass_graphviz_edge(fp, node, parent, self);
			cane_pass_graphviz_walker(fp, lhs, id, self);
			cane_pass_graphviz_walker(fp, rhs, id, self);
		} break;

		default: {
			cane_report_and_die(
				loc,
				CANE_REPORT_GENERIC,
				"unhandled case `%s`!",
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
	CANE_FUNCTION_ENTER();
	cane_pass_semantic_analysis_walker(node);
}

// TODO:
// 1. We need to store assigned types
// 2. Function types
static bool cane_type_remapper(
	cane_ast_node_t* node,
	cane_symbol_kind_t kind,
	cane_type_kind_t expected_lhs,
	cane_type_kind_t expected_rhs,
	cane_type_kind_t out,
	cane_symbol_kind_t new_kind
) {
	if (node == NULL || node->kind != kind) {
		return false;
	}

	cane_location_t loc = node->location;
	cane_lineinfo_t info = cane_location_coordinates(loc);

	// In the case of a UNARY remapping, rhs will match with NONE anyway so we
	// can always just compare both types.
	cane_type_kind_t lhs = cane_pass_semantic_analysis_walker(node->lhs);
	cane_type_kind_t rhs = cane_pass_semantic_analysis_walker(node->rhs);

	CANE_LOG_INFO(
		"attempt " CANE_BLUE "`%s`" CANE_RESET " { `%s = " CANE_MAGENTA
		"%s" CANE_RESET "`, `%s = " CANE_MAGENTA "%s" CANE_RESET
		"` } -> " CANE_MAGENTA "%s" CANE_RESET " " CANE_BLUE
		"@ loc(%d:%d)" CANE_RESET,
		CANE_SYMBOL_TO_STR[kind],
		CANE_TYPE_KIND_TO_STR_HUMAN[expected_lhs],
		CANE_TYPE_KIND_TO_STR_HUMAN[lhs],
		CANE_TYPE_KIND_TO_STR_HUMAN[expected_rhs],
		CANE_TYPE_KIND_TO_STR_HUMAN[rhs],
		CANE_TYPE_KIND_TO_STR_HUMAN[out],
		info.line,
		info.column
	);

	if (lhs != expected_lhs || rhs != expected_rhs) {
		// If the types don't match, it just means this overload of the operator
		// isn't the correct one but we might have one handled later.
		CANE_LOG_FAIL("└─ " CANE_RED "failed!" CANE_RESET);
		return false;
	}

	CANE_LOG_OKAY("└─ " CANE_YELLOW "success!" CANE_RESET);

	node->type = out;
	node->kind = new_kind;

	return true;
}

// TODO:
// We need to figure out how to handle rhythms with beats/rests and how
// to type check them. They really need to be a unified type.
static bool cane_type_remap_trivial(cane_ast_node_t* node) {
#define CANE_TYPE_REMAP(symbol, lhs_type, rhs_type, out_type, out_symbol) \
	cane_type_remapper( \
		node, \
		CANE_SYMBOL_##symbol, \
		CANE_TYPE_##lhs_type, \
		CANE_TYPE_##rhs_type, \
		CANE_TYPE_##out_type, \
		CANE_SYMBOL_##out_symbol \
	)

	// clang-format off

	return
		/* Prefix/Unary */
		CANE_TYPE_REMAP(ABS, SCALAR, NONE, SCALAR, ABS_SCALAR) ||
		CANE_TYPE_REMAP(NEG, SCALAR, NONE, SCALAR, NEG_SCALAR) ||

		CANE_TYPE_REMAP(INVERT, RHYTHM, NONE, RHYTHM, INVERT_RHYTHM) ||
		CANE_TYPE_REMAP(REVERSE, RHYTHM, NONE, RHYTHM, REVERSE_RHYTHM) ||

		CANE_TYPE_REMAP(REVERSE, MELODY, NONE, MELODY, REVERSE_MELODY) ||

		/* Scalar */
		CANE_TYPE_REMAP(ADD, SCALAR, SCALAR, SCALAR, ADD_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(SUB, SCALAR, SCALAR, SCALAR, SUB_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(MUL, SCALAR, SCALAR, SCALAR, MUL_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(DIV, SCALAR, SCALAR, SCALAR, DIV_SCALAR_SCALAR) ||

		CANE_TYPE_REMAP(LSHIFT, SCALAR, SCALAR, SCALAR, LSHIFT_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(RSHIFT, SCALAR, SCALAR, SCALAR, RSHIFT_SCALAR_SCALAR) ||

		CANE_TYPE_REMAP(LCM, SCALAR, SCALAR, SCALAR, LCM_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(GCD, SCALAR, SCALAR, SCALAR, GCD_SCALAR_SCALAR) ||

		CANE_TYPE_REMAP(EUCLIDEAN, SCALAR, SCALAR, SCALAR, EUCLIDEAN_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(CONCATENATE, SCALAR, SCALAR, MELODY, CONCATENATE_SCALAR_SCALAR) ||
		CANE_TYPE_REMAP(RANDOM, SCALAR, SCALAR, SCALAR, RANDOM_SCALAR_SCALAR) ||

		/* Melody */
		CANE_TYPE_REMAP(MAP, MELODY, RHYTHM, SEQUENCE, MAP_MELODY_RHYTHM) ||

		CANE_TYPE_REMAP(LSHIFT, MELODY, SCALAR, MELODY, LSHIFT_MELODY_SCALAR) ||
		CANE_TYPE_REMAP(RSHIFT, MELODY, SCALAR, MELODY, RSHIFT_MELODY_SCALAR) ||

		CANE_TYPE_REMAP(ADD, MELODY, SCALAR, MELODY, ADD_MELODY_SCALAR) ||
		CANE_TYPE_REMAP(SUB, MELODY, SCALAR, MELODY, SUB_MELODY_SCALAR) ||
		CANE_TYPE_REMAP(MUL, MELODY, SCALAR, MELODY, MUL_MELODY_SCALAR) ||
		CANE_TYPE_REMAP(DIV, MELODY, SCALAR, MELODY, DIV_MELODY_SCALAR) ||

		CANE_TYPE_REMAP(REPEAT, MELODY, SCALAR, MELODY, REPEAT_MELODY_SCALAR) ||
		CANE_TYPE_REMAP(CONCATENATE, MELODY, MELODY, MELODY, CONCATENATE_MELODY_MELODY) ||

		/* Rhythm */
		CANE_TYPE_REMAP(MAP, RHYTHM, MELODY, SEQUENCE, MAP_RHYTHM_MELODY) ||

		CANE_TYPE_REMAP(LSHIFT, RHYTHM, SCALAR, RHYTHM, LSHIFT_RHYTHM_SCALAR) ||
		CANE_TYPE_REMAP(RSHIFT, RHYTHM, SCALAR, RHYTHM, RSHIFT_RHYTHM_SCALAR) ||

		CANE_TYPE_REMAP(REPEAT, RHYTHM, SCALAR, RHYTHM, REPEAT_RHYTHM_SCALAR) ||
		CANE_TYPE_REMAP(CONCATENATE, RHYTHM, RHYTHM, RHYTHM, CONCATENATE_RHYTHM_RHYTHM) ||

		CANE_TYPE_REMAP(OR, RHYTHM, RHYTHM, RHYTHM, OR_RHYTHM_RHYTHM) ||
		CANE_TYPE_REMAP(XOR, RHYTHM, RHYTHM, RHYTHM, XOR_RHYTHM_RHYTHM) ||
		CANE_TYPE_REMAP(AND, RHYTHM, RHYTHM, RHYTHM, AND_RHYTHM_RHYTHM) ||

		/* Sequence */
		CANE_TYPE_REMAP(CONCATENATE, SEQUENCE, SEQUENCE, SEQUENCE, CONCATENATE_SEQUENCE_SEQUENCE) ||

		CANE_TYPE_REMAP(MUL, SEQUENCE, SCALAR, SEQUENCE, MUL_SEQUENCE_SCALAR) ||
		CANE_TYPE_REMAP(DIV, SEQUENCE, SCALAR, SEQUENCE, DIV_SEQUENCE_SCALAR)
	;

	// clang-format on

#undef CANE_TYPE_REMAP
}

static cane_type_kind_t cane_pass_semantic_analysis_walker(cane_ast_node_t* node
) {
	if (node == NULL) {
		return CANE_TYPE_NONE;
	}

	cane_location_t loc = node->location;

	// Handle trivial cases for remapping first but otherwise fallback to this
	// switch where we handle them manually.
	if (!cane_type_remap_trivial(node)) {
		switch (node->kind) {
			// Assignment
			case CANE_SYMBOL_IDENTIFIER:
			case CANE_SYMBOL_ASSIGN:
			case CANE_SYMBOL_SEND: {
				CANE_UNIMPLEMENTED();
			} break;

			// Literals
			case CANE_SYMBOL_NUMBER:
			case CANE_SYMBOL_STRING:
			case CANE_SYMBOL_RHYTHM:
			case CANE_SYMBOL_MELODY: {
				return node->type;
			} break;

			// TODO: Fix this, not sure if this is actually correct in all
			// cases.
			case CANE_SYMBOL_FUNCTION: {
				return node->rhs->type;
			} break;

			case CANE_SYMBOL_CALL: {
				cane_type_kind_t function =
					cane_pass_semantic_analysis_walker(node->lhs);

				cane_type_kind_t argument =
					cane_pass_semantic_analysis_walker(node->rhs);

				if (function != argument) {
					cane_report_and_die(
						loc,
						CANE_REPORT_TYPE,
						"incorrect argument type for function `%s`!",
						CANE_SYMBOL_TO_STR[node->kind]
					);
				}

				return function;
			} break;

			// Statements always return the type of their last expression.
			case CANE_SYMBOL_LAYER:
			case CANE_SYMBOL_STATEMENT: {
				cane_type_kind_t lhs =
					cane_pass_semantic_analysis_walker(node->lhs);
				cane_type_kind_t rhs =
					cane_pass_semantic_analysis_walker(node->rhs);

				CANE_UNUSED(rhs);  // Always discard right hand type since
								   // we return the last expression's type.

				node->type = lhs;
				return lhs;
			} break;

			default: {
				cane_report_and_die(
					loc,
					CANE_REPORT_TYPE,
					"unknown type mapping for `%s`!",
					CANE_SYMBOL_TO_STR[node->kind]
				);
			} break;
		}
	}

	return node->type;
}

///////////////
// Evaluator //
///////////////

static cane_value_t cane_pass_evaluator_walker(cane_ast_node_t* node);

static cane_value_t cane_pass_evaluator(cane_ast_node_t* node) {
	CANE_FUNCTION_ENTER();
	return cane_pass_evaluator_walker(node);
}

static cane_value_t cane_pass_evaluator_walker(cane_ast_node_t* node) {
	if (node == NULL) {
		return (cane_value_t){};
	}

	cane_location_t loc = node->location;

	switch (node->kind) {
		case CANE_SYMBOL_ABS_SCALAR:
		case CANE_SYMBOL_NEG_SCALAR:

		case CANE_SYMBOL_INVERT_RHYTHM:
		case CANE_SYMBOL_REVERSE_RHYTHM:
		case CANE_SYMBOL_REVERSE_MELODY:

		case CANE_SYMBOL_ADD_SCALAR_SCALAR:
		case CANE_SYMBOL_SUB_SCALAR_SCALAR:
		case CANE_SYMBOL_MUL_SCALAR_SCALAR:
		case CANE_SYMBOL_DIV_SCALAR_SCALAR:

		case CANE_SYMBOL_LSHIFT_SCALAR_SCALAR:
		case CANE_SYMBOL_RSHIFT_SCALAR_SCALAR:

		case CANE_SYMBOL_LCM_SCALAR_SCALAR:
		case CANE_SYMBOL_GCD_SCALAR_SCALAR:

		case CANE_SYMBOL_EUCLIDEAN_SCALAR_SCALAR:
		case CANE_SYMBOL_CONCATENATE_SCALAR_SCALAR:
		case CANE_SYMBOL_RANDOM_SCALAR_SCALAR:
		case CANE_SYMBOL_MAP_MELODY_RHYTHM:

		case CANE_SYMBOL_LSHIFT_MELODY_SCALAR:
		case CANE_SYMBOL_RSHIFT_MELODY_SCALAR:

		case CANE_SYMBOL_ADD_MELODY_SCALAR:
		case CANE_SYMBOL_SUB_MELODY_SCALAR:
		case CANE_SYMBOL_MUL_MELODY_SCALAR:
		case CANE_SYMBOL_DIV_MELODY_SCALAR:

		case CANE_SYMBOL_REPEAT_MELODY_SCALAR:
		case CANE_SYMBOL_CONCATENATE_MELODY_MELODY:
		case CANE_SYMBOL_MAP_RHYTHM_MELODY:

		case CANE_SYMBOL_LSHIFT_RHYTHM_SCALAR:
		case CANE_SYMBOL_RSHIFT_RHYTHM_SCALAR:

		case CANE_SYMBOL_REPEAT_RHYTHM_SCALAR:
		case CANE_SYMBOL_CONCATENATE_RHYTHM_RHYTHM:

		case CANE_SYMBOL_OR_RHYTHM_RHYTHM:
		case CANE_SYMBOL_XOR_RHYTHM_RHYTHM:
		case CANE_SYMBOL_AND_RHYTHM_RHYTHM:

		case CANE_SYMBOL_CONCATENATE_SEQUENCE_SEQUENCE:

		case CANE_SYMBOL_MUL_SEQUENCE_SCALAR:
		case CANE_SYMBOL_DIV_SEQUENCE_SCALAR: {
			CANE_UNIMPLEMENTED();
		} break;

		default: {
			cane_report_and_die(
				loc,
				CANE_REPORT_TYPE,
				"cannot evaluate `%s`!",
				CANE_SYMBOL_TO_STR[node->kind]
			);
		} break;
	}

	return (cane_value_t){};
}

#endif
