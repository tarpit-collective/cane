#ifndef CANE_PARSE_H
#define CANE_PARSE_H

#include <stdbool.h>
#include <stddef.h>

#include <stdlib.h>
#include <stdio.h>

#include <string.h>

#include <cane/def.h>
#include <cane/enum.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/lex.h>

/////////////////
// Classifiers //
/////////////////

// Note: these functions use the symbol kind _after_ remapping.

static bool cane_parser_is_literal(cane_symbol_kind_t kind) {
	return kind == CANE_SYMBOL_NUMBER || kind == CANE_SYMBOL_STRING ||
		kind == CANE_SYMBOL_REST || kind == CANE_SYMBOL_BEAT;
}

static bool cane_parser_is_primary(cane_symbol_kind_t kind) {
	return cane_parser_is_literal(kind) || kind == CANE_SYMBOL_FUNCTION ||
		kind == CANE_SYMBOL_IDENTIFIER || kind == CANE_SYMBOL_LPAREN ||
		kind == CANE_SYMBOL_CHOICE || kind == CANE_SYMBOL_LAYER;
}

static bool cane_parser_is_prefix(cane_symbol_kind_t kind) {
	return kind == CANE_SYMBOL_ABS || kind == CANE_SYMBOL_NEG ||
		kind == CANE_SYMBOL_INVERT || kind == CANE_SYMBOL_REVERSE;
}

static bool cane_parser_is_infix(cane_symbol_kind_t kind) {
	return
		// Arithmetic
		kind == CANE_SYMBOL_ADD || kind == CANE_SYMBOL_SUB ||
		kind == CANE_SYMBOL_MUL || kind == CANE_SYMBOL_DIV ||

		kind == CANE_SYMBOL_LCM || kind == CANE_SYMBOL_GCD ||

		// Misc.
		kind == CANE_SYMBOL_RHYTHM ||       // Euclide
		kind == CANE_SYMBOL_REPEAT ||       // Repeat
		kind == CANE_SYMBOL_MAP ||          // Map
		kind == CANE_SYMBOL_CONCATENATE ||  // Concatenate
		kind == CANE_SYMBOL_CALL ||

		// Logic
		kind == CANE_SYMBOL_OR || kind == CANE_SYMBOL_XOR ||
		kind == CANE_SYMBOL_AND ||

		// Left/Right Shift
		kind == CANE_SYMBOL_LSHIFT || kind == CANE_SYMBOL_RSHIFT;
}

static bool cane_parser_is_postfix(cane_symbol_kind_t kind) {
	return kind == CANE_SYMBOL_ASSIGN;  // Assignment
}

static bool cane_parser_is_unary(cane_symbol_kind_t kind) {
	return cane_parser_is_prefix(kind);
}

static bool cane_parser_is_binary(cane_symbol_kind_t kind) {
	return cane_parser_is_infix(kind) || cane_parser_is_postfix(kind);
}

static bool cane_parser_is_expression(cane_symbol_kind_t kind) {
	return cane_parser_is_primary(kind) || cane_parser_is_prefix(kind);
}

//////////////////////
// Symbol Remapping //
//////////////////////

// Remap symbols based on their position.
// Makes it easier to reason about operator kinds during parsing and when
// constructing the AST.

#define CANE_SYMBOL_OPFIX \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_DOT, CANE_SYMBOL_REST) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_EXCLAIM, CANE_SYMBOL_BEAT) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_TILDA, CANE_SYMBOL_INVERT) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_QUOTE, CANE_SYMBOL_REVERSE) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_ADD, CANE_SYMBOL_ABS) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_SUB, CANE_SYMBOL_NEG) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_LBRACE, CANE_SYMBOL_CHOICE) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_LBRACKET, CANE_SYMBOL_LAYER) \
	X(CANE_OPFIX_PREFIX, CANE_SYMBOL_BACKSLASH, CANE_SYMBOL_FUNCTION) \
\
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_COLON, CANE_SYMBOL_RHYTHM) \
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_STARS, CANE_SYMBOL_REPEAT) \
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_AT, CANE_SYMBOL_MAP) \
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_DOT, CANE_SYMBOL_CONCATENATE) \
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_LCHEVRON, CANE_SYMBOL_LSHIFT) \
	X(CANE_OPFIX_INFIX, CANE_SYMBOL_RCHEVRON, CANE_SYMBOL_RSHIFT) \
\
	X(CANE_OPFIX_POSTFIX, CANE_SYMBOL_ARROW, CANE_SYMBOL_ASSIGN)

static cane_symbol_kind_t
cane_fix_symbol(cane_opfix_kind_t opfix, cane_symbol_kind_t kind) {
#define X(o, f, t) \
	if (o == opfix && f == kind) { \
		return t; \
	}

	CANE_SYMBOL_OPFIX;
	return kind;

#undef X
}

// Wrappers so we can pass them directly to various lexer utility functions.
static cane_symbol_kind_t cane_fix_prefix_symbol(cane_symbol_kind_t kind) {
	return cane_fix_symbol(CANE_OPFIX_PREFIX, kind);
}

static cane_symbol_kind_t cane_fix_infix_symbol(cane_symbol_kind_t kind) {
	return cane_fix_symbol(CANE_OPFIX_INFIX, kind);
}

static cane_symbol_kind_t cane_fix_postfix_symbol(cane_symbol_kind_t kind) {
	return cane_fix_symbol(CANE_OPFIX_POSTFIX, kind);
}

static cane_symbol_kind_t cane_fix_unary_symbol(cane_symbol_kind_t kind) {
	return cane_fix_symbol(CANE_OPFIX_PREFIX, kind);
}

static cane_symbol_kind_t cane_fix_binary_symbol(cane_symbol_kind_t kind) {
	kind = cane_fix_symbol(CANE_OPFIX_INFIX, kind);
	kind = cane_fix_symbol(CANE_OPFIX_POSTFIX, kind);

	return kind;
}

/////////////////////////////////////////
// Binding Power / Operator Precedence //
/////////////////////////////////////////

typedef struct cane_binding_power cane_binding_power_t;

struct cane_binding_power {
	size_t lbp;
	size_t rbp;
};

// TODO: This is ugly as fuck. Fix it.
// X macro?
static cane_binding_power_t cane_parser_binding_power(cane_symbol_kind_t kind) {
	cane_binding_power_t bp = (cane_binding_power_t){
		.lbp = 0,
		.rbp = 0,
	};

	// TODO: Combine this with symbol definition X macros aswell as remapping
	// macros

	// clang-format off

#define CANE_BINDING_POWERS \
	X(CANE_SYMBOL_CALL,        1, 2) \
	X(CANE_SYMBOL_ASSIGN,      2, 3) \
\
	X(CANE_SYMBOL_OR,          3, 4) \
	X(CANE_SYMBOL_AND,         3, 4) \
	X(CANE_SYMBOL_XOR,         3, 4) \
	X(CANE_SYMBOL_REPEAT,      3, 4) \
	X(CANE_SYMBOL_LSHIFT,      3, 4) \
	X(CANE_SYMBOL_RSHIFT,      3, 4) \
\
	X(CANE_SYMBOL_MAP,         4, 5) \
\
	X(CANE_SYMBOL_CONCATENATE, 5, 6) \
\
	X(CANE_SYMBOL_INVERT,      6, 6) \
	X(CANE_SYMBOL_REVERSE,     6, 6) \
\
	X(CANE_SYMBOL_ADD,         7, 8) \
	X(CANE_SYMBOL_SUB,         7, 8) \
\
	X(CANE_SYMBOL_MUL,         8, 9) \
	X(CANE_SYMBOL_DIV,         8, 9) \
\
	X(CANE_SYMBOL_RHYTHM,      9, 10) \
\
	X(CANE_SYMBOL_LCM,         10, 11) \
	X(CANE_SYMBOL_GCD,         10, 11) \
\
	X(CANE_SYMBOL_ABS,         11, 11) \
	X(CANE_SYMBOL_NEG,         11, 11)

	// clang-format on

#define X(symbol, lbp, rbp) \
	case symbol: bp = (cane_binding_power_t){lbp, rbp}; break;

	switch (kind) {
		CANE_BINDING_POWERS;
		default: {
			CANE_DIE(
				cane_logger_create_default(),
				"no binding power for `%s`",
				CANE_SYMBOL_TO_STR[kind]
			);
		} break;
	}

	return bp;

#undef X
#undef CANE_BINDING_POWERS
}

/////////
// AST //
/////////

typedef struct cane_ast_node cane_ast_node_t;

struct cane_ast_node {
	cane_symbol_kind_t kind;
	cane_string_view_t sv;

	cane_type_kind_t type;

	cane_ast_node_t* lhs;
	cane_ast_node_t* rhs;
};

static cane_ast_node_t*
cane_ast_node_create(cane_symbol_kind_t kind, cane_string_view_t sv) {
	cane_ast_node_t* node = calloc(1, sizeof(cane_ast_node_t));

	node->kind = kind;
	node->sv = sv;

	node->type = CANE_TYPE_NONE;

	node->lhs = NULL;
	node->rhs = NULL;

	return node;
}

////////////
// PARSER //
////////////

// Primary call that sets up lexer and context automatically.
static cane_ast_node_t* cane_parse(cane_string_view_t sv);

// Forward declarations for mutual recursion
static cane_ast_node_t* cane_parse_program(cane_lexer_t* lx);

// Expression parsing
static cane_ast_node_t*
cane_parse_primary(cane_lexer_t* lx, cane_symbol_t symbol);

static cane_ast_node_t*
cane_parse_prefix(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp);

static cane_ast_node_t* cane_parse_infix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs, size_t bp
);

static cane_ast_node_t* cane_parse_postfix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs
);

static cane_ast_node_t* cane_parse_expression(cane_lexer_t* lx, size_t bp);

//////////////////////
// PARSER FUNCTIONS //
//////////////////////

static cane_ast_node_t* cane_parse(cane_string_view_t sv) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_lexer_t lx = cane_lexer_create(sv);
	return cane_parse_program(&lx);
}

// Core parsing functions
static cane_ast_node_t* cane_parse_program(cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_symbol_t symbol;
	cane_ast_node_t* root = NULL;

	while (!cane_lexer_peek_is_kind(lx, CANE_SYMBOL_ENDFILE, &symbol, NULL)) {
		cane_ast_node_t* node = cane_parse_expression(lx, 0);

		cane_ast_node_t* concat =
			cane_ast_node_create(CANE_SYMBOL_STATEMENT, symbol.sv);

		concat->lhs = node;
		concat->rhs = root;

		root = concat;

		cane_lexer_discard_if_kind(lx, CANE_SYMBOL_SEMICOLON);
	}

	if (!cane_lexer_discard_if_kind(lx, CANE_SYMBOL_ENDFILE)) {
		cane_report_and_die(
			cane_lexer_location_create(lx),
			CANE_REPORT_SYNTAX,
			"expected end of file"
		);
	}

	return root;
}

// Expression parsing
static cane_ast_node_t*
cane_parse_primary(cane_lexer_t* lx, cane_symbol_t symbol) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	switch (symbol.kind) {
		// Literals
		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER: {
			cane_lexer_discard(lx);
			return cane_ast_node_create(symbol.kind, symbol.sv);
		}

		// Literals (Implicit Concat)
		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST: {
			cane_lexer_discard(lx);
			cane_ast_node_t* root =
				cane_ast_node_create(symbol.kind, symbol.sv);

			while (cane_lexer_peek_is_kind(
					   lx, CANE_SYMBOL_BEAT, &symbol, cane_fix_unary_symbol
				   ) ||
				   cane_lexer_peek_is_kind(
					   lx, CANE_SYMBOL_REST, &symbol, cane_fix_unary_symbol
				   )) {
				cane_lexer_take(lx, &symbol, cane_fix_unary_symbol);
				cane_ast_node_t* node =
					cane_ast_node_create(symbol.kind, symbol.sv);

				cane_ast_node_t* concat =
					cane_ast_node_create(CANE_SYMBOL_CONCATENATE, symbol.sv);

				concat->lhs = node;
				concat->rhs = root;

				root = concat;
			}

			return root;
		} break;

		case CANE_SYMBOL_LPAREN: {
			cane_lexer_discard(lx);  // Skip `(`

			cane_ast_node_t* expr = cane_parse_expression(lx, 0);

			if (!cane_lexer_discard_if_kind(lx, CANE_SYMBOL_RPAREN)) {
				cane_report_and_die(
					cane_lexer_location_create(lx),
					CANE_REPORT_SYNTAX,
					"expected `)`"
				);
			}

			return expr;
		} break;

		case CANE_SYMBOL_CHOICE: {
			cane_lexer_discard(lx);  // Skip `{`
			cane_ast_node_t* root = NULL;

			// Need at least one expression.
			do {
				cane_ast_node_t* node = cane_parse_expression(lx, 0);

				cane_ast_node_t* choice =
					cane_ast_node_create(CANE_SYMBOL_CHOICE, symbol.sv);

				choice->lhs = node;
				choice->rhs = root;

				root = choice;

				cane_lexer_discard_if_kind(lx, CANE_SYMBOL_COMMA);
			} while (
				!cane_lexer_peek_is_kind(lx, CANE_SYMBOL_RBRACE, &symbol, NULL)
			);

			if (!cane_lexer_discard_if_kind(lx, CANE_SYMBOL_RBRACE)) {
				cane_report_and_die(
					cane_lexer_location_create(lx),
					CANE_REPORT_SYNTAX,
					"expected `}`"
				);
			}

			return root;
		} break;

		case CANE_SYMBOL_LAYER: {
			cane_lexer_discard(lx);  // Skip `[`
			cane_ast_node_t* root = NULL;

			// Need at least one expression.
			do {
				cane_ast_node_t* node = cane_parse_expression(lx, 0);

				cane_ast_node_t* choice =
					cane_ast_node_create(CANE_SYMBOL_LAYER, symbol.sv);

				choice->lhs = node;
				choice->rhs = root;

				root = choice;

				cane_lexer_discard_if_kind(lx, CANE_SYMBOL_COMMA);
			} while (!cane_lexer_peek_is_kind(
				lx, CANE_SYMBOL_RBRACKET, &symbol, NULL
			));

			if (!cane_lexer_discard_if_kind(lx, CANE_SYMBOL_RBRACKET)) {
				cane_report_and_die(
					cane_lexer_location_create(lx),
					CANE_REPORT_SYNTAX,
					"expected `]`"
				);
			}

			return root;
		} break;

		case CANE_SYMBOL_FUNCTION: {
			cane_lexer_discard(lx);  // Skip `\`

			cane_symbol_t ident;
			if (!cane_lexer_take_if_kind(
					lx, CANE_SYMBOL_IDENTIFIER, &ident, NULL
				)) {
				cane_report_and_die(
					cane_lexer_location_create(lx),
					CANE_REPORT_SYNTAX,
					"expected an identifier"
				);
			}

			cane_ast_node_t* param =
				cane_ast_node_create(CANE_SYMBOL_IDENTIFIER, ident.sv);

			// Reset binding power
			cane_ast_node_t* body = cane_parse_expression(lx, 0);

			cane_ast_node_t* fn =
				cane_ast_node_create(CANE_SYMBOL_FUNCTION, ident.sv);

			fn->lhs = param;
			fn->rhs = body;

			return fn;
		} break;

		default: break;
	}

	cane_report_and_die(
		cane_lexer_location_create(lx),
		CANE_REPORT_SYNTAX,
		"expected a primary expression"
	);

	return NULL;
}

static cane_ast_node_t*
cane_parse_prefix(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	// We need to call this function directly instead of using something like
	// `cane_lexer_discard_if` because we have fixed up the symbol earlier and
	// peeking again would return the incorrect/lexical token kind instead.
	if (!cane_parser_is_prefix(symbol.kind)) {
		cane_report_and_die(
			cane_lexer_location_create(lx),
			CANE_REPORT_SYNTAX,
			"expected a prefix operator"
		);
	}

	cane_ast_node_t* node = cane_ast_node_create(symbol.kind, symbol.sv);
	cane_lexer_discard(lx);

	cane_ast_node_t* expr = cane_parse_expression(lx, bp);

	// Prefix nodes don't have a left-hand side.
	node->lhs = NULL;
	node->rhs = expr;

	return node;
}

static cane_ast_node_t* cane_parse_infix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs, size_t bp
) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	if (!cane_parser_is_infix(symbol.kind)) {
		cane_report_and_die(
			cane_lexer_location_create(lx),
			CANE_REPORT_SYNTAX,
			"expected an infix operator"
		);
	}

	cane_ast_node_t* node = cane_ast_node_create(symbol.kind, symbol.sv);
	cane_lexer_discard(lx);

	cane_ast_node_t* rhs = cane_parse_expression(lx, bp);

	node->lhs = lhs;
	node->rhs = rhs;

	return node;
}

static cane_ast_node_t* cane_parse_postfix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs
) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	if (!cane_parser_is_postfix(symbol.kind)) {
		cane_report_and_die(
			cane_lexer_location_create(lx),
			CANE_REPORT_SYNTAX,
			"expected a postfix operator"
		);
	}

	cane_ast_node_t* node = cane_ast_node_create(symbol.kind, symbol.sv);
	cane_lexer_discard(lx);

	// We put the child node on the `rhs` to be consistent with prefix
	// expressions so we don't have to have seperate handling for them.
	node->lhs = NULL;
	node->rhs = lhs;

	// Assignment is a bit special in that it has a parameter in the form of an
	// identifier to bind the expression's value to.
	if (symbol.kind == CANE_SYMBOL_ASSIGN) {
		cane_symbol_t ident;

		if (!cane_lexer_take_if_kind(
				lx, CANE_SYMBOL_IDENTIFIER, &ident, NULL
			)) {
			cane_report_and_die(
				cane_lexer_location_create(lx),
				CANE_REPORT_SYNTAX,
				"expected an identifier"
			);
		}

		node->lhs = cane_ast_node_create(CANE_SYMBOL_IDENTIFIER, ident.sv);
	}

	return node;
}

static cane_ast_node_t* cane_parse_expression(cane_lexer_t* lx, size_t min_bp) {
	cane_ast_node_t* node = NULL;

	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol, cane_fix_unary_symbol);

	if (cane_parser_is_primary(symbol.kind)) {
		node = cane_parse_primary(lx, symbol);
	}

	else if (cane_parser_is_prefix(symbol.kind)) {
		size_t rbp = cane_parser_binding_power(symbol.kind).rbp;
		node = cane_parse_prefix(lx, symbol, rbp);
	}

	else {
		cane_report_and_die(
			cane_lexer_location_create(lx),
			CANE_REPORT_SYNTAX,
			"expected a primary expression or a prefix operator"
		);
	}

	// State has changed since we called prefix/primary parser functions
	// so we need to peek again.
	cane_lexer_peek(lx, &symbol, cane_fix_binary_symbol);

	while (cane_parser_is_infix(symbol.kind) ||
		   cane_parser_is_postfix(symbol.kind) ||
		   cane_parser_is_expression(symbol.kind)) {
		// Two expressions juxtaposed is a function call
		// if (cane_is_expression(symbol)) {
		// 	symbol.kind = CANE_SYMBOL_CALL;
		// }

		cane_binding_power_t binding_power =
			cane_parser_binding_power(symbol.kind);

		if (binding_power.lbp < min_bp) {
			break;
		}

		if (cane_parser_is_postfix(symbol.kind)) {
			node = cane_parse_postfix(lx, symbol, node);
		}

		else if (cane_parser_is_infix(symbol.kind)) {
			node = cane_parse_infix(lx, symbol, node, binding_power.rbp);
		}

		else {
			cane_report_and_die(
				cane_lexer_location_create(lx),
				CANE_REPORT_SYNTAX,
				"expected an infix or postfix operator"

			);
		}

		cane_lexer_peek(lx, &symbol, cane_fix_binary_symbol);
	}

	return node;
}

#endif
