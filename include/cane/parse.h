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

// Fix symbols in different contexts.
// TODO:
// 1. Turn this into an X macro like for sigil parsing to clean things up
// 2. Split into 3 distinct functions for prefix/infix/postfix
static cane_symbol_kind_t cane_fix_unary_symbol(cane_symbol_kind_t kind) {
	// Convert symbols in prefix location.
	// Makes it easier to reason about operator kinds during parsing and when
	// constructing the AST.

	cane_symbol_kind_t new_kind = kind;

	switch (kind) {
		case CANE_SYMBOL_DOT: new_kind = CANE_SYMBOL_REST; break;
		case CANE_SYMBOL_EXCLAIM: new_kind = CANE_SYMBOL_BEAT; break;

		case CANE_SYMBOL_TILDA: new_kind = CANE_SYMBOL_INVERT; break;
		case CANE_SYMBOL_QUOTE: new_kind = CANE_SYMBOL_REVERSE; break;

		case CANE_SYMBOL_ADD: new_kind = CANE_SYMBOL_ABS; break;
		case CANE_SYMBOL_SUB: new_kind = CANE_SYMBOL_NEG; break;

		case CANE_SYMBOL_LBRACE: new_kind = CANE_SYMBOL_CHOICE; break;
		case CANE_SYMBOL_LBRACKET: new_kind = CANE_SYMBOL_LAYER; break;

		case CANE_SYMBOL_BACKSLASH: new_kind = CANE_SYMBOL_FUNCTION; break;

		default: return kind;
	}

	CANE_LOG_WARN(
		cane_logger_create_default(),
		"fixup unary %s => %s",
		CANE_SYMBOL_TO_STR[kind],
		CANE_SYMBOL_TO_STR[new_kind]
	);

	return new_kind;
}

static cane_symbol_kind_t cane_fix_binary_symbol(cane_symbol_kind_t kind) {
	cane_symbol_kind_t new_kind = kind;

	switch (kind) {
		case CANE_SYMBOL_COLON: new_kind = CANE_SYMBOL_RHYTHM; break;
		case CANE_SYMBOL_STARS: new_kind = CANE_SYMBOL_REPEAT; break;
		case CANE_SYMBOL_AT: new_kind = CANE_SYMBOL_MAP; break;
		case CANE_SYMBOL_DOT: new_kind = CANE_SYMBOL_CONCATENATE; break;

		case CANE_SYMBOL_LCHEVRON: new_kind = CANE_SYMBOL_LSHIFT; break;
		case CANE_SYMBOL_RCHEVRON: new_kind = CANE_SYMBOL_RSHIFT; break;

		case CANE_SYMBOL_ARROW: new_kind = CANE_SYMBOL_ASSIGN; break;

		default: return kind;
	}

	CANE_LOG_WARN(
		cane_logger_create_default(),
		"fixup binary %s => %s",
		CANE_SYMBOL_TO_STR[kind],
		CANE_SYMBOL_TO_STR[new_kind]
	);

	return new_kind;
}

// Binding power
typedef struct cane_binding_power cane_binding_power_t;

struct cane_binding_power {
	size_t lbp;
	size_t rbp;
};

// TODO: This is ugly as fuck. Fix it.
// X macro?
static cane_binding_power_t cane_parser_binding_power(cane_symbol_kind_t s) {
	switch (s) {
		// Prefix
		case CANE_SYMBOL_ABS: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_NEG: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_INVERT: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_REVERSE: return (cane_binding_power_t){0, 0};

		// Infix
		case CANE_SYMBOL_ADD: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_SUB: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_MUL: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_DIV: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_LCM: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_GCD: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_RHYTHM: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_MAP: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_REPEAT: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_LSHIFT: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_RSHIFT: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_OR: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_XOR: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_AND: return (cane_binding_power_t){0, 0};

		case CANE_SYMBOL_CALL: return (cane_binding_power_t){0, 0};
		case CANE_SYMBOL_CONCATENATE: return (cane_binding_power_t){0, 0};

		// Postfix
		case CANE_SYMBOL_ASSIGN: return (cane_binding_power_t){0, 0};

		default: break;
	}

	return (cane_binding_power_t){
		.lbp = 0,
		.rbp = 0,
	};
}

// Convenience functions
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

static bool cane_parser_is_expression(cane_symbol_kind_t kind) {
	return cane_parser_is_primary(kind) || cane_parser_is_prefix(kind);
}

static bool cane_peek_is_literal(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_literal, NULL, NULL);
}

static bool cane_peek_is_primary(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_primary, NULL, NULL);
}

static bool cane_peek_is_prefix(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_prefix, NULL, NULL);
}

static bool cane_peek_is_infix(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_infix, NULL, NULL);
}

static bool cane_peek_is_postfix(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_postfix, NULL, NULL);
}

static bool cane_peek_is_expression(cane_lexer_t* lx) {
	return cane_lexer_peek_is(lx, cane_parser_is_expression, NULL, NULL);
}

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
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	cane_ast_node_t* root = NULL;

	// TODO: To keep things simple, we can use a binary tree for everything
	// and join statements together with a CANE_SYMBOL_STATEMENT node.
	// We can just evaluate both sides in isolation.
	// Basically a "cons" list like in lisp.

	while (!cane_lexer_peek_is_kind(lx, CANE_SYMBOL_ENDFILE, NULL, NULL)) {
		root = cane_parse_expression(lx, 0);
		cane_lexer_discard_if_kind(lx, CANE_SYMBOL_SEMICOLON);
	}

	if (!cane_lexer_discard_if_kind(lx, CANE_SYMBOL_ENDFILE)) {
		cane_report_and_die(CANE_REPORT_SYNTAX, "expected end of file");
	}

	return root;
}

// Expression parsing
static cane_ast_node_t*
cane_parse_primary(cane_lexer_t* lx, cane_symbol_t symbol) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

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
					   lx, CANE_SYMBOL_BEAT, NULL, cane_fix_unary_symbol
				   ) ||
				   cane_lexer_peek_is_kind(
					   lx, CANE_SYMBOL_REST, NULL, cane_fix_unary_symbol
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
				cane_report_and_die(CANE_REPORT_SYNTAX, "expected `)`");
			}

			return expr;
		} break;

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER: {
			CANE_UNIMPLEMENTED(log);
		} break;

		case CANE_SYMBOL_FUNCTION: {
			cane_lexer_discard(lx);  // Skip `\`

			cane_symbol_t ident;
			if (!cane_lexer_take_if_kind(
					lx, CANE_SYMBOL_IDENTIFIER, &ident, cane_fix_unary_symbol
				)) {
				cane_report_and_die(
					CANE_REPORT_SYNTAX, "expected an identifier"
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

	cane_report_and_die(CANE_REPORT_SYNTAX, "expected a primary expression");

	return NULL;
}

static cane_ast_node_t*
cane_parse_prefix(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	// We need to call this function directly instead of using something like
	// `cane_lexer_discard_if` because we have fixed up the symbol earlier and
	// peeking again would return the incorrect/lexical token kind instead.
	if (!cane_parser_is_prefix(symbol.kind)) {
		cane_report_and_die(CANE_REPORT_SYNTAX, "expected a prefix operator");
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
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	if (!cane_parser_is_infix(symbol.kind)) {
		cane_report_and_die(CANE_REPORT_SYNTAX, "expected an infix operator");
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
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	if (!cane_parser_is_postfix(symbol.kind)) {
		cane_report_and_die(CANE_REPORT_SYNTAX, "expected a postfix operator");
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
				lx, CANE_SYMBOL_IDENTIFIER, &ident, cane_fix_unary_symbol
			)) {
			cane_report_and_die(CANE_REPORT_SYNTAX, "expected an identifier");
		}

		node->lhs = cane_ast_node_create(CANE_SYMBOL_IDENTIFIER, ident.sv);
	}

	return node;
}

static cane_ast_node_t* cane_parse_expression(cane_lexer_t* lx, size_t min_bp) {
	cane_ast_node_t* node = NULL;

	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

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
		// TODO: report an error
		cane_report_and_die(
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
			cane_lexer_peek(lx, &symbol, cane_fix_binary_symbol);

			cane_report_and_die(
				CANE_REPORT_SYNTAX, "expected an infix or postfix operator"

			);
		}

		cane_lexer_peek(lx, &symbol, cane_fix_binary_symbol);
	}

	return node;
}

#endif
