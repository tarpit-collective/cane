#ifndef CANE_H
#define CANE_H

#include <stdbool.h>
#include <stddef.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <string.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/lex.h>

/////////
// AST //
/////////

typedef struct cane_ast_node cane_ast_node_t;

struct cane_ast_node {
	cane_symbol_kind_t kind;

	cane_ast_node_t* lhs;
	cane_ast_node_t* rhs;
};

////////////
// PARSER //
////////////

typedef struct cane_parser cane_parser_t;

struct cane_parser {
	// Function definitions
	// Bindings
	// Typestack
};

static cane_parser_t cane_parser_create() {
	return (cane_parser_t){};
}

// Primary call that sets up lexer and context automatically.
static cane_ast_node_t* cane_parse(cane_string_view_t sv);

// Forward declarations for mutual recursion
static cane_ast_node_t* cane_parse_program(cane_parser_t* p, cane_lexer_t* lx);
static cane_ast_node_t* cane_parse_literal(cane_parser_t* p, cane_lexer_t* lx);

// Expression parsing
static cane_ast_node_t* cane_parse_primary(cane_parser_t* p, cane_lexer_t* lx);

static cane_ast_node_t*
cane_parse_prefix(cane_parser_t* p, cane_lexer_t* lx, size_t bp);

static cane_ast_node_t* cane_parse_infix(
	cane_parser_t* p, cane_lexer_t* lx, cane_ast_node_t* lhs, size_t bp
);

static cane_ast_node_t*
cane_parse_postfix(cane_parser_t* p, cane_lexer_t* lx, cane_ast_node_t* lhs);

static cane_ast_node_t*
cane_parse_expression(cane_parser_t* p, cane_lexer_t* lx, size_t bp);

// Convenience functions
typedef bool (*cane_parser_pred_t)(cane_symbol_t);  // For parser predicates

static bool cane_is_literal(cane_symbol_t symbol) {
	return symbol.kind == CANE_SYMBOL_NUMBER ||
		symbol.kind == CANE_SYMBOL_STRING || symbol.kind == CANE_SYMBOL_DOT ||
		symbol.kind == CANE_SYMBOL_EXCLAIM;
}

static bool cane_is_primary(cane_symbol_t symbol) {
	return cane_is_literal(symbol) || symbol.kind == CANE_SYMBOL_BACKSLASH ||
		symbol.kind == CANE_SYMBOL_IDENTIFIER ||
		symbol.kind == CANE_SYMBOL_LPAREN ||
		symbol.kind == CANE_SYMBOL_LBRACE ||
		symbol.kind == CANE_SYMBOL_LBRACKET;
}

static bool cane_is_prefix(cane_symbol_t symbol) {
	return
		// Arithmetic
		symbol.kind == CANE_SYMBOL_ADD || symbol.kind == CANE_SYMBOL_SUB ||

		// Time divisions
		symbol.kind == CANE_SYMBOL_MUL || symbol.kind == CANE_SYMBOL_DIV ||

		symbol.kind == CANE_SYMBOL_TILDA ||  // Invert
		symbol.kind == CANE_SYMBOL_QUOTE     // Reverse
		;
}

static bool cane_is_infix(cane_symbol_t symbol) {
	return
		// Arithmetic
		symbol.kind == CANE_SYMBOL_ADD || symbol.kind == CANE_SYMBOL_SUB ||
		symbol.kind == CANE_SYMBOL_MUL || symbol.kind == CANE_SYMBOL_DIV ||

		symbol.kind == CANE_SYMBOL_LCM || symbol.kind == CANE_SYMBOL_GCD ||

		// Misc.
		symbol.kind == CANE_SYMBOL_COLON ||  // Euclide
		symbol.kind == CANE_SYMBOL_STARS ||  // Repeat
		symbol.kind == CANE_SYMBOL_AT ||     // Map
		symbol.kind == CANE_SYMBOL_DOT ||    // Concatenate

		// Logic
		symbol.kind == CANE_SYMBOL_OR || symbol.kind == CANE_SYMBOL_XOR ||
		symbol.kind == CANE_SYMBOL_AND ||

		// Left/Right Shift
		symbol.kind == CANE_SYMBOL_LCHEVRON ||
		symbol.kind == CANE_SYMBOL_RCHEVRON;
}

static bool cane_is_postfix(cane_symbol_t symbol) {
	return symbol.kind == CANE_SYMBOL_ARROW  // Assignment
		;
}

static bool cane_is_expression(cane_symbol_t symbol) {
	return cane_is_primary(symbol) || cane_is_prefix(symbol);
}

// Automatically handle peeking
static bool cane_peek_is_kind(cane_lexer_t* lx, cane_symbol_kind_t kind) {
	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	return symbol.kind == kind;
}

static bool cane_peek_is(cane_lexer_t* lx, cane_parser_pred_t cond) {
	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	return cond(symbol);
}

static bool cane_peek_is_literal(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_literal);
}

static bool cane_peek_is_primary(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_primary);
}

static bool cane_peek_is_prefix(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_prefix);
}

static bool cane_peek_is_infix(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_infix);
}

static bool cane_peek_is_postfix(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_postfix);
}

static bool cane_peek_is_expression(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_expression);
}

// Parsing utilities
static void cane_expect_kind(
	cane_lexer_t* lx, cane_symbol_kind_t kind, const char* fmt, ...
) {
	// TODO: Report line info from cane lexer. (use cane_log_info function).
	cane_logger_t log = cane_logger_create_default();

	if (cane_peek_is_kind(lx, kind)) {
		return;
	}

	cane_symbol_t peeker;
	cane_lexer_peek(lx, &peeker);

	CANE_LOG_INFO(
		log,

		"expected = %s, kind = %s, ptr = %p, end = %p, size = %lu, str = "
		"'%.*s'",

		CANE_SYMBOL_KIND_TO_STR[kind],
		CANE_SYMBOL_KIND_TO_STR[peeker.kind],

		(void*)peeker.str.begin,
		(void*)peeker.str.end,

		cane_ptrdiff(peeker.str.begin, peeker.str.end),
		cane_ptrdiff(peeker.str.begin, peeker.str.end),

		peeker.str.begin
	);

	va_list args;
	va_start(args, fmt);

	cane_log(log, CANE_PRIORITY_FAIL, fmt, args);

	va_end(args);

	exit(EXIT_FAILURE);
}

static void
cane_expect(cane_lexer_t* lx, cane_parser_pred_t cond, const char* fmt, ...) {
	// TODO: Report line info from cane lexer. (use cane_log_info function).
	cane_logger_t log = cane_logger_create_default();

	if (cane_peek_is(lx, cond)) {
		return;
	}

	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(log, CANE_PRIORITY_FAIL, NULL, NULL, NULL, fmt, args);

	va_end(args);

	exit(EXIT_FAILURE);
}

//////////////////////
// PARSER FUNCTIONS //
//////////////////////

static cane_ast_node_t* cane_parse(cane_string_view_t sv) {
	cane_lexer_t lx = cane_lexer_create(sv);
	cane_parser_t p = cane_parser_create();

	cane_ast_node_t* program = cane_parse_program(&p, &lx);

	return program;
}

// Core parsing functions
static cane_ast_node_t* cane_parse_program(cane_parser_t* p, cane_lexer_t* lx) {
	// TODO: To keep things simple, we can use a binary tree for everything
	// and join statements together with a CANE_SYMBOL_STATEMENT node.
	// We can just evaluate both sides in isolation.
	// Basically a "cons" list like in lisp.
	while (cane_peek_is_expression(lx)) {
		cane_ast_node_t* expr = cane_parse_expression(p, lx, 0);
	}

	cane_expect_kind(lx, CANE_SYMBOL_ENDFILE, "expected end of file");

	return NULL;
}

static cane_ast_node_t* cane_parse_literal(cane_parser_t* p, cane_lexer_t* lx) {
	cane_expect(lx, cane_is_literal, "expected a literal");

	cane_symbol_t symbol;
	cane_lexer_take(lx, &symbol);

	return NULL;
}

// Expression parsing
static cane_ast_node_t* cane_parse_primary(cane_parser_t* p, cane_lexer_t* lx) {
	cane_logger_t log = cane_logger_create_default();
	cane_symbol_t symbol;

	cane_expect(lx, cane_is_primary, "expected a primary expression");
	cane_lexer_take(lx, &symbol);

	switch (symbol.kind) {
		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER:

		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST: {
			// TODO: Allocate node and set lhs/rhs to NULL
			// How we should we handle chains of ! and . without implicit
			// concat?
			return NULL;
		} break;

		case CANE_SYMBOL_LPAREN: {
			cane_ast_node_t* expr = cane_parse_expression(p, lx, 0);

			cane_expect_kind(lx, CANE_SYMBOL_RPAREN, "expected `)`");
			cane_lexer_take(lx, NULL);

			return expr;
		} break;

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER: {
			CANE_UNIMPLEMENTED(log);
		} break;

		case CANE_SYMBOL_FUNCTION: {
			cane_symbol_t ident;

			cane_expect_kind(
				lx, CANE_SYMBOL_IDENTIFIER, "expected an identifier"
			);

			cane_lexer_take(lx, &ident);

			// Reset binding power
			cane_ast_node_t* expr = cane_parse_expression(p, lx, 0);

			return NULL;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	cane_die(log, NULL, NULL, NULL, "expected a primary expression");

	return NULL;
}

static cane_ast_node_t*
cane_parse_prefix(cane_parser_t* p, cane_lexer_t* lx, size_t bp) {
	cane_symbol_t symbol;
	cane_expect(lx, cane_is_prefix, "expected a prefix operator");
	cane_lexer_take(lx, &symbol);

	switch (symbol.kind) {
		case CANE_SYMBOL_ABS:
		case CANE_SYMBOL_NEG:

		case CANE_SYMBOL_TIMEMUL:
		case CANE_SYMBOL_TIMEDIV:

		case CANE_SYMBOL_INVERT:
		case CANE_SYMBOL_REVERSE: {
			cane_ast_node_t* rhs = cane_parse_expression(p, lx, bp);

			return NULL;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	cane_logger_t log = cane_logger_create_default();
	cane_die(log, NULL, NULL, NULL, "expected a prefix operator");

	return NULL;
}

static cane_ast_node_t* cane_parse_infix(
	cane_parser_t* p, cane_lexer_t* lx, cane_ast_node_t* lhs, size_t bp
) {
	cane_symbol_t symbol;
	cane_expect(lx, cane_is_infix, "expected an infix operator");
	cane_lexer_take(lx, &symbol);

	switch (symbol.kind) {
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
			// TODO: allocate node and set lhs and rhs pointers.
			cane_ast_node_t* rhs = cane_parse_expression(p, lx, bp);
			return NULL;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	cane_logger_t log = cane_logger_create_default();
	cane_die(log, NULL, NULL, NULL, "expected an infix operator");

	return NULL;
}

static cane_ast_node_t*
cane_parse_postfix(cane_parser_t* p, cane_lexer_t* lx, cane_ast_node_t* lhs) {
	cane_symbol_t symbol;
	cane_expect(lx, cane_is_postfix, "expected a postfix operator");
	cane_lexer_take(lx, &symbol);

	switch (symbol.kind) {
		case CANE_SYMBOL_ASSIGN: {
			// TODO: expression already consumed and operator token discarded.
			// Just need to create an AST node.
			return NULL;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	cane_logger_t log = cane_logger_create_default();
	cane_die(log, NULL, NULL, NULL, "expected a postfix operator");

	return NULL;
}

static cane_ast_node_t*
cane_parse_expression(cane_parser_t* p, cane_lexer_t* lx, size_t bp) {
	cane_logger_t log = cane_logger_create_default();

	cane_ast_node_t* node = NULL;

	cane_symbol_t symbol;
	cane_lexer_take(lx, &symbol);

	// Convert symbols in prefix location.
	// Makes it easier to reason about operator kinds during parsing and when
	// constructing the AST.
	switch (symbol.kind) {
		case CANE_SYMBOL_ADD: symbol.kind = CANE_SYMBOL_ABS; break;
		case CANE_SYMBOL_SUB: symbol.kind = CANE_SYMBOL_NEG; break;

		case CANE_SYMBOL_MUL: symbol.kind = CANE_SYMBOL_TIMEMUL; break;
		case CANE_SYMBOL_DIV: symbol.kind = CANE_SYMBOL_TIMEDIV; break;

		case CANE_SYMBOL_LBRACE: symbol.kind = CANE_SYMBOL_CHOICE; break;
		case CANE_SYMBOL_LBRACKET: symbol.kind = CANE_SYMBOL_LAYER; break;

		default: break;
	}

	if (cane_is_prefix(symbol)) {
		size_t rbp = 0;  // TODO: Get binding power for operator
		node = cane_parse_prefix(p, lx, rbp);
	}

	else if (cane_is_primary(symbol)) {
		node = cane_parse_primary(p, lx);
	}

	else {
		// TODO: report an error
		cane_die(
			log,
			NULL,
			NULL,
			NULL,
			"expected a primary expression or a prefix operator"
		);
	}

	cane_lexer_peek(lx, &symbol);

	while (cane_is_infix(symbol) || cane_is_postfix(symbol) ||
		   cane_is_expression(symbol)) {
		// Two expressions juxtaposed is a function call
		if (cane_is_expression(symbol)) {
			symbol.kind = CANE_SYMBOL_CALL;
		}

		size_t lbp = 0;
		size_t rbp = 0;

		if (lbp < bp) {
			break;
		}

		if (cane_is_postfix(symbol)) {
			node = cane_parse_postfix(p, lx, node);
		}

		else if (cane_is_infix(symbol)) {
			node = cane_parse_infix(p, lx, node, rbp);
		}

		else {
			// TODO: report an error
			cane_die(
				log, NULL, NULL, NULL, "expected an infix or postfix operator"
			);
		}

		cane_lexer_peek(lx, &symbol);
	}

	return node;
}

#endif
