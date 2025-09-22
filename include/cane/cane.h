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

////////////
// PARSER //
////////////

typedef struct {
	// Function definitions
	// Bindings
	// Typestack
} cane_context_t;

// static cane_context_t cane_context_create() {
// 	return (cane_context_t){};
// }

// Primary call that sets up lexer and context automatically.
static cane_symbol_t* cane_parse(cane_context_t* ctx, cane_lexer_t* lx);

// Forward declarations for mutual recursion
static cane_symbol_t* cane_parse_program(cane_context_t* ctx, cane_lexer_t* lx);

// static cane_symbol_t*
// cane_parse_expression(cane_context_t* ctx, cane_lexer_t*
// lx);

static cane_symbol_t*
cane_parse_function(cane_context_t* ctx, cane_lexer_t* lx);

static cane_symbol_t* cane_parse_literal(cane_context_t* ctx, cane_lexer_t* lx);

// Convenience functions
typedef bool (*cane_parser_pred_t)(cane_symbol_t);  // For parser predicates

static bool cane_is_operator(cane_symbol_t symbol) {
	return
		// Misc.
		symbol.kind == CANE_SYMBOL_ASSIGN ||
		symbol.kind == CANE_SYMBOL_RHYTHM ||
		symbol.kind == CANE_SYMBOL_REVERSE || symbol.kind == CANE_SYMBOL_MAP ||
		symbol.kind == CANE_SYMBOL_REPEAT ||
		symbol.kind == CANE_SYMBOL_INVERT ||

		// Logical
		symbol.kind == CANE_SYMBOL_OR || symbol.kind == CANE_SYMBOL_XOR ||
		symbol.kind == CANE_SYMBOL_AND || symbol.kind == CANE_SYMBOL_NOT ||

		// Arithmetic
		symbol.kind == CANE_SYMBOL_ADD || symbol.kind == CANE_SYMBOL_SUB ||
		symbol.kind == CANE_SYMBOL_MUL || symbol.kind == CANE_SYMBOL_DIV ||

		// Keywords
		symbol.kind == CANE_SYMBOL_LCM || symbol.kind == CANE_SYMBOL_GCD;
}

static bool cane_is_literal(cane_symbol_t symbol) {
	return symbol.kind == CANE_SYMBOL_NUMBER ||
		symbol.kind == CANE_SYMBOL_STRING;
}

static bool cane_is_expression(cane_symbol_t symbol) {
	return cane_is_literal(symbol) || symbol.kind == CANE_SYMBOL_IDENT ||
		symbol.kind == CANE_SYMBOL_LPAREN ||
		symbol.kind == CANE_SYMBOL_LBRACE ||
		symbol.kind == CANE_SYMBOL_LBRACKET ||
		symbol.kind == CANE_SYMBOL_LCHEVRON;
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

static bool cane_peek_is_expression(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_expression);
}

static bool cane_peek_is_operator(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_operator);
}

static bool cane_peek_is_literal(cane_lexer_t* lx) {
	return cane_peek_is(lx, cane_is_literal);
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

// Implementation of core parsing functions
static cane_symbol_t* cane_parse(cane_context_t* ctx, cane_lexer_t* lx) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	cane_symbol_t* program = cane_parse_program(ctx, lx);

	return program;
}

// Core parsing functions
static cane_symbol_t*
cane_parse_program(cane_context_t* ctx, cane_lexer_t* lx) {
	cane_symbol_t* program =
		NULL;  // TODO: Use allocator API to construct linked list.

	while (cane_peek_is_expression(lx)) {
		// TODO: Concatenate `expr` with `program` to build up a linked list of
		// instructions which will act as our intermediate representation in the
		// compiler.
		// cane_symbol_t* expr = cane_parse_expression(ctx, lx);
	}

	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	cane_expect_kind(
		lx,
		CANE_SYMBOL_ENDFILE,
		"expected EOF but found: '%s'",
		CANE_SYMBOL_TO_STR[symbol.kind]
	);

	return program;
}

// static cane_symbol_t* cane_parse_expression(
// 	cane_context_t* ctx, cane_lexer_t* lx
// ) {
// 	CANE_FUNCTION_ENTER(log);

// 	cane_expect(lx, cane_is_expression, "expected an expression");

// 	cane_symbol_t* expression =
// 		NULL;  // TODO: Use allocator API to construct linked list.

// 	// Integers/strings
// 	if (cane_peek_is_literal(lx)) {
// 		expression = cane_parse_literal(ctx, lx);
// 	}

// 	// Builtin operators/functions
// 	else if (cane_peek_is_operator(lx)) {
// 		expression = cane_parse_operator(ctx, lx);
// 	}

// 	// Symbol reference
// 	else if (cane_peek_is_kind(lx, CANE_IDENT)) {
// 		cane_symbol_t symbol;
// 		cane_lexer_take(lx, &symbol);

// 		// TODO: Handle symbol reference that isn't a builtin
// 		CANE_WHEREAMI(log);
// 	}

// 	// Functions
// 	else if (cane_peek_is_kind(lx, CANE_LBRACKET)) {
// 		expression = cane_parse_function(ctx, lx);
// 	}

// 	// Swizzle
// 	else if (cane_peek_is_kind(lx, CANE_LPAREN)) {
// 		expression = cane_parse_swizzle(ctx, lx);
// 	}

// 	// Type assertion
// 	else if (cane_peek_is_kind(lx, CANE_TYPE)) {
// 		// TODO: Should we even concatenate this expression? Maybe we should
// 		// just discard it.
// 		expression = cane_parse_assertion(ctx, lx);
// 	}

// 	else {
// 		// TODO: Unreachable
// 		cane_symbol_t symbol;
// 		cane_lexer_peek(lx, &symbol);

// 		cane_log(
// 			log,
// 			CANE_PRIORITY_FAIL,
// 			"unexpected token '%s'",
// 			CANE_SYMBOL_TO_STR[symbol.kind]
// 		);

// 		exit(EXIT_FAILURE);
// 	}

// 	return expression;
// }

// static cane_symbol_t*
// cane_parse_function(cane_context_t* ctx, cane_lexer_t* lx)
// { 	CANE_FUNCTION_ENTER(log);

// 	cane_expect_kind(lx, CANE_LBRACKET, "expected '['");
// 	cane_lexer_take(lx, NULL);

// 	cane_symbol_t* function = NULL;

// 	while (cane_peek_is_expression(lx)) {
// 		// TODO: Concatenate `expr` with `function` to build up a linked list of
// 		// instructions which will act as our intermediate representation in the
// 		// compiler.
// 		cane_symbol_t* expr = cane_parse_expression(ctx, lx);
// 	}

// 	cane_expect_kind(lx, CANE_RBRACKET, "expected ']'");
// 	cane_lexer_take(lx, NULL);

// 	return function;
// }

// static cane_symbol_t*
// cane_parse_builtin(cane_context_t* ctx, cane_lexer_t* lx)
// { 	CANE_FUNCTION_ENTER(log);

// 	cane_expect(lx, cane_is_builtin, "expected a built-in");

// 	// TODO: Allocate this node on the heap and return the pointer.
// 	cane_symbol_t symbol;
// 	cane_lexer_take(lx, &symbol);

// 	return NULL;
// }

static cane_symbol_t*
cane_parse_literal(cane_context_t* ctx, cane_lexer_t* lx) {
	cane_expect(lx, cane_is_literal, "expected a literal");

	// TODO: Allocate this node on the heap and return the pointer.
	cane_symbol_t symbol;
	cane_lexer_take(lx, &symbol);

	return NULL;
}

#endif
