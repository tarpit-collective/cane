#ifndef CANE_PARSE_H
#define CANE_PARSE_H

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
	cane_string_view_t sv;

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

typedef struct cane_binding_power cane_binding_power_t;

struct cane_binding_power {
	size_t lhs;
	size_t rhs;
};

static size_t cane_prefix_binding_power(cane_symbol_kind_t s) {
	switch (s) {
		case CANE_SYMBOL_ABS: return 0;
		case CANE_SYMBOL_NEG: return 0;

		case CANE_SYMBOL_INVERT: return 0;
		case CANE_SYMBOL_REVERSE: return 0;

		default: break;
	}

	return 0;
}

static cane_binding_power_t cane_infix_binding_power(cane_symbol_kind_t s) {
	switch (s) {
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

		default: break;
	}

	return (cane_binding_power_t){
		.lhs = 0,
		.rhs = 0,
	};
}

static size_t cane_postfix_binding_power(cane_symbol_kind_t s) {
	switch (s) {
		case CANE_SYMBOL_ASSIGN: return 0;
		default: break;
	}

	return 0;
}

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

static cane_ast_node_t*
cane_parse_expression(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp);

// Convenience functions
static bool cane_is_literal(cane_symbol_kind_t kind) {
	return kind == CANE_SYMBOL_NUMBER || kind == CANE_SYMBOL_STRING ||
		kind == CANE_SYMBOL_REST || kind == CANE_SYMBOL_BEAT;
}

static bool cane_is_primary(cane_symbol_kind_t kind) {
	return cane_is_literal(kind) || kind == CANE_SYMBOL_FUNCTION ||
		kind == CANE_SYMBOL_IDENTIFIER || kind == CANE_SYMBOL_LPAREN ||
		kind == CANE_SYMBOL_CHOICE || kind == CANE_SYMBOL_LAYER;
}

static bool cane_is_prefix(cane_symbol_kind_t kind) {
	return
		// Arithmetic
		kind == CANE_SYMBOL_ABS || kind == CANE_SYMBOL_NEG ||

		kind == CANE_SYMBOL_INVERT ||  // Invert
		kind == CANE_SYMBOL_REVERSE;   // Reverse
}

static bool cane_is_infix(cane_symbol_kind_t kind) {
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

static bool cane_is_postfix(cane_symbol_kind_t kind) {
	return kind == CANE_SYMBOL_ASSIGN;  // Assignment
}

static bool cane_is_expression(cane_symbol_kind_t kind) {
	return cane_is_primary(kind) || cane_is_prefix(kind);
}

// Automatically handle peeking
static bool cane_peek_is_kind(cane_lexer_t* lx, cane_symbol_kind_t kind) {
	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	return symbol.kind == kind;
}

static bool cane_peek_is(cane_lexer_t* lx, cane_symbol_pred_t cond) {
	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	return cond(symbol.kind);
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

		(void*)peeker.sv.begin,
		(void*)peeker.sv.end,

		cane_ptrdiff(peeker.sv.begin, peeker.sv.end),
		cane_ptrdiff(peeker.sv.begin, peeker.sv.end),

		peeker.sv.begin
	);

	va_list args;
	va_start(args, fmt);

	cane_log(log, CANE_PRIORITY_FAIL, fmt, args);

	va_end(args);

	exit(EXIT_FAILURE);
}

static void
cane_expect(cane_lexer_t* lx, cane_symbol_pred_t cond, const char* fmt, ...) {
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
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_lexer_t lx = cane_lexer_create(sv);
	return cane_parse_program(&lx);
}

// Core parsing functions
static cane_ast_node_t* cane_parse_program(cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(cane_logger_create_default());

	cane_ast_node_t* node = NULL;

	// TODO: To keep things simple, we can use a binary tree for everything
	// and join statements together with a CANE_SYMBOL_STATEMENT node.
	// We can just evaluate both sides in isolation.
	// Basically a "cons" list like in lisp.

	cane_symbol_t symbol;
	cane_lexer_peek(lx, &symbol);

	while (symbol.kind != CANE_SYMBOL_ENDFILE) {
		// cane_ast_node_t* expr = cane_parse_expression(lx, 0);

		node = cane_parse_expression(lx, symbol, 0);
		cane_lexer_peek(lx, &symbol);
	}

	cane_expect_kind(lx, CANE_SYMBOL_ENDFILE, "expected end of file");

	return node;
}

// Expression parsing
static cane_ast_node_t*
cane_parse_primary(cane_lexer_t* lx, cane_symbol_t symbol) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	switch (symbol.kind) {
		// Literals
		case CANE_SYMBOL_IDENTIFIER:
		case CANE_SYMBOL_NUMBER:

		case CANE_SYMBOL_BEAT:
		case CANE_SYMBOL_REST: {
			// TODO: How we should we handle chains of ! and . without implicit
			// concat?
			cane_lexer_take(lx, &symbol);
			return cane_ast_node_create(symbol.kind, symbol.sv);
		} break;

		case CANE_SYMBOL_LPAREN: {
			cane_lexer_take(lx, &symbol);

			cane_symbol_t symbol;
			cane_lexer_peek(lx, &symbol);

			cane_ast_node_t* expr = cane_parse_expression(lx, symbol, 0);

			cane_expect_kind(lx, CANE_SYMBOL_RPAREN, "expected `)`");
			cane_lexer_take(lx, NULL);

			return expr;
		} break;

		case CANE_SYMBOL_CHOICE:
		case CANE_SYMBOL_LAYER: {
			CANE_UNIMPLEMENTED(log);
		} break;

		case CANE_SYMBOL_FUNCTION: {
			cane_lexer_take(lx, &symbol);  // Skip `\`

			cane_expect_kind(
				lx, CANE_SYMBOL_IDENTIFIER, "expected an identifier"
			);

			cane_symbol_t ident;
			cane_lexer_take(lx, &ident);

			cane_ast_node_t* param =
				cane_ast_node_create(CANE_SYMBOL_PARAM, ident.sv);

			// Reset binding power
			cane_symbol_t symbol;
			cane_lexer_peek(lx, &symbol);

			cane_ast_node_t* body = cane_parse_expression(lx, symbol, 0);

			cane_ast_node_t* fn =
				cane_ast_node_create(CANE_SYMBOL_FUNCTION, ident.sv);

			fn->lhs = param;
			fn->rhs = body;

			return fn;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	CANE_DIE(
		log,
		"expected a primary expression `%s`",
		CANE_SYMBOL_KIND_TO_STR[symbol.kind]
	);

	return NULL;
}

static cane_ast_node_t*
cane_parse_prefix(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	switch (symbol.kind) {
		case CANE_SYMBOL_ABS:
		case CANE_SYMBOL_NEG:

		case CANE_SYMBOL_INVERT:
		case CANE_SYMBOL_REVERSE: {
			cane_lexer_take(lx, &symbol);

			cane_ast_node_t* node =
				cane_ast_node_create(symbol.kind, symbol.sv);

			cane_symbol_t symbol;
			cane_lexer_peek(lx, &symbol);

			cane_ast_node_t* expr = cane_parse_expression(lx, symbol, bp);

			node->lhs = NULL;
			node->rhs = expr;

			return node;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	CANE_DIE(
		log,
		"expected a prefix operator `%s`",
		CANE_SYMBOL_KIND_TO_STR[symbol.kind]
	);

	return NULL;
}

static cane_ast_node_t* cane_parse_infix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs, size_t bp
) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	// TODO: Can we ignore this switch entirely and just do the check with
	// `cane_is_infix` and then allocate the node the exact same way?
	// It's redundant.
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
			cane_ast_node_t* node =
				cane_ast_node_create(symbol.kind, symbol.sv);

			cane_lexer_take(lx, &symbol);

			cane_symbol_t symbol;
			cane_lexer_peek(lx, &symbol);

			cane_ast_node_t* rhs = cane_parse_expression(lx, symbol, bp);

			node->lhs = lhs;
			node->rhs = rhs;

			return node;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	CANE_DIE(
		log,
		"expected an infix operator `%s`",
		CANE_SYMBOL_KIND_TO_STR[symbol.kind]
	);

	return NULL;
}

static cane_ast_node_t* cane_parse_postfix(
	cane_lexer_t* lx, cane_symbol_t symbol, cane_ast_node_t* lhs
) {
	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	switch (symbol.kind) {
		case CANE_SYMBOL_ASSIGN: {
			cane_lexer_take(lx, &symbol);

			cane_ast_node_t* node =
				cane_ast_node_create(symbol.kind, symbol.sv);

			// We put the child node on the `rhs` to be consistent with prefix
			// expressions so we don't have to have seperate handling for them.
			node->lhs = NULL;
			node->rhs = lhs;

			return node;
		} break;

		default: break;
	}

	// TODO: Report an error here.
	CANE_DIE(
		log,
		"expected a postfix operator `%s`",
		CANE_SYMBOL_KIND_TO_STR[symbol.kind]
	);

	return NULL;
}

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

		default: break;
	}

	CANE_LOG_WARN(
		cane_logger_create_default(),
		"fixup unary %s => %s",
		CANE_SYMBOL_KIND_TO_STR[kind],
		CANE_SYMBOL_KIND_TO_STR[new_kind]
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

		default: break;
	}

	CANE_LOG_WARN(
		cane_logger_create_default(),
		"fixup binary %s => %s",
		CANE_SYMBOL_KIND_TO_STR[kind],
		CANE_SYMBOL_KIND_TO_STR[new_kind]
	);

	return new_kind;
}

static cane_ast_node_t*
cane_parse_expression(cane_lexer_t* lx, cane_symbol_t symbol, size_t bp) {
	cane_ast_node_t* node = NULL;

	cane_logger_t log = cane_logger_create_default();
	CANE_FUNCTION_ENTER(log);

	symbol.kind = cane_fix_unary_symbol(symbol.kind);

	if (cane_is_primary(symbol.kind)) {
		node = cane_parse_primary(lx, symbol);
	}

	else if (cane_is_prefix(symbol.kind)) {
		size_t rbp = cane_prefix_binding_power(symbol.kind);
		node = cane_parse_prefix(lx, symbol, rbp);
	}

	else {
		// TODO: report an error
		CANE_DIE(
			log,
			"expected a primary expression or a prefix operator `%s`",
			CANE_SYMBOL_KIND_TO_STR[symbol.kind]
		);
	}

	// State has changed since we called prefix/primary parser functions
	// so we need to peek again.
	cane_lexer_peek(lx, &symbol);
	symbol.kind = cane_fix_binary_symbol(symbol.kind);

	while (cane_is_infix(symbol.kind) || cane_is_postfix(symbol.kind) ||
		   cane_is_expression(symbol.kind)) {
		// Two expressions juxtaposed is a function call
		// if (cane_is_expression(symbol)) {
		// 	symbol.kind = CANE_SYMBOL_CALL;
		// }

		if (cane_is_postfix(symbol.kind)) {
			size_t lbp = cane_postfix_binding_power(symbol.kind);

			if (lbp < bp) {
				break;
			}

			node = cane_parse_postfix(lx, symbol, node);
		}

		else if (cane_is_infix(symbol.kind)) {
			cane_binding_power_t binding_power =
				cane_infix_binding_power(symbol.kind);

			if (binding_power.lhs < bp) {
				break;
			}

			node = cane_parse_infix(lx, symbol, node, binding_power.rhs);
		}

		else {
			cane_lexer_peek(lx, &symbol);

			// TODO: report an error
			CANE_DIE(
				log,
				"expected an infix or postfix operator `%s`",
				CANE_SYMBOL_KIND_TO_STR[symbol.kind]
			);
		}

		cane_lexer_peek(lx, &symbol);
		symbol.kind = cane_fix_binary_symbol(symbol.kind);
	}

	return node;
}

#endif
