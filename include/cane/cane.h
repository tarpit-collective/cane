#ifndef CANE_H
#define CANE_H

#include <stdbool.h>
#include <stddef.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <ctype.h>
#include <string.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>

////////////
// TOKENS //
////////////

#define SYMBOLS \
	X(CANE_SYMBOL_NONE, "none") \
	X(CANE_SYMBOL_ENDFILE, "end of file") \
\
	X(CANE_SYMBOL_WHITESPACE, "whitespace") \
	X(CANE_SYMBOL_COMMENT, "comment") \
\
	/* Atoms */ \
	X(CANE_SYMBOL_NUMBER, "number") \
	X(CANE_SYMBOL_STRING, "string") \
	X(CANE_SYMBOL_IDENT, "ident") \
\
	X(CANE_SYMBOL_BEAT, "beat `!`") \
	X(CANE_SYMBOL_REST, "rest `.`") \
\
	/* Keywords */ \
	X(CANE_SYMBOL_LCM, "lcm") \
	X(CANE_SYMBOL_GCD, "gcd") \
\
	/* Operators */ \
	X(CANE_SYMBOL_ASSIGN, "assign `=>`") \
	X(CANE_SYMBOL_RHYTHM, "rhythm `:`") \
	X(CANE_SYMBOL_REVERSE, "reverse `'`") \
	X(CANE_SYMBOL_MAP, "map `@`") \
	X(CANE_SYMBOL_REPEAT, "repeat `**`") \
	X(CANE_SYMBOL_INVERT, "invert `~`") \
\
	X(CANE_SYMBOL_OR, "or") \
	X(CANE_SYMBOL_XOR, "xor") \
	X(CANE_SYMBOL_AND, "and") \
	X(CANE_SYMBOL_NOT, "not") \
\
	X(CANE_SYMBOL_ADD, "add `+`") \
	X(CANE_SYMBOL_SUB, "sub `-`") \
	X(CANE_SYMBOL_MUL, "mul `*`") \
	X(CANE_SYMBOL_DIV, "div `/`") \
\
	/* Grouping */ \
	X(CANE_SYMBOL_LPAREN, "lparen `(`") \
	X(CANE_SYMBOL_RPAREN, "rparen `)`") \
\
	X(CANE_SYMBOL_LBRACE, "lbrace `{`") \
	X(CANE_SYMBOL_RBRACE, "rbrace `}`") \
\
	X(CANE_SYMBOL_LBRACKET, "lbracket `[`") \
	X(CANE_SYMBOL_RBRACKET, "rbracket `]`") \
\
	X(CANE_SYMBOL_LCHEVRON, "lchevron `<`") \
	X(CANE_SYMBOL_RCHEVRON, "rchevron `>`")

#define X(x, y) x,

typedef enum {
	SYMBOLS
} cane_symbol_kind_t;

#undef X

#define X(x, y) [x] = #x,
const char* CANE_SYMBOL_KIND_TO_STR[] = {SYMBOLS};
#undef X

#define X(x, y) [x] = y,
const char* CANE_SYMBOL_TO_STR[] = {SYMBOLS};
#undef X

#undef SYMBOLS

typedef struct cane_symbol_t cane_symbol_t;

struct cane_symbol_t {
	cane_symbol_kind_t kind;

	struct {
		const char* ptr;
		const char* end;
	} str;
};

///////////
// LEXER //
///////////

typedef bool (*cane_lexer_pred_t)(char);  // Used for lexer predicates

typedef struct cane_lexer cane_lexer_t;

struct cane_lexer {
	const char* const src;

	const char* ptr;
	const char* end;

	cane_symbol_t peek;
};

static bool
cane_lexer_take(cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol);

// static cane_lexer_t
// cane_lexer_create(cane_logger_t log, const char* ptr, const char* end) {
// 	cane_lexer_t lx = (cane_lexer_t){
// 		.src = ptr,

// 		.ptr = ptr,
// 		.end = end,

// 		.peek =
// 			(cane_symbol_t){
// 				.kind = CANE_SYMBOL_NONE,
// 				.str.ptr = ptr,
// 				.str.end = ptr,
// 			},
// 	};

// 	cane_lexer_take(log, &lx, NULL);

// 	return lx;
// }

// Basic stream interaction
static char cane_lexer_char_peek(cane_lexer_t* lx) {
	if (lx->ptr >= lx->end) {
		return '\0';
	}

	return *lx->ptr;
}

static char cane_lexer_char_take(cane_lexer_t* lx) {
	if (lx->ptr >= lx->end) {
		return '\0';
	}

	return *lx->ptr++;
}

// Conditional consumers
static cane_status_t
cane_lexer_take_if(cane_lexer_t* lx, cane_lexer_pred_t cond) {
	char c = cane_lexer_char_peek(lx);

	if (!c || !cond(c)) {
		return cane_fail();
	}

	cane_lexer_char_take(lx);
	return cane_okay();
}

// Same as take_if but just takes a character directly
// for common usecases.
static cane_status_t cane_lexer_take_ifc(cane_lexer_t* lx, char c) {
	if (c != cane_lexer_char_peek(lx)) {
		return cane_fail();
	}

	cane_lexer_char_take(lx);
	return cane_okay();
}

static cane_status_t cane_lexer_take_str(cane_lexer_t* lx, const char* str) {
	size_t length = strlen(str);

	if (lx->ptr + length >= lx->end) {
		return cane_fail();
	}

	// TODO: Use custom strncmp to avoid iterating strings twice.
	if (strncmp(lx->ptr, str, length) != 0) {
		return cane_fail();
	}

	lx->ptr += length;
	return cane_okay();
}

static cane_status_t
cane_lexer_take_while(cane_lexer_t* lx, cane_lexer_pred_t cond) {
	bool taken = false;

	while (cane_lexer_take_if(lx, cond).is_okay) {
		taken = true;
	}

	return (cane_status_t){.is_okay = taken};
}

// Lexer predicates
static bool cane_is_whitespace(char c) {
	return isspace(c);
}

static bool cane_is_comment(char c) {
	return c != '\n';
}

static bool cane_is_alpha(char c) {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool cane_is_digit(char c) {
	return c >= '0' && c <= '9';
}

static bool cane_is_alphanum(char c) {
	return cane_is_alpha(c) || cane_is_digit(c);
}

static bool cane_is_ident(char c) {
	return cane_is_alphanum(c) || c == '_';
}

// Token producers
// These functions wrap the basic "lexer_take" functions so that we can
// wrap them into a symbol type/token.
static bool cane_lexer_produce_if(
	cane_logger_t log,
	cane_lexer_t* lx,
	cane_symbol_t* symbol,
	cane_symbol_kind_t kind,
	cane_lexer_pred_t cond
) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	if (!cane_lexer_take_if(lx, cond).is_okay) {
		return false;
	}

	next_symbol.str.end = lx->ptr;
	next_symbol.kind = kind;

	if (symbol != NULL) {
		*symbol = next_symbol;
	}

	return true;
}

static bool cane_lexer_produce_while(
	cane_logger_t log,
	cane_lexer_t* lx,
	cane_symbol_t* symbol,
	cane_symbol_kind_t kind,
	cane_lexer_pred_t cond
) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	if (!cane_lexer_take_while(lx, cond).is_okay) {
		return false;
	}

	next_symbol.str.end = lx->ptr;
	next_symbol.kind = kind;

	if (symbol != NULL) {
		*symbol = next_symbol;
	}

	return true;
}

static bool cane_lexer_produce_str(
	cane_logger_t log,
	cane_lexer_t* lx,
	cane_symbol_t* symbol,
	cane_symbol_kind_t kind,
	const char* str
) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	if (!cane_lexer_take_str(lx, str).is_okay) {
		return false;
	}

	next_symbol.str.end = lx->ptr;
	next_symbol.kind = kind;

	if (symbol != NULL) {
		*symbol = next_symbol;
	}

	return true;
}

// Cane specific lexer functions
static bool cane_lexer_produce_ident(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol
) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,

		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	if (!cane_lexer_take_if(lx, cane_is_alpha).is_okay) {
		return false;
	}

	cane_lexer_take_while(lx, cane_is_ident);

	next_symbol.str.end = lx->ptr;

	// We could have used `cane_produce_str` here to handle these cases
	// but we want "maximal munch" meaning that we lex the entire
	// identifier before trying to classify it. Why? because if we
	// didn't, an identifier like "letfoo" would actually be lexed
	// as 2 seperate tokens because it sees `let` and stops there.

	// Keywords
	if (strcmp(next_symbol.str.ptr, "lcm") != 0) {
		next_symbol.kind = CANE_SYMBOL_LCM;
	}

	else if (strcmp(next_symbol.str.ptr, "gcd") != 0) {
		next_symbol.kind = CANE_SYMBOL_GCD;
	}

	// Operators
	else if (strcmp(next_symbol.str.ptr, "or") != 0) {
		next_symbol.kind = CANE_SYMBOL_OR;
	}

	else if (strcmp(next_symbol.str.ptr, "xor") != 0) {
		next_symbol.kind = CANE_SYMBOL_XOR;
	}

	else if (strcmp(next_symbol.str.ptr, "and") != 0) {
		next_symbol.kind = CANE_SYMBOL_AND;
	}

	else if (strcmp(next_symbol.str.ptr, "not") != 0) {
		next_symbol.kind = CANE_SYMBOL_NOT;
	}

	// User identifier
	else {
		next_symbol.kind = CANE_SYMBOL_IDENT;
	}

	if (symbol != NULL) {
		*symbol = next_symbol;
	}

	return true;
}

static bool cane_lexer_produce_number(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol
) {
	return cane_lexer_produce_while(
		log, lx, symbol, CANE_SYMBOL_NUMBER, cane_is_digit
	);
}

static bool cane_lexer_produce_sigil(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol
) {
	// clang-format off
	#define CANE_PRODUCE_SIGIL(sym, str) \
		cane_lexer_produce_str(log, lx, symbol, sym, str)

	return
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_BEAT,  "!") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REST,  ".") ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ASSIGN,  "=>") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RHYTHM,  ":")  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REVERSE, "'")  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_MAP,     "@")  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REPEAT,  "**") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_INVERT,  "~")  ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ADD, "+") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_SUB, "-") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_MUL, "*") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_DIV, "/") ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LPAREN,   "(") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RPAREN,   ")") ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LBRACE,   "{") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RBRACE,   "}") ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LBRACKET, "[") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RBRACKET, "]") ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LCHEVRON, "<") ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RCHEVRON, ">")
	;

	#undef CANE_PRODUCE_SIGIL
	// clang-fcane_fail()ormat on
}

static bool cane_lexer_produce_whitespace(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol
) {
	return cane_lexer_produce_while(
		log, lx, symbol, CANE_SYMBOL_WHITESPACE, cane_is_whitespace
	);
}

static bool cane_lexer_produce_comment(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol
) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,

		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	if (!cane_lexer_take_str(lx, "#!").is_okay) {
		return false;
	}

	if (!cane_lexer_take_while(lx, cane_is_comment).is_okay) {
		return false;
	}

	next_symbol.str.end = lx->ptr;
	next_symbol.kind = CANE_SYMBOL_COMMENT;

	if (symbol != NULL) {
		*symbol = next_symbol;
	}

	return true;
}

// Core lexer interface
// TODO: Reconsider implementation. Is it safe to always return true?
static bool
cane_lexer_peek(cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol) {
	if (symbol != NULL) {
		*symbol = lx->peek;
	}

	return true;
}

// TODO: Make lexer_next produce all tokens and then wrap it in
// another function which skips whitespace and comments.
static bool
cane_lexer_take(cane_logger_t log, cane_lexer_t* lx, cane_symbol_t* symbol) {
	cane_symbol_t next_symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,

		.str.ptr = lx->ptr,
		.str.end = lx->ptr,
	};

	while (cane_lexer_produce_whitespace(log, lx, NULL) ||
		   cane_lexer_produce_comment(log, lx, NULL)) {}

	// Handle EOF
	if (lx->ptr >= lx->end) {
		next_symbol.kind = CANE_SYMBOL_ENDFILE;
	}

	// Handle normal tokens
	else if (!(cane_lexer_produce_ident(log, lx, &next_symbol) ||
			   cane_lexer_produce_number(log, lx, &next_symbol) ||
			   cane_lexer_produce_sigil(log, lx, &next_symbol))) {
		cane_log(log, CANE_PRIORITY_FAIL, "unknown character");
		return false;
	}

	// Return previously peeked token and then store newly
	// lexed token to be used on the next call to peek.
	if (symbol != NULL) {
		cane_lexer_peek(log, lx, symbol);
	}

	lx->peek = next_symbol;
	return true;
}

////////////
// PARSER //
////////////

typedef struct {
	// Function definitions
	// Bindings
	// Typestack
} cane_context_t;

static cane_context_t cane_context_create() {
	return (cane_context_t){};
}

// Primary call that sets up lexer and context automatically.
static cane_symbol_t*
cane_parse(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);

// Forward declarations for mutual recursion
// TODO: Make these functions return cane_instr_t* linked lists.
static cane_symbol_t*
cane_parse_program(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_expression(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_function(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_builtin(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_literal(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_type(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_fntype(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_assertion(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);
static cane_symbol_t*
cane_parse_swizzle(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx);

// Convenience functions
typedef bool (*cane_parser_pred_t)(cane_symbol_t
);  // Used for parser predicates

static bool cane_is_builtin(cane_symbol_t symbol) {
	return
		// Logical
		symbol.kind == CANE_OR || symbol.kind == CANE_AND ||
		symbol.kind == CANE_NOT ||

		// Arithmetic
		symbol.kind == CANE_ADD || symbol.kind == CANE_SUB ||
		symbol.kind == CANE_MUL || symbol.kind == CANE_DIV ||

		// Other
		symbol.kind == CANE_COND || symbol.kind == CANE_APPLY ||
		symbol.kind == CANE_EQUAL;
}

static bool cane_is_literal(cane_symbol_t symbol) {
	return symbol.kind == CANE_NUMBER || symbol.kind == CANE_STRING;
}

static bool cane_is_primitive(cane_symbol_t symbol) {
	return symbol.kind == CANE_TYPE_NUMBER || symbol.kind == CANE_TYPE_STRING ||
		symbol.kind == CANE_TYPE_ANY;
}

static bool cane_is_expression(cane_symbol_t symbol) {
	return cane_is_literal(symbol) || cane_is_builtin(symbol) ||
		symbol.kind == CANE_IDENT ||     // Call
		symbol.kind == CANE_LBRACKET ||  // Function
		symbol.kind == CANE_TYPE ||      // Type assertion
		symbol.kind == CANE_LPAREN;      // Swizzle
}

// Automatically handle peeking
static bool cane_peek_is_kind(
	cane_logger_t log, cane_lexer_t* lx, cane_symbol_kind_t kind
) {
	cane_symbol_t symbol;
	cane_lexer_peek(log, lx, &symbol);

	return symbol.kind == kind;
}

static bool
cane_peek_is(cane_logger_t log, cane_lexer_t* lx, cane_parser_pred_t cond) {
	cane_symbol_t symbol;
	cane_lexer_peek(log, lx, &symbol);

	return cond(symbol);
}

static bool cane_peek_is_expression(cane_logger_t log, cane_lexer_t* lx) {
	return cane_peek_is(log, lx, cane_is_expression);
}

static bool cane_peek_is_builtin(cane_logger_t log, cane_lexer_t* lx) {
	return cane_peek_is(log, lx, cane_is_builtin);
}

static bool cane_peek_is_literal(cane_logger_t log, cane_lexer_t* lx) {
	return cane_peek_is(log, lx, cane_is_literal);
}

static bool cane_peek_is_primitive(cane_logger_t log, cane_lexer_t* lx) {
	return cane_peek_is(log, lx, cane_is_primitive);
}

static bool cane_peek_is_type(cane_logger_t log, cane_lexer_t* lx) {
	return cane_peek_is_primitive(log, lx) ||
		cane_peek_is_kind(log, lx, CANE_TYPE_FN);
}

// Parsing utilities
// TODO: `cane_expect_kind` should call `cane_expect_kind` to avoid redundant
// code.
static void cane_expect_kind(
	cane_logger_t log,
	cane_lexer_t* lx,
	cane_symbol_kind_t kind,
	const char* fmt,
	...
) {
	// TODO: Report line info from cane lexer. (use cane_log_info function).

	if (cane_peek_is_kind(log, lx, kind)) {
		return;
	}

	cane_symbol_t peeker;
	cane_lexer_peek(log, lx, &peeker);

	CANE_LOG_INFO(
		log,
		"expected = %s, kind = %s, ptr = %p, end = %p, size = %lu, str = "
		"'%.*s'",
		CANE_SYMBOL_KIND_TO_STR[kind],
		CANE_SYMBOL_KIND_TO_STR[peeker.kind],
		(void*)peeker.str.ptr,
		(void*)peeker.str.end,
		cane_ptrdiff(peeker.str.ptr, peeker.str.end),
		cane_ptrdiff(peeker.str.ptr, peeker.str.end),
		peeker.str.ptr
	);

	va_list args;
	va_start(args, fmt);

	cane_log(log, CANE_PRIORITY_FAIL, fmt, args);

	va_end(args);

	exit(EXIT_FAILURE);
}

static void cane_expect(
	cane_logger_t log,
	cane_lexer_t* lx,
	cane_parser_pred_t cond,
	const char* fmt,
	...
) {
	// TODO: Report line info from cane lexer. (use cane_log_info function).

	if (cane_peek_is(log, lx, cond)) {
		return;
	}

	va_list args;
	va_start(args, fmt);

	cane_log_lineinfo_v(log, CANE_PRIORITY_FAIL, NULL, NULL, NULL, fmt, args);

	va_end(args);

	exit(EXIT_FAILURE);
}

// Implementation of core parsing functions
static cane_symbol_t*
cane_parse(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_symbol_t* program = cane_parse_program(log, ctx, lx);

	return program;
}

// Core parsing functions
static cane_symbol_t*
cane_parse_program(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_symbol_t* program =
		NULL;  // TODO: Use allocator API to construct linked list.

	while (cane_peek_is_expression(log, lx)) {
		// TODO: Concatenate `expr` with `program` to build up a linked list of
		// instructions which will act as our intermediate representation in the
		// compiler.
		cane_symbol_t* expr = cane_parse_expression(log, ctx, lx);
	}

	cane_symbol_t symbol;
	cane_lexer_peek(log, lx, &symbol);

	cane_expect_kind(
		log,
		lx,
		CANE_ENDFILE,
		"expected EOF but found: '%s'",
		CANE_SYMBOL_TO_STR[symbol.kind]
	);

	return program;
}

static cane_symbol_t* cane_parse_expression(
	cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx
) {
	CANE_FUNCTION_ENTER(log);

	cane_expect(log, lx, cane_is_expression, "expected an expression");

	cane_symbol_t* expression =
		NULL;  // TODO: Use allocator API to construct linked list.

	// Integers/strings
	if (cane_peek_is_literal(log, lx)) {
		expression = cane_parse_literal(log, ctx, lx);
	}

	// Builtin operators/functions
	else if (cane_peek_is_builtin(log, lx)) {
		expression = cane_parse_builtin(log, ctx, lx);
	}

	// Symbol reference
	else if (cane_peek_is_kind(log, lx, CANE_IDENT)) {
		cane_symbol_t symbol;
		cane_lexer_take(log, lx, &symbol);

		// TODO: Handle symbol reference that isn't a builtin
		CANE_WHEREAMI(log);
	}

	// Functions
	else if (cane_peek_is_kind(log, lx, CANE_LBRACKET)) {
		expression = cane_parse_function(log, ctx, lx);
	}

	// Swizzle
	else if (cane_peek_is_kind(log, lx, CANE_LPAREN)) {
		expression = cane_parse_swizzle(log, ctx, lx);
	}

	// Type assertion
	else if (cane_peek_is_kind(log, lx, CANE_TYPE)) {
		// TODO: Should we even concatenate this expression? Maybe we should
		// just discard it.
		expression = cane_parse_assertion(log, ctx, lx);
	}

	else {
		// TODO: Unreachable
		cane_symbol_t symbol;
		cane_lexer_peek(log, lx, &symbol);

		cane_log(
			log,
			CANE_PRIORITY_FAIL,
			"unexpected token '%s'",
			CANE_SYMBOL_TO_STR[symbol.kind]
		);

		exit(EXIT_FAILURE);
	}

	return expression;
}

static cane_symbol_t*
cane_parse_function(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect_kind(log, lx, CANE_LBRACKET, "expected '['");
	cane_lexer_take(log, lx, NULL);

	cane_symbol_t* function = NULL;

	while (cane_peek_is_expression(log, lx)) {
		// TODO: Concatenate `expr` with `function` to build up a linked list of
		// instructions which will act as our intermediate representation in the
		// compiler.
		cane_symbol_t* expr = cane_parse_expression(log, ctx, lx);
	}

	cane_expect_kind(log, lx, CANE_RBRACKET, "expected ']'");
	cane_lexer_take(log, lx, NULL);

	return function;
}

static cane_symbol_t*
cane_parse_builtin(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect(log, lx, cane_is_builtin, "expected a built-in");

	// TODO: Allocate this node on the heap and return the pointer.
	cane_symbol_t symbol;
	cane_lexer_take(log, lx, &symbol);

	return NULL;
}

static cane_symbol_t*
cane_parse_literal(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect(log, lx, cane_is_literal, "expected a literal");

	// TODO: Allocate this node on the heap and return the pointer.
	cane_symbol_t symbol;
	cane_lexer_take(log, lx, &symbol);

	return NULL;
}

static cane_symbol_t*
cane_parse_type(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_symbol_t symbol;

	if (cane_peek_is_kind(log, lx, CANE_TYPE_NUMBER)) {
		// TODO: Check top-of-stack is number.
		cane_lexer_take(log, lx, &symbol);
	}

	else if (cane_peek_is_kind(log, lx, CANE_TYPE_STRING)) {
		// TODO: Check top-of-stack is string.
		cane_lexer_take(log, lx, &symbol);
	}

	else if (cane_peek_is_kind(log, lx, CANE_TYPE_ANY)) {
		// TODO: Check that there is an element on the type stack. It doesn't
		// matter what it is.
		cane_lexer_take(log, lx, &symbol);
	}

	else if (cane_peek_is_kind(log, lx, CANE_TYPE_FN)) {
		cane_parse_fntype(log, ctx, lx);
	}

	return NULL;
}

static cane_symbol_t*
cane_parse_fntype(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect_kind(log, lx, CANE_TYPE_FN, "expected 'fn'");
	cane_lexer_take(log, lx, NULL);

	cane_expect_kind(log, lx, CANE_LPAREN, "expected '('");
	cane_lexer_take(log, lx, NULL);

	// Parse input types
	while (cane_peek_is_type(log, lx)) {
		cane_parse_type(log, ctx, lx);
	}

	// Seperator
	cane_expect_kind(log, lx, CANE_ARROW, "expected '->'");
	cane_lexer_take(log, lx, NULL);

	// Parse output types
	while (cane_peek_is_type(log, lx)) {
		cane_parse_type(log, ctx, lx);
	}

	cane_expect_kind(log, lx, CANE_RPAREN, "expected ')'");
	cane_lexer_take(log, lx, NULL);

	return NULL;
}

static cane_symbol_t*
cane_parse_assertion(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect_kind(log, lx, CANE_TYPE, "expected '$'");
	cane_lexer_take(log, lx, NULL);

	cane_expect_kind(log, lx, CANE_LPAREN, "expected '('");
	cane_lexer_take(log, lx, NULL);

	while (cane_peek_is_type(log, lx)) {
		cane_parse_type(log, ctx, lx);
	}

	cane_expect_kind(log, lx, CANE_RPAREN, "expected ')'");
	cane_lexer_take(log, lx, NULL);

	return NULL;  // This function doesn't return anything. It just uses the
				  // type-stack from the context to do assertions on what types
				  // are present. We might need to do some NULL checking from
				  // the caller level in this case.
}

static cane_symbol_t*
cane_parse_swizzle(cane_logger_t log, cane_context_t* ctx, cane_lexer_t* lx) {
	CANE_FUNCTION_ENTER(log);

	cane_expect_kind(log, lx, CANE_LPAREN, "expected '('");
	cane_lexer_take(log, lx, NULL);

	cane_symbol_t* swizzle = NULL;

	// TODO: Collect a list of input identifiers and a list of output
	// identifiers then match up where the old slots map to the new slots and
	// try to synthesize a series of primitive ops that encode the same mapping.

	// Parse at least one identifier on the left side. This is because we can
	// use swizzle to "drop" items from the stack where the right side is empty.
	cane_expect_kind(log, lx, CANE_IDENT, "expected an identifier");

	do {
		// TODO: Figure out what swizzle should lower to. A series of primitive
		// dup, rot etc. or a single intrinsic.
		cane_symbol_t symbol;
		cane_lexer_take(log, lx, &symbol);
	} while (cane_peek_is_kind(log, lx, CANE_IDENT));

	// Seperator
	cane_expect_kind(log, lx, CANE_ARROW, "expected '->'");
	cane_lexer_take(log, lx, NULL);

	// Parse new stack slots.
	while (cane_peek_is_kind(log, lx, CANE_IDENT)) {
		cane_symbol_t symbol;
		cane_lexer_take(log, lx, &symbol);
	}

	cane_expect_kind(log, lx, CANE_RPAREN, "expected ')'");
	cane_lexer_take(log, lx, NULL);

	return swizzle;
}

#endif
