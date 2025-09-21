#ifndef CANE_LEX_H
#define CANE_LEX_H

#include <ctype.h>
#include <stdbool.h>
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
static bool cane_lexer_take_if(cane_lexer_t* lx, cane_lexer_pred_t cond) {
	char c = cane_lexer_char_peek(lx);

	if (!c || !cond(c)) {
		return false;
	}

	cane_lexer_char_take(lx);
	return true;
}

// Same as take_if but just takes a character directly
// for common usecases.
static bool cane_lexer_take_ifc(cane_lexer_t* lx, char c) {
	if (c != cane_lexer_char_peek(lx)) {
		return false;
	}

	cane_lexer_char_take(lx);
	return true;
}

static bool cane_lexer_take_str(cane_lexer_t* lx, const char* str) {
	size_t length = strlen(str);

	if (lx->ptr + length >= lx->end) {
		return false;
	}

	// TODO: Use custom strncmp to avoid iterating strings twice.
	if (strncmp(lx->ptr, str, length) != 0) {
		return false;
	}

	lx->ptr += length;
	return true;
}

static bool cane_lexer_take_while(cane_lexer_t* lx, cane_lexer_pred_t cond) {
	bool taken = false;

	while (cane_lexer_take_if(lx, cond)) {
		taken = true;
	}

	return taken;
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

	if (!cane_lexer_take_if(lx, cond)) {
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

	if (!cane_lexer_take_while(lx, cond)) {
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

	if (!cane_lexer_take_str(lx, str)) {
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

	if (!cane_lexer_take_if(lx, cane_is_alpha)) {
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

	if (!cane_lexer_take_str(lx, "#!")) {
		return false;
	}

	if (!cane_lexer_take_while(lx, cane_is_comment)) {
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

#endif
