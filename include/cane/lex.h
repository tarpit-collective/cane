#ifndef CANE_LEX_H
#define CANE_LEX_H

#include <ctype.h>
#include <stdbool.h>
#include <string.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/str.h>

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

/////////////
// Symbols //
/////////////

typedef struct cane_symbol cane_symbol_t;

struct cane_symbol {
	cane_symbol_kind_t kind;
	cane_string_view_t str;
};

///////////
// LEXER //
///////////

typedef bool (*cane_lexer_pred_t)(char);  // Used for lexer predicates

typedef struct cane_lexer cane_lexer_t;

struct cane_lexer {
	cane_string_view_t source;

	const char* ptr;
	const char* end;

	cane_symbol_t peek;
};

static bool cane_lexer_take(cane_lexer_t* lx, cane_symbol_t* symbol);

static cane_lexer_t cane_lexer_create(cane_string_view_t sv) {
	cane_lexer_t lx = (cane_lexer_t){
		.source = sv,  // Original source for error reporting

		.ptr = sv.begin,  // Initialise lexer pointers
		.end = sv.end,

		.peek =  // Initialise peek to NONE
		{
			.kind = CANE_SYMBOL_NONE,
			.str =
				{
					.begin = sv.begin,
					.end = sv.end,
				},
		},
	};

	cane_lexer_take(&lx, NULL);

	return lx;
}

// Basic stream interaction
static char cane_lexer_char_peek(cane_lexer_t* lx) {
	if (lx->ptr > lx->end) {
		return '\0';
	}

	return *lx->ptr;
}

static char cane_lexer_char_take(cane_lexer_t* lx) {
	if (lx->ptr > lx->end) {
		return '\0';
	}

	return *lx->ptr++;
}

// Conditional consumers
static bool cane_lexer_take_if(cane_lexer_t* lx, cane_lexer_pred_t pred) {
	char c = cane_lexer_char_peek(lx);

	if (!c || !pred(c)) {
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

// Consumes a given string from the lexer stream or nothing at all.
static bool cane_lexer_take_str(cane_lexer_t* lx, cane_string_view_t sv) {
	size_t length = cane_string_view_length(sv);

	if (lx->ptr + length > lx->end) {
		return false;
	}

	cane_string_view_t current_symbol = (cane_string_view_t){
		.begin = lx->ptr,
		.end = lx->ptr + length,
	};

	if (!cane_string_view_eq(sv, current_symbol)) {
		return false;
	}

	lx->ptr += length;
	return true;
}

// Continue to consume characters while the predicate holds.
static bool cane_lexer_take_while(cane_lexer_t* lx, cane_lexer_pred_t pred) {
	bool taken = false;

	while (cane_lexer_take_if(lx, pred)) {
		taken = true;
	}

	return taken;
}

// Token producers
// These functions wrap the basic "lexer_take" functions so that we can
// wrap them into a symbol type/token.
static bool cane_lexer_produce_if(
	cane_lexer_t* lx,
	cane_symbol_t* out,
	cane_symbol_kind_t kind,
	cane_lexer_pred_t pred
) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = kind,
		.str = {lx->ptr, lx->ptr},
	};

	if (!cane_lexer_take_if(lx, pred)) {
		return false;
	}

	symbol.str.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

static bool cane_lexer_produce_while(
	cane_lexer_t* lx,
	cane_symbol_t* out,
	cane_symbol_kind_t kind,
	cane_lexer_pred_t pred
) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = kind,
		.str = {lx->ptr, lx->ptr},
	};

	if (!cane_lexer_take_while(lx, pred)) {
		return false;
	}

	symbol.str.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

static bool cane_lexer_produce_str(
	cane_lexer_t* lx,
	cane_symbol_t* out,
	cane_symbol_kind_t kind,
	cane_string_view_t sv
) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = kind,
		.str = {lx->ptr, lx->ptr},
	};

	if (!cane_lexer_take_str(lx, sv)) {
		return false;
	}

	symbol.str.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

// Cane specific lexer functions
static bool cane_lexer_produce_ident(cane_lexer_t* lx, cane_symbol_t* out) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.str = {lx->ptr, lx->ptr},
	};

	if (!cane_lexer_take_if(lx, cane_is_alpha)) {
		return false;
	}

	cane_lexer_take_while(lx, cane_is_ident);

	symbol.str.end = lx->ptr;

	// We could have used `cane_produce_str` here to handle these cases
	// but we want "maximal munch" meaning that we lex the entire
	// identifier before trying to classify it. Why? because if we
	// didn't, an identifier like "letfoo" would actually be lexed
	// as 2 seperate tokens because it sees `let` and stops there.

	// Keywords
	if (cane_string_view_eq(symbol.str, CANE_SV("lcm"))) {
		symbol.kind = CANE_SYMBOL_LCM;
	}

	else if (cane_string_view_eq(symbol.str, CANE_SV("gcd"))) {
		symbol.kind = CANE_SYMBOL_GCD;
	}

	// Operators
	else if (cane_string_view_eq(symbol.str, CANE_SV("or"))) {
		symbol.kind = CANE_SYMBOL_OR;
	}

	else if (cane_string_view_eq(symbol.str, CANE_SV("xor"))) {
		symbol.kind = CANE_SYMBOL_XOR;
	}

	else if (cane_string_view_eq(symbol.str, CANE_SV("and"))) {
		symbol.kind = CANE_SYMBOL_AND;
	}

	else if (cane_string_view_eq(symbol.str, CANE_SV("not"))) {
		symbol.kind = CANE_SYMBOL_NOT;
	}

	// User identifier
	else {
		symbol.kind = CANE_SYMBOL_IDENT;
	}

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

static bool cane_lexer_produce_number(cane_lexer_t* lx, cane_symbol_t* out) {
	return cane_lexer_produce_while(lx, out, CANE_SYMBOL_NUMBER, cane_is_digit);
}

static bool cane_lexer_produce_sigil(cane_lexer_t* lx, cane_symbol_t* out) {
	// clang-format off
	#define CANE_PRODUCE_SIGIL(kind, sv) \
		cane_lexer_produce_str(lx, out, kind, sv)

	return
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_BEAT, CANE_SV("!")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REST, CANE_SV(".")) ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ASSIGN,  CANE_SV("=>")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RHYTHM,  CANE_SV(":"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REVERSE, CANE_SV("'"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_MAP,     CANE_SV("@"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_REPEAT,  CANE_SV("**")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_INVERT,  CANE_SV("~"))  ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ADD, CANE_SV("+")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_SUB, CANE_SV("-")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_MUL, CANE_SV("*")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_DIV, CANE_SV("/")) ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LPAREN, CANE_SV("(")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RPAREN, CANE_SV(")")) ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LBRACE, CANE_SV("{")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RBRACE, CANE_SV("}")) ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LBRACKET, CANE_SV("[")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RBRACKET, CANE_SV("]")) ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_LCHEVRON, CANE_SV("<")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_RCHEVRON, CANE_SV(">"))
	;

	#undef CANE_PRODUCE_SIGIL
	// clang-format on
}

static bool
cane_lexer_produce_whitespace(cane_lexer_t* lx, cane_symbol_t* out) {
	return cane_lexer_produce_while(
		lx, out, CANE_SYMBOL_WHITESPACE, cane_is_whitespace
	);
}

static bool cane_lexer_produce_comment(cane_lexer_t* lx, cane_symbol_t* out) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_COMMENT,
		.str = {lx->ptr, lx->ptr},
	};

	if (!cane_lexer_take_str(lx, CANE_SV("#!")) ||
		!cane_lexer_take_while(lx, cane_is_comment)) {
		return false;
	}

	symbol.str.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

// Core lexer interface
// TODO: Reconsider implementation. Is it safe to always return true?
static bool cane_lexer_peek(cane_lexer_t* lx, cane_symbol_t* out) {
	if (out != NULL) {
		*out = lx->peek;
	}

	return true;
}

static bool cane_lexer_take(cane_lexer_t* lx, cane_symbol_t* out) {
	cane_logger_t log = cane_logger_create_default();

	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.str = {lx->ptr, lx->ptr},
	};

	while (cane_lexer_produce_whitespace(lx, NULL) ||
		   cane_lexer_produce_comment(lx, NULL)) {
		// Just consume whitespace and comments and do nothing else.
	}

	// Handle EOF
	if (lx->ptr >= lx->end) {
		symbol.kind = CANE_SYMBOL_ENDFILE;
	}

	// Handle normal tokens
	else if (!(cane_lexer_produce_ident(lx, &symbol) ||
			   cane_lexer_produce_number(lx, &symbol) ||
			   cane_lexer_produce_sigil(lx, &symbol))) {
		cane_die(log, NULL, NULL, NULL, "unknown character `%c`!", lx->ptr[0]);
		return false;
	}

	// Return previously peeked token and then store newly
	// lexed token to be used on the next call to peek.
	if (out != NULL) {
		*out = lx->peek;
	}

	lx->peek = symbol;
	return true;
}

#endif
