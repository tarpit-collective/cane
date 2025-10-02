#ifndef CANE_LEX_H
#define CANE_LEX_H

#include <stdbool.h>
#include <string.h>

#include <cane/def.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/str.h>

////////////
// TOKENS //
////////////

// clang-format off

#define SYMBOLS \
	X(CANE_SYMBOL_NONE,       "none") \
\
	X(CANE_SYMBOL_ENDFILE,    "end of file") \
	X(CANE_SYMBOL_WHITESPACE, "whitespace") \
	X(CANE_SYMBOL_COMMENT,    "comment") \
\
	/* Misc. */ \
	X(CANE_SYMBOL_BACKSLASH, "backslash `\\`") \
	X(CANE_SYMBOL_COMMA,     "comma `,`") \
	X(CANE_SYMBOL_DOT,       "dot `.`") \
	X(CANE_SYMBOL_EXCLAIM,   "exclaim `!`") \
	X(CANE_SYMBOL_ARROW,     "arrow `=>`") \
	X(CANE_SYMBOL_COLON,     "colon `:`") \
	X(CANE_SYMBOL_SEMICOLON, "semicolon `;`") \
	X(CANE_SYMBOL_QUOTE,     "quote `'`") \
	X(CANE_SYMBOL_AT,        "at `@`") \
	X(CANE_SYMBOL_STARS,     "stars `**`") \
	X(CANE_SYMBOL_TILDA,     "tilda `~`") \
\
	/* Cons Lists */ \
	X(CANE_SYMBOL_STATEMENT, "statement") \
	X(CANE_SYMBOL_CHOICE, "choice") \
	X(CANE_SYMBOL_LAYER, "layer") \
\
	/* AST */ \
	X(CANE_SYMBOL_CONCATENATE, "concatenate") \
	X(CANE_SYMBOL_CALL, "call") \
	X(CANE_SYMBOL_ASSIGN, "assign") \
	X(CANE_SYMBOL_FUNCTION, "function") \
	X(CANE_SYMBOL_REPEAT, "repeat") \
	X(CANE_SYMBOL_MAP, "map") \
	X(CANE_SYMBOL_INVERT, "invert") \
	X(CANE_SYMBOL_REVERSE, "reverse") \
	X(CANE_SYMBOL_RHYTHM, "rhythm") \
\
	X(CANE_SYMBOL_ABS, "abs") \
	X(CANE_SYMBOL_NEG, "neg") \
\
	X(CANE_SYMBOL_BEAT, "beat") \
	X(CANE_SYMBOL_REST, "rest") \
\
	X(CANE_SYMBOL_LSHIFT, "lshift") \
	X(CANE_SYMBOL_RSHIFT, "rshift") \
\
	/* Atoms */ \
	X(CANE_SYMBOL_NUMBER,     "number") \
	X(CANE_SYMBOL_STRING,     "string") \
	X(CANE_SYMBOL_IDENTIFIER, "identifier") \
\
	/* Keywords */ \
	X(CANE_SYMBOL_LCM, "lcm") \
	X(CANE_SYMBOL_GCD, "gcd") \
\
	/* Operators */ \
	X(CANE_SYMBOL_OR,  "or") \
	X(CANE_SYMBOL_XOR, "xor") \
	X(CANE_SYMBOL_AND, "and") \
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

// clang-format on

#define X(x, y) x,

typedef enum {
	SYMBOLS CANE_SYMBOL_TOTAL
} cane_symbol_kind_t;

#undef X

// Maps the enum const direct to a string
#define X(x, y) [x] = #x,
const char* CANE_SYMBOL_TO_STR[] = {SYMBOLS};
#undef X

// Map the enum const to a human readable string
#define X(x, y) [x] = y,
const char* CANE_SYMBOL_TO_STR_HUMAN[] = {SYMBOLS};
#undef X

#undef SYMBOLS

/////////////
// Symbols //
/////////////

typedef struct cane_symbol cane_symbol_t;

struct cane_symbol {
	cane_symbol_kind_t kind;
	cane_string_view_t sv;
};

// Parser and lexer predicates & other function pointers
typedef bool (*cane_char_pred_t)(char);
typedef bool (*cane_symbol_pred_t)(cane_symbol_kind_t);

typedef cane_symbol_kind_t (*cane_lexer_fixup_t)(cane_symbol_kind_t);

///////////
// LEXER //
///////////

typedef struct cane_lexer cane_lexer_t;

struct cane_lexer {
	cane_string_view_t source;

	const char* ptr;
	const char* end;

	cane_symbol_t peek;
};

static bool cane_lexer_take(
	cane_lexer_t* lx, cane_symbol_t* symbol, cane_lexer_fixup_t fixup
);

static cane_lexer_t cane_lexer_create(cane_string_view_t sv) {
	// Initialise peek to NONE
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.sv = {sv.begin, sv.end},
	};

	cane_lexer_t lx = (cane_lexer_t){
		.source = sv,  // Original source for error reporting

		.ptr = sv.begin,  // Initialise lexer pointers
		.end = sv.end,

		.peek = symbol,
	};

	// Prime the lexer by storing a peek token
	cane_lexer_take(&lx, NULL, NULL);

	return lx;
}

// Basic stream interaction
static bool cane_str_peek(cane_lexer_t* lx, char* c) {
	// Return false for EOF.
	if (lx->ptr > lx->end) {
		return false;
	}

	if (c != NULL) {
		*c = *lx->ptr;
	}

	return true;
}

static bool cane_str_take(cane_lexer_t* lx, char* c) {
	if (!cane_str_peek(lx, c)) {
		return false;
	}

	lx->ptr++;
	return true;
}

// Conditional consumers
static bool cane_str_take_if(cane_lexer_t* lx, cane_char_pred_t pred, char* c) {
	char peek;

	if (!cane_str_peek(lx, &peek) || !pred(peek)) {
		return false;
	}

	cane_str_take(lx, c);
	return true;
}

// Same as take_if but just takes a character directly
// for common usecases.
static bool cane_str_take_if_char(cane_lexer_t* lx, char c) {
	char peek;

	if (!cane_str_peek(lx, &peek)) {
		return false;
	}

	if (c != peek) {
		return false;
	}

	cane_str_take(lx, NULL);
	return true;
}

// Consumes a given string from the lexer stream or nothing at all.
static bool cane_str_take_str(cane_lexer_t* lx, cane_string_view_t sv) {
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
static bool cane_str_take_while(cane_lexer_t* lx, cane_char_pred_t pred) {
	bool taken = false;

	while (cane_str_take_if(lx, pred, NULL)) {
		taken = true;
	}

	return taken;
}

// Token producers
// These functions wrap the basic "str_take" functions so that we can
// wrap them into a symbol type/token.
static bool cane_lexer_produce_if(
	cane_lexer_t* lx,
	cane_symbol_t* out,
	cane_symbol_kind_t kind,
	cane_char_pred_t pred
) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = kind,
		.sv = {lx->ptr, lx->ptr},
	};

	if (!cane_str_take_if(lx, pred, NULL)) {
		return false;
	}

	symbol.sv.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

static bool cane_lexer_produce_while(
	cane_lexer_t* lx,
	cane_symbol_t* out,
	cane_symbol_kind_t kind,
	cane_char_pred_t pred
) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = kind,
		.sv = {lx->ptr, lx->ptr},
	};

	if (!cane_str_take_while(lx, pred)) {
		return false;
	}

	symbol.sv.end = lx->ptr;

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
		.sv = {lx->ptr, lx->ptr},
	};

	if (!cane_str_take_str(lx, sv)) {
		return false;
	}

	symbol.sv.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

// Cane specific lexer functions
static bool
cane_lexer_produce_identifier(cane_lexer_t* lx, cane_symbol_t* out) {
	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.sv = {lx->ptr, lx->ptr},
	};

	if (!cane_str_take_if(lx, cane_is_identifier_start, NULL)) {
		return false;
	}

	cane_str_take_while(lx, cane_is_identifier);

	symbol.sv.end = lx->ptr;

	// We could have used `cane_produce_str` here to handle these cases
	// but we want "maximal munch" meaning that we lex the entire
	// identifier before trying to classify it. Why? because if we
	// didn't, an identifier like "letfoo" would actually be lexed
	// as 2 seperate tokens because it sees `let` and stops there.

	// Keywords
	if (cane_string_view_eq(symbol.sv, CANE_SV("lcm"))) {
		symbol.kind = CANE_SYMBOL_LCM;
	}

	else if (cane_string_view_eq(symbol.sv, CANE_SV("gcd"))) {
		symbol.kind = CANE_SYMBOL_GCD;
	}

	// Operators
	else if (cane_string_view_eq(symbol.sv, CANE_SV("or"))) {
		symbol.kind = CANE_SYMBOL_OR;
	}

	else if (cane_string_view_eq(symbol.sv, CANE_SV("xor"))) {
		symbol.kind = CANE_SYMBOL_XOR;
	}

	else if (cane_string_view_eq(symbol.sv, CANE_SV("and"))) {
		symbol.kind = CANE_SYMBOL_AND;
	}

	// User identifier
	else {
		symbol.kind = CANE_SYMBOL_IDENTIFIER;
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
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_BACKSLASH, CANE_SV("\\")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_COMMA,     CANE_SV(","))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_DOT,       CANE_SV("."))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_EXCLAIM,   CANE_SV("!"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ARROW,     CANE_SV("=>")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_COLON,     CANE_SV(":"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_SEMICOLON, CANE_SV(";"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_QUOTE,     CANE_SV("'"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_AT,        CANE_SV("@"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_STARS,     CANE_SV("**")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_TILDA,     CANE_SV("~"))  ||

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
		.sv = {lx->ptr, lx->ptr},
	};

	if (!cane_str_take_str(lx, CANE_SV("#!")) ||
		!cane_str_take_while(lx, cane_is_not_newline)) {
		return false;
	}

	symbol.sv.end = lx->ptr;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

// Core lexer interface
static bool cane_lexer_peek(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	if (out != NULL) {
		*out = lx->peek;
	}

	if (fixup != NULL) {
		out->kind = fixup(out->kind);
	}

	if (!cane_str_peek(lx, NULL)) {  // Check for EOF
		return false;
	}

	return true;
}

static bool cane_lexer_peek_is(
	cane_lexer_t* lx,
	cane_symbol_pred_t cond,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	// cane_symbol_t symbol = (cane_symbol_t){
	// 	.kind = CANE_SYMBOL_NONE,
	// 	.sv = {lx->ptr, lx->ptr},
	// };

	cane_symbol_t symbol;

	if (!cane_lexer_peek(lx, &symbol, fixup)) {
		return false;
	}

	if (out != NULL) {  // DO NOT REMOVE! This shouldn't be needed since we do
						// it already inside cane_lexer_peek but it segfaults
						// without it, so...
		*out = symbol;
	}

	if (!cond(symbol.kind)) {
		return false;
	}

	return true;
}

static bool cane_lexer_peek_is_kind(
	cane_lexer_t* lx,
	cane_symbol_kind_t kind,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t symbol;
	// cane_symbol_t symbol = (cane_symbol_t){
	// 	.kind = CANE_SYMBOL_NONE,
	// 	.sv = {lx->ptr, lx->ptr},
	// };

	if (!cane_lexer_peek(lx, &symbol, fixup)) {
		return false;
	}

	if (out != NULL) {  // DO NOT REMOVE!
		*out = symbol;
	}

	if (symbol.kind != kind) {
		return false;
	}

	return true;
}

static bool cane_lexer_take_any(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	cane_logger_t log = cane_logger_create_default();

	cane_symbol_t symbol = (cane_symbol_t){
		.kind = CANE_SYMBOL_NONE,
		.sv = {lx->ptr, lx->ptr},
	};

	// Handle EOF
	if (lx->ptr >= lx->end) {
		symbol.kind = CANE_SYMBOL_ENDFILE;
	}

	// Handle normal tokens
	else if (!(cane_lexer_produce_identifier(lx, &symbol) ||
			   cane_lexer_produce_number(lx, &symbol) ||
			   cane_lexer_produce_sigil(lx, &symbol))) {
		CANE_DIE(log, "unknown character `%c`!", lx->ptr[0]);
		return false;
	}

	// Return previously peeked token and then store newly
	// lexed token to be used on the next call to peek.
	if (out != NULL) {
		*out = lx->peek;
	}

	if (fixup != NULL) {
		out->kind = fixup(out->kind);
	}

	lx->peek = symbol;

	return true;
}

// Filter out whitespace and comments.
static bool cane_lexer_take(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	while (cane_lexer_produce_whitespace(lx, NULL) ||
		   cane_lexer_produce_comment(lx, NULL))
		;

	return cane_lexer_take_any(lx, out, fixup);
}

static bool cane_lexer_take_if(
	cane_lexer_t* lx,
	cane_symbol_pred_t pred,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t symbol;
	// cane_symbol_t symbol = (cane_symbol_t){
	// 	.kind = CANE_SYMBOL_NONE,
	// 	.sv = {lx->ptr, lx->ptr},
	// };

	cane_lexer_peek(lx, &symbol, fixup);

	if (!pred(symbol.kind)) {
		return false;
	}

	return cane_lexer_take(lx, out, fixup);
}

static bool cane_lexer_take_if_kind(
	cane_lexer_t* lx,
	cane_symbol_kind_t kind,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t symbol;
	// cane_symbol_t symbol = (cane_symbol_t){
	// 	.kind = CANE_SYMBOL_NONE,
	// 	.sv = {lx->ptr, lx->ptr},
	// };

	cane_lexer_peek(lx, &symbol, fixup);

	if (symbol.kind != kind) {
		return false;
	}

	return cane_lexer_take(lx, out, fixup);
}

static bool cane_lexer_discard(cane_lexer_t* lx) {
	return cane_lexer_take(lx, NULL, NULL);
}

static bool cane_lexer_discard_if(cane_lexer_t* lx, cane_symbol_pred_t pred) {
	return cane_lexer_take_if(lx, pred, NULL, NULL);
}

static bool
cane_lexer_discard_if_kind(cane_lexer_t* lx, cane_symbol_kind_t kind) {
	return cane_lexer_take_if_kind(lx, kind, NULL, NULL);
}

#endif
