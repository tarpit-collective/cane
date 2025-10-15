#ifndef CANE_LEX_H
#define CANE_LEX_H

#include <stdbool.h>
#include <string.h>

#include <cane/def.h>
#include <cane/enum.h>
#include <cane/log.h>
#include <cane/util.h>
#include <cane/str.h>

// Allow us to provide line info and line preview.
typedef struct cane_location cane_location_t;

struct cane_location {
	cane_string_view_t source;  // Original source file
	cane_string_view_t symbol;  // Specific token
};

typedef struct cane_lexer cane_lexer_t;

static cane_location_t cane_location_create(cane_lexer_t* lx);

static void cane_report_and_die(
	cane_location_t loc, cane_report_kind_t kind, const char* fmt, ...
);

/////////////
// Symbols //
/////////////

typedef struct cane_symbol cane_symbol_t;

struct cane_symbol {
	cane_symbol_kind_t kind;
	cane_location_t location;
};

static cane_symbol_t
cane_symbol_create(cane_symbol_kind_t kind, cane_location_t loc) {
	return (cane_symbol_t){
		.kind = kind,
		.location = loc,
	};
}

static cane_symbol_t cane_symbol_create_default() {
	cane_string_view_t sv = CANE_SV("(empty)");

	return cane_symbol_create(
		CANE_SYMBOL_NONE,
		(cane_location_t){
			.symbol = sv,
			.source = sv,
		}
	);
}

// Parser and lexer predicates & other function pointers
typedef bool (*cane_char_pred_t)(char);
typedef bool (*cane_symbol_pred_t)(cane_symbol_kind_t);

typedef cane_symbol_kind_t (*cane_lexer_fixup_t)(cane_symbol_kind_t);

///////////
// LEXER //
///////////

struct cane_lexer {
	cane_location_t location;
	cane_symbol_t peek;
};

static bool cane_lexer_take(
	cane_lexer_t* lx, cane_symbol_t* symbol, cane_lexer_fixup_t fixup
);

static cane_lexer_t cane_lexer_create(cane_string_view_t sv) {
	// Initialise peek to NONE
	cane_symbol_t symbol = cane_symbol_create_default();

	cane_lexer_t lx = (cane_lexer_t){
		.location.source = sv,  // Original source for error reporting
		.location.symbol = sv,  // Initialise lexer pointers
		.peek = symbol,
	};

	// Prime the lexer by storing a peek token
	cane_lexer_take(&lx, NULL, NULL);

	return lx;
}

// Basic stream interaction
static bool cane_str_peek(cane_lexer_t* lx, char* c) {
	// Return false for EOF.
	if (lx->location.symbol.begin > lx->location.symbol.end) {
		return false;
	}

	if (c != NULL) {
		*c = *lx->location.symbol.begin;
	}

	return true;
}

static bool cane_str_take(cane_lexer_t* lx, char* c) {
	if (!cane_str_peek(lx, c)) {
		return false;
	}

	lx->location.symbol.begin++;
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

	if (lx->location.symbol.begin + length > lx->location.symbol.end) {
		return false;
	}

	cane_string_view_t current_symbol = (cane_string_view_t){
		.begin = lx->location.symbol.begin,
		.end = lx->location.symbol.begin + length,
	};

	if (!cane_string_view_eq(sv, current_symbol)) {
		return false;
	}

	lx->location.symbol.begin += length;
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
		.location = lx->location,
	};

	if (!cane_str_take_if(lx, pred, NULL)) {
		return false;
	}

	symbol.location.symbol.end = lx->location.symbol.begin;

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
		.location = lx->location,
	};

	if (!cane_str_take_while(lx, pred)) {
		return false;
	}

	symbol.location.symbol.end = lx->location.symbol.begin;

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
		.location = lx->location,
	};

	if (!cane_str_take_str(lx, sv)) {
		return false;
	}

	symbol.location.symbol.end = lx->location.symbol.begin;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

// Cane specific lexer functions
static bool
cane_lexer_produce_identifier(cane_lexer_t* lx, cane_symbol_t* out) {
	cane_symbol_t symbol =
		cane_symbol_create(CANE_SYMBOL_IDENTIFIER, lx->location);

	if (!cane_str_take_if(lx, cane_is_identifier_start, NULL)) {
		return false;
	}

	cane_str_take_while(lx, cane_is_identifier);

	symbol.location.symbol.end = lx->location.symbol.begin;

	// We could have used `cane_produce_str` here to handle these cases
	// but we want "maximal munch" meaning that we lex the entire
	// identifier before trying to classify it. Why? because if we
	// didn't, an identifier like "letfoo" would actually be lexed
	// as 2 seperate tokens because it sees `let` and stops there.

	// Keywords
	if (cane_string_view_eq(symbol.location.symbol, CANE_SV("lcm"))) {
		symbol.kind = CANE_SYMBOL_LCM;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("gcd"))) {
		symbol.kind = CANE_SYMBOL_GCD;
	}

	// Operators
	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("or"))) {
		symbol.kind = CANE_SYMBOL_OR;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("xor"))) {
		symbol.kind = CANE_SYMBOL_XOR;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("and"))) {
		symbol.kind = CANE_SYMBOL_AND;
	}

	// Type annotations
	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("number"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_NUMBER;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("string"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_STRING;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("rhythm"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_RHYTHM;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("melody"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_MELODY;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("sequence"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_SEQUENCE;
	}

	else if (cane_string_view_eq(symbol.location.symbol, CANE_SV("pattern"))) {
		symbol.kind = CANE_SYMBOL_ANNOTATION_PATTERN;
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
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_COERCE,      CANE_SV("&")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_BACKSLASH,   CANE_SV("\\")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_COMMA,       CANE_SV(","))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_BACKTICK,    CANE_SV("`"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_QUOTE,       CANE_SV("'"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_AT,          CANE_SV("@"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_STARS,       CANE_SV("**")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_TILDA,       CANE_SV("~"))  ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_DOT,         CANE_SV("."))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_EXCLAIM,     CANE_SV("!"))  ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_COLON,       CANE_SV(":"))  ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_SEMICOLON,   CANE_SV(";"))  ||

		CANE_PRODUCE_SIGIL(CANE_SYMBOL_FATARROW,    CANE_SV("=>")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_WIGGLEARROW, CANE_SV("~>")) ||
		CANE_PRODUCE_SIGIL(CANE_SYMBOL_ARROW,       CANE_SV("->")) ||

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
	cane_symbol_t symbol =
		cane_symbol_create(CANE_SYMBOL_COMMENT, lx->location);

	if (!cane_str_take_str(lx, CANE_SV("#!")) ||
		!cane_str_take_while(lx, cane_is_not_newline)) {
		return false;
	}

	symbol.location.symbol.end = lx->location.symbol.begin;

	if (out != NULL) {
		*out = symbol;
	}

	return true;
}

////////////////
// Lexer Core //
////////////////

static bool cane_lexer_peek(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	cane_symbol_t peek = lx->peek;

	if (out != NULL) {
		if (fixup != NULL) {
			peek.kind = fixup(peek.kind);
		}

		*out = peek;
	}

	return cane_str_peek(lx, NULL);
}

static bool cane_lexer_peek_is(
	cane_lexer_t* lx,
	cane_symbol_pred_t cond,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t peek;

	if (!cane_lexer_peek(lx, &peek, fixup)) {
		return false;
	}

	if (out != NULL) {
		*out = peek;
	}

	if (!cond(peek.kind)) {
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
	cane_symbol_t peek;
	if (!cane_lexer_peek(lx, &peek, fixup)) {
		return false;
	}

	if (out != NULL) {
		*out = peek;
	}

	if (peek.kind != kind) {
		return false;
	}

	return true;
}

static bool cane_lexer_take_any(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	cane_symbol_t symbol = cane_symbol_create_default();

	// Handle EOF
	if (lx->location.symbol.begin >=
		lx->location.symbol
			.end) {  // Must be >= which means we can't use `cane_str_peek`
		symbol.kind = CANE_SYMBOL_ENDFILE;
	}

	// Handle normal tokens
	else if (!(cane_lexer_produce_identifier(lx, &symbol) ||
			   cane_lexer_produce_number(lx, &symbol) ||
			   cane_lexer_produce_sigil(lx, &symbol))) {
		cane_report_and_die(

			cane_location_create(lx),
			CANE_REPORT_LEXICAL,
			"unknown character `%c`!",
			*lx->location.symbol.begin
		);

		return false;
	}

	// Return previously peeked token and then store newly
	// lexed token to be used on the next call to peek.
	if (out != NULL) {
		*out = lx->peek;

		if (fixup != NULL) {
			out->kind = fixup(out->kind);
		}
	}

	lx->peek = symbol;

	// CANE_LOG_OKAY(
	// 	cane_logger_create_default(),
	// 	"kind = %s",
	// 	CANE_SYMBOL_TO_STR[lx->peek.kind]
	// );

	return true;
}

// Filter out whitespace and comments.
static bool cane_lexer_take(
	cane_lexer_t* lx, cane_symbol_t* out, cane_lexer_fixup_t fixup
) {
	while (cane_lexer_produce_whitespace(lx, out) ||
		   cane_lexer_produce_comment(lx, out))
		;

	return cane_lexer_take_any(lx, out, fixup);
}

static bool cane_lexer_take_if(
	cane_lexer_t* lx,
	cane_symbol_pred_t pred,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t peek;
	cane_lexer_peek(lx, &peek, fixup);

	if (!pred(peek.kind)) {
		return false;
	}

	if (out != NULL) {
		*out = peek;
	}

	return cane_lexer_take(lx, out, fixup);
}

static bool cane_lexer_take_if_kind(
	cane_lexer_t* lx,
	cane_symbol_kind_t kind,
	cane_symbol_t* out,
	cane_lexer_fixup_t fixup
) {
	cane_symbol_t peek;
	cane_lexer_peek(lx, &peek, fixup);

	if (peek.kind != kind) {
		return false;
	}

	if (out != NULL) {
		*out = peek;
	}

	return cane_lexer_take(lx, out, fixup);
}

static bool cane_lexer_discard(cane_lexer_t* lx) {
	cane_symbol_t symbol;
	return cane_lexer_take(lx, &symbol, NULL);
}

static bool cane_lexer_discard_if(cane_lexer_t* lx, cane_symbol_pred_t pred) {
	cane_symbol_t symbol;
	return cane_lexer_take_if(lx, pred, &symbol, NULL);
}

static bool
cane_lexer_discard_if_kind(cane_lexer_t* lx, cane_symbol_kind_t kind) {
	cane_symbol_t symbol;
	return cane_lexer_take_if_kind(lx, kind, &symbol, NULL);
}

///////////////
// Reporting //
///////////////

static cane_location_t cane_location_create(cane_lexer_t* lx) {
	return (cane_location_t){
		.source = lx->location.source,
		.symbol = lx->peek.location.symbol,
	};
}

typedef struct cane_lineinfo cane_lineinfo_t;

struct cane_lineinfo {
	size_t line;
	size_t column;
};

// Calculate line and column.
static cane_lineinfo_t cane_location_coordinates(cane_location_t location) {
	cane_lineinfo_t info = (cane_lineinfo_t){
		.line = 1,
		.column = 1,
	};

	cane_string_view_t source = location.source;
	cane_string_view_t symbol = location.symbol;

	if (!(symbol.begin >= source.begin && symbol.end <= source.end)) {
		CANE_DIE("symbol not in range of source");
	}

	for (const char* ptr = source.begin; ptr != symbol.end; ptr++) {
		if (*ptr == '\n') {
			info.line++;
			info.column = 0;
		}

		info.column++;
	}

	return info;
}

// TODO: Preview of token/line of code where error occured
static void cane_report_and_die(
	cane_location_t loc, cane_report_kind_t kind, const char* fmt, ...
) {
	cane_lineinfo_t info = cane_location_coordinates(loc);

	va_list args;
	va_start(args, fmt);

	fprintf(
		stderr,
		"%s%s %s error" CANE_RESET " @ %zu:%zu => ",
		CANE_LOGLEVEL_COLOUR[CANE_PRIORITY_FAIL],
		CANE_LOGLEVEL_TO_STR[CANE_PRIORITY_FAIL],
		CANE_REPORT_KIND_TO_STR_HUMAN[kind],
		info.line,
		info.column
	);

	vfprintf(stderr, fmt, args);
	fputc('\n', stderr);

	va_end(args);
	exit(EXIT_FAILURE);
}

#endif
