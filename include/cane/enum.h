#ifndef CANE_ENUM_H
#define CANE_ENUM_H

/////////////
// Reports //
/////////////

#define CANE_REPORT_KINDS \
	X(CANE_REPORT_GENERIC, "generic") \
	X(CANE_REPORT_LEXICAL, "lexical") \
	X(CANE_REPORT_SYNTAX, "syntax") \
	X(CANE_REPORT_SEMANTIC, "semantic") \
	X(CANE_REPORT_TYPE, "type")

#define X(x, y) x,

typedef enum {
	CANE_REPORT_KINDS CANE_REPORT_TOTAL
} cane_report_kind_t;

#undef X

// Maps the enum const direct to a string
#define X(x, y) [x] = #x,
const char* CANE_REPORT_KIND_TO_STR[] = {CANE_REPORT_KINDS};
#undef X

// Map the enum const to a human readable string
#define X(x, y) [x] = y,
const char* CANE_REPORT_KIND_TO_STR_HUMAN[] = {CANE_REPORT_KINDS};
#undef X

/////////////
// Symbols //
/////////////

#define CANE_SYMBOL_KINDS \
	X(CANE_SYMBOL_NONE, "none") \
\
	/* Misc. */ \
	X(CANE_SYMBOL_ENDFILE, "end of file") \
	X(CANE_SYMBOL_WHITESPACE, "whitespace") \
	X(CANE_SYMBOL_COMMENT, "comment") \
\
	/* Raw Symbols (later converted to operator symbols) */ \
	X(CANE_SYMBOL_AMPERSAND, "ampersand `&`") \
	X(CANE_SYMBOL_BACKSLASH, "backslash `\\`") \
	X(CANE_SYMBOL_BACKTICK, "backtick ```") \
	X(CANE_SYMBOL_COMMA, "comma `,`") \
	X(CANE_SYMBOL_DOT, "dot `.`") \
	X(CANE_SYMBOL_EXCLAIM, "exclaim `!`") \
	X(CANE_SYMBOL_FATARROW, "fat arrow `=>`") \
	X(CANE_SYMBOL_ARROW, "arrow `->`") \
	X(CANE_SYMBOL_COLON, "colon `:`") \
	X(CANE_SYMBOL_SEMICOLON, "semicolon `;`") \
	X(CANE_SYMBOL_QUOTE, "quote `'`") \
	X(CANE_SYMBOL_AT, "at `@`") \
	X(CANE_SYMBOL_STARS, "stars `**`") \
	X(CANE_SYMBOL_TILDA, "tilda `~`") \
	X(CANE_SYMBOL_QUESTION, "question `?`") \
\
	/* AST */ \
	X(CANE_SYMBOL_COERCE, "coerce") \
	X(CANE_SYMBOL_CONCATENATE, "concatenate") \
	X(CANE_SYMBOL_CALL, "call") \
	X(CANE_SYMBOL_ASSIGN, "assign") \
	X(CANE_SYMBOL_FUNCTION, "function") \
	X(CANE_SYMBOL_REPEAT, "repeat") \
	X(CANE_SYMBOL_MAP, "map") \
	X(CANE_SYMBOL_INVERT, "invert") \
	X(CANE_SYMBOL_REVERSE, "reverse") \
	X(CANE_SYMBOL_EUCLIDEAN, "euclidean") \
\
	X(CANE_SYMBOL_ABS, "abs") \
	X(CANE_SYMBOL_NEG, "neg") \
\
	X(CANE_SYMBOL_LSHIFT, "lshift") \
	X(CANE_SYMBOL_RSHIFT, "rshift") \
\
	X(CANE_SYMBOL_LCHEVRON, "lchevron `<`") \
	X(CANE_SYMBOL_RCHEVRON, "rchevron `>`") \
\
	/* Operators */ \
	X(CANE_SYMBOL_LCM, "lcm") \
	X(CANE_SYMBOL_GCD, "gcd") \
\
	X(CANE_SYMBOL_RANDOM, "random") \
\
	X(CANE_SYMBOL_OR, "or") \
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
	X(CANE_SYMBOL_LBRACE, "lbrace `{`") \
	X(CANE_SYMBOL_RBRACE, "rbrace `}`") \
	X(CANE_SYMBOL_LBRACKET, "lbracket `[`") \
	X(CANE_SYMBOL_RBRACKET, "rbracket `]`") \
\
	/* Cons Lists */ \
	X(CANE_SYMBOL_STATEMENT, "statement") \
	X(CANE_SYMBOL_LAYER, "layer") \
\
	/* Atoms */ \
	X(CANE_SYMBOL_NUMBER, "number") \
	X(CANE_SYMBOL_STRING, "string") \
	X(CANE_SYMBOL_IDENTIFIER, "identifier") \
	X(CANE_SYMBOL_RHYTHM, "rhythm") \
	X(CANE_SYMBOL_MELODY, "melody") \
	X(CANE_SYMBOL_BEAT, "beat") \
	X(CANE_SYMBOL_REST, "rest") \
\
	/* Annotations */ \
	X(CANE_SYMBOL_ANNOTATION_NUMBER, "number (type)") \
	X(CANE_SYMBOL_ANNOTATION_STRING, "string (type)") \
	X(CANE_SYMBOL_ANNOTATION_RHYTHM, "rhythm (type)") \
	X(CANE_SYMBOL_ANNOTATION_MELODY, "melody (type)") \
	X(CANE_SYMBOL_ANNOTATION_SEQUENCE, "sequence (type)") \
	X(CANE_SYMBOL_ANNOTATION_PATTERN, "pattern (type)") \
\
	/* === Type Specific Symbols (Assigned during typechecking) === */ \
\
	/* PREFIX */ \
	X(CANE_SYMBOL_ABS_SCALAR, "scalar abs") \
	X(CANE_SYMBOL_NEG_SCALAR, "scalar neg") \
\
	X(CANE_SYMBOL_INVERT_RHYTHM, "rhythm invert") \
	X(CANE_SYMBOL_REVERSE_RHYTHM, "rhythm reverse") \
\
	X(CANE_SYMBOL_REVERSE_MELODY, "melody reverse") \
\
	/* SCALAR */ \
	X(CANE_SYMBOL_ADD_SCALAR_SCALAR, "scalar add") \
	X(CANE_SYMBOL_SUB_SCALAR_SCALAR, "scalar sub") \
	X(CANE_SYMBOL_MUL_SCALAR_SCALAR, "scalar mul") \
	X(CANE_SYMBOL_DIV_SCALAR_SCALAR, "scalar div") \
\
	X(CANE_SYMBOL_LSHIFT_SCALAR_SCALAR, "scalar lshift") \
	X(CANE_SYMBOL_RSHIFT_SCALAR_SCALAR, "scalar rshift") \
\
	X(CANE_SYMBOL_LCM_SCALAR_SCALAR, "scalar lcm") \
	X(CANE_SYMBOL_GCD_SCALAR_SCALAR, "scalar gcd") \
\
	X(CANE_SYMBOL_EUCLIDEAN_SCALAR_SCALAR, "scalar euclidean") \
	X(CANE_SYMBOL_CONCATENATE_SCALAR_SCALAR, "scalar concatenate") \
\
	X(CANE_SYMBOL_RANDOM_SCALAR_SCALAR, "scalar random") \
\
	/* MELODY */ \
	X(CANE_SYMBOL_MAP_MELODY_RHYTHM, "melody map") \
\
	X(CANE_SYMBOL_LSHIFT_MELODY_SCALAR, "melody lshift") \
	X(CANE_SYMBOL_RSHIFT_MELODY_SCALAR, "melody rshift") \
\
	X(CANE_SYMBOL_ADD_MELODY_SCALAR, "melody add") \
	X(CANE_SYMBOL_SUB_MELODY_SCALAR, "melody sub") \
	X(CANE_SYMBOL_MUL_MELODY_SCALAR, "melody mul") \
	X(CANE_SYMBOL_DIV_MELODY_SCALAR, "melody div") \
\
	X(CANE_SYMBOL_REPEAT_MELODY_SCALAR, "melody repeat") \
	X(CANE_SYMBOL_CONCATENATE_MELODY_MELODY, "melody concatenate") \
\
	/* RHYTHM */ \
	X(CANE_SYMBOL_MAP_RHYTHM_MELODY, "rhythm map") \
\
	X(CANE_SYMBOL_LSHIFT_RHYTHM_SCALAR, "rhythm lshift") \
	X(CANE_SYMBOL_RSHIFT_RHYTHM_SCALAR, "rhythm rshift") \
\
	X(CANE_SYMBOL_REPEAT_RHYTHM_SCALAR, "rhythm repeat") \
	X(CANE_SYMBOL_CONCATENATE_RHYTHM_RHYTHM, "rhythm concatenate") \
\
	X(CANE_SYMBOL_OR_RHYTHM_RHYTHM, "rhythm or") \
	X(CANE_SYMBOL_XOR_RHYTHM_RHYTHM, "rhythm xor") \
	X(CANE_SYMBOL_AND_RHYTHM_RHYTHM, "rhythm and") \
\
	/* SEQUENCE */ \
	X(CANE_SYMBOL_CONCATENATE_SEQUENCE_SEQUENCE, "sequence concatenate") \
\
	X(CANE_SYMBOL_MUL_SEQUENCE_SCALAR, "sequence mul") \
	X(CANE_SYMBOL_DIV_SEQUENCE_SCALAR, "sequence div")

#define X(x, y) x,

typedef enum {
	CANE_SYMBOL_KINDS CANE_SYMBOL_TOTAL
} cane_symbol_kind_t;

#undef X

// Maps the enum const direct to a string
#define X(x, y) [x] = #x,
const char* CANE_SYMBOL_TO_STR[] = {CANE_SYMBOL_KINDS};
#undef X

// Map the enum const to a human readable string
#define X(x, y) [x] = y,
const char* CANE_SYMBOL_TO_STR_HUMAN[] = {CANE_SYMBOL_KINDS};
#undef X

///////////
// Types //
///////////

#define CANE_TYPE_KINDS \
	X(CANE_TYPE_NONE, "none") \
\
	X(CANE_TYPE_SCALAR, "scalar") \
	X(CANE_TYPE_STRING, "string") \
\
	X(CANE_TYPE_MELODY, "melody") \
	X(CANE_TYPE_RHYTHM, "rhythm") \
	X(CANE_TYPE_SEQUENCE, "sequence") \
	X(CANE_TYPE_PATTERN, "pattern") \
	X(CANE_TYPE_FUNCTION, "function")

#define X(x, y) x,

typedef enum {
	CANE_TYPE_KINDS CANE_TYPE_TOTAL
} cane_type_kind_t;

#undef X

// Maps the enum const direct to a string
#define X(x, y) [x] = #x,
const char* CANE_TYPE_KIND_TO_STR[] = {CANE_TYPE_KINDS};
#undef X

// Map the enum const to a human readable string
#define X(x, y) [x] = y,
const char* CANE_TYPE_KIND_TO_STR_HUMAN[] = {CANE_TYPE_KINDS};
#undef X

///////////
// Opfix //
///////////

#define CANE_OPFIX_KINDS \
	X(CANE_OPFIX_PREFIX, "prefix") \
	X(CANE_OPFIX_INFIX, "infix") \
	X(CANE_OPFIX_POSTFIX, "postfix") \
\
	/* For use with AST passes since prefix/postfix have the same \
	 * representation. */ \
	X(CANE_OPFIX_UNARY, "unary") \
	X(CANE_OPFIX_BINARY, "binary")

#define X(x, y) x,

typedef enum {
	CANE_OPFIX_KINDS CANE_OPFIX_TOTAL
} cane_opfix_kind_t;

#undef X

// Maps the enum const direct to a string
#define X(x, y) [x] = #x,
const char* CANE_OPFIX_KIND_TO_STR[] = {CANE_OPFIX_KINDS};
#undef X

// Map the enum const to a human readable string
#define X(x, y) [x] = y,
const char* CANE_OPFIX_KIND_TO_STR_HUMAN[] = {CANE_OPFIX_KINDS};
#undef X

#endif
