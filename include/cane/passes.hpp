#ifndef CANE_PASSES_HPP
#define CANE_PASSES_HPP

#include <memory>
#include <print>
#include <vector>

#include <cane/parse.hpp>

namespace cane {

	//////////////////////
	// AST Printer Pass //
	//////////////////////

	inline void pass_print_walker(
		std::shared_ptr<ASTNode> node,
		std::vector<bool> bits = {},
		size_t depth = 0
	);

	namespace detail {
		inline std::string_view SIGIL_NULL = CANE_CSTR("⊗");
		inline std::string_view SIGIL_BRANCH = CANE_CSTR("○");
		inline std::string_view SIGIL_LEAF = CANE_CSTR("►");

		inline std::string_view SIGIL_LHS = CANE_CSTR("├");
		inline std::string_view SIGIL_RHS = CANE_CSTR("└");

		inline void pass_print_indent_node(
			std::string_view bar, std::vector<bool> bits = {}
		) {
			for (bool b: bits) {
				std::print(" {}", b ? "│" : " ");
			}

			std::print(" {}", bar);
		}

		inline void pass_print_indent_node_child_lhs(
			std::shared_ptr<ASTNode> node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_LHS, bits);

			bits.push_back(true);
			pass_print_walker(node->lhs, bits, depth);
			bits.pop_back();
		}

		inline void pass_print_indent_node_child_rhs(
			std::shared_ptr<ASTNode> node,
			std::vector<bool> bits = {},
			size_t depth = 0
		) {
			pass_print_indent_node(SIGIL_RHS, bits);

			bits.push_back(false);
			pass_print_walker(node->rhs, bits, depth);
			bits.pop_back();
		}
	}  // namespace detail

	inline void pass_print(std::shared_ptr<ASTNode> node) {
		CANE_FUNC();
		pass_print_walker(node);
	}

	inline void pass_print_walker(
		std::shared_ptr<ASTNode> node, std::vector<bool> bits, size_t depth
	) {
		if (node == nullptr) {
			std::cout << detail::SIGIL_NULL << '\n';
			return;
		}

		bool is_root = depth == 0;
		bool is_leaf = cane::is_literal(node->kind);

		auto joiner = is_root ? " " : "─";
		auto marker = is_leaf ? detail::SIGIL_LEAF : detail::SIGIL_BRANCH;

		std::cout << joiner << marker;

		auto symbol_sv = symbol_kind_to_str_human(node->kind);
		auto node_sv = node->sv;
		auto type_sv = type_kind_to_str(node->type);

		std::print(" " CANE_COLOUR_YELLOW "{}" CANE_RESET, symbol_sv);
		std::print(" " CANE_COLOUR_BLUE "\"{}\"" CANE_RESET, node_sv);
		std::print(" " CANE_COLOUR_RED "{}" CANE_RESET, type_sv);

		std::cout << '\n';

		if (not is_leaf) {
			detail::pass_print_indent_node_child_lhs(node, bits, depth + 1);
			detail::pass_print_indent_node_child_rhs(node, bits, depth + 1);
		}
	}

	//////////////////
	// AST GraphViz //
	//////////////////

	// static void cane_pass_graphviz_edge(
	// 	cane_file_t fp, cane_ast_node_t* node, size_t parent, size_t self
	// ) {
	// 	cane_symbol_kind_t kind = CANE_SYMBOL_NONE;
	// 	cane_string_view_t sv = CANE_SV("NULL");

	// 	TypeKind type = CANE_TYPE_NONE;

	// 	if (node != NULL) {
	// 		kind = node->kind;
	// 		sv = node->location.symbol;

	// 		type = node->type;
	// 	}

	// 	cane_string_view_info_t info = cane_string_view_info(sv);

	// 	fprintf(
	// 		fp,
	// 		"  n%zu [label=\"kind = {}\nsv = `%.*s`\ntype = {}\"];\n",
	// 		self,
	// 		CANE_SYMBOL_TO_STR_HUMAN[kind],
	// 		(int)info.length,
	// 		info.ptr,
	// 		CANE_TYPE_KIND_TO_STR_HUMAN[type]
	// 	);

	// 	if (parent != self) {
	// 		fprintf(fp, "  n%zu -> n%zu;\n", parent, self);
	// 	}
	// }

	// static void cane_pass_graphviz_walker(
	// 	cane_file_t fp, cane_ast_node_t* node, size_t* id, size_t parent
	// );

	// static void cane_pass_graphviz(cane_ast_node_t* node, cane_file_t fp) {
	// 	CANE_FUNCTION_ENTER();

	// 	size_t id = 0;

	// 	fprintf(fp, "digraph {\n");
	// 	fprintf(fp, "  node [shape=box style=filled fillcolor=\"#bfbfbf\"];\n");

	// 	cane_pass_graphviz_walker(fp, node, &id, 0);

	// 	fprintf(fp, "}\n");
	// }

	// static void cane_pass_graphviz_walker(
	// 	cane_file_t fp, cane_ast_node_t* node, size_t* id, size_t parent
	// ) {
	// 	size_t self = (*id)++;

	// 	if (node == NULL) {
	// 		cane_pass_graphviz_edge(fp, node, parent, self);
	// 		return;
	// 	}

	// 	cane_symbol_kind_t kind = node->kind;

	// 	cane_location_t loc = node->location;

	// 	cane_ast_node_t* lhs = node->lhs;
	// 	cane_ast_node_t* rhs = node->rhs;

	// 	switch (kind) {
	// 		// Literals
	// 		case CANE_SYMBOL_BEAT:
	// 		case CANE_SYMBOL_REST:

	// 		case CANE_SYMBOL_RHYTHM:
	// 		case CANE_SYMBOL_MELODY:

	// 		case CANE_SYMBOL_IDENTIFIER:
	// 		case CANE_SYMBOL_STRING:
	// 		case CANE_SYMBOL_NUMBER: {
	// 			cane_pass_graphviz_edge(fp, node, parent, self);
	// 		} break;

	// 		// Unary
	// 		case CANE_SYMBOL_ABS:
	// 		case CANE_SYMBOL_NEG:

	// 		case CANE_SYMBOL_INVERT:
	// 		case CANE_SYMBOL_REVERSE: {
	// 			cane_pass_graphviz_edge(fp, node, parent, self);
	// 			cane_pass_graphviz_walker(fp, rhs, id, self);
	// 		} break;

	// 		// Binary
	// 		case CANE_SYMBOL_ADD:
	// 		case CANE_SYMBOL_SUB:
	// 		case CANE_SYMBOL_MUL:
	// 		case CANE_SYMBOL_DIV:

	// 		case CANE_SYMBOL_LCM:
	// 		case CANE_SYMBOL_GCD:

	// 		case CANE_SYMBOL_EUCLIDEAN:
	// 		case CANE_SYMBOL_MAP:
	// 		case CANE_SYMBOL_REPEAT:

	// 		case CANE_SYMBOL_LSHIFT:
	// 		case CANE_SYMBOL_RSHIFT:

	// 		case CANE_SYMBOL_OR:
	// 		case CANE_SYMBOL_XOR:
	// 		case CANE_SYMBOL_AND:

	// 		case CANE_SYMBOL_CALL:
	// 		case CANE_SYMBOL_CONCATENATE:
	// 		case CANE_SYMBOL_RANDOM:

	// 		case CANE_SYMBOL_ASSIGN:
	// 		case CANE_SYMBOL_SEND:
	// 		case CANE_SYMBOL_FUNCTION: {
	// 			cane_pass_graphviz_edge(fp, node, parent, self);
	// 			cane_pass_graphviz_walker(fp, lhs, id, self);
	// 			cane_pass_graphviz_walker(fp, rhs, id, self);
	// 		} break;

	// 		// Lists
	// 		case CANE_SYMBOL_LAYER:
	// 		case CANE_SYMBOL_STATEMENT: {
	// 			cane_pass_graphviz_edge(fp, node, parent, self);
	// 			cane_pass_graphviz_walker(fp, lhs, id, self);
	// 			cane_pass_graphviz_walker(fp, rhs, id, self);
	// 		} break;

	// 		default: {
	// 			cane_report_and_die(
	// 				loc,
	// 				CANE_REPORT_GENERIC,
	// 				"unhandled case `{}`!",
	// 				CANE_SYMBOL_TO_STR[kind]
	// 			);
	// 		} break;
	// 	}
	// }

	// //////////////////
	// // Type Checker //
	// //////////////////

	inline TypeKind pass_semantic_analysis_walker(std::shared_ptr<ASTNode> node
	);

	inline void pass_semantic_analysis(std::shared_ptr<ASTNode> node) {
		CANE_FUNC();
		pass_semantic_analysis_walker(node);
	}

	// TODO:
	// 1. We need to store assigned types
	// 2. Function types
	inline bool type_remapper(
		std::shared_ptr<ASTNode> node,
		SymbolKind kind,
		TypeKind expected_lhs,
		TypeKind expected_rhs,
		TypeKind out,
		SymbolKind op
	) {
		if (node == nullptr or node->kind != kind) {
			return false;
		}

		// In the case of a UNARY remapping, rhs will match with NONE anyway so
		// we can always just compare both types.
		TypeKind lhs = pass_semantic_analysis_walker(node->lhs);
		TypeKind rhs = pass_semantic_analysis_walker(node->rhs);

		CANE_INFO(
			"attempt " CANE_COLOUR_BLUE "`{}`" CANE_RESET
			" [ `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
			"`, `{} = " CANE_COLOUR_MAGENTA "{}" CANE_RESET
			"` ] -> " CANE_COLOUR_MAGENTA "{}" CANE_RESET,
			symbol_kind_to_str(kind),
			type_kind_to_str_human(expected_lhs),
			type_kind_to_str_human(lhs),
			type_kind_to_str_human(expected_rhs),
			type_kind_to_str_human(rhs),
			type_kind_to_str_human(out)
		);

		if (lhs != expected_lhs || rhs != expected_rhs) {
			// If the types don't match, it just means this overload of the
			// operator isn't the correct one but we might have one handled
			// later.
			CANE_FAIL("└► " CANE_COLOUR_RED "failed!" CANE_RESET);
			return false;
		}

		CANE_OKAY("└► " CANE_COLOUR_YELLOW "success!" CANE_RESET);

		node->type = out;
		node->op = op;

		return true;
	}

	// TODO:
	// We need to figure out how to handle rhythms with beats/rests and how
	// to type check them. They really need to be a unified type.
	inline bool type_remap_trivial(std::shared_ptr<ASTNode> node) {
#define CANE_TYPE_REMAP(symbol, lhs_type, rhs_type, out_type, out_symbol) \
	type_remapper( \
		node, \
		SymbolKind::symbol, \
		TypeKind::lhs_type, \
		TypeKind::rhs_type, \
		TypeKind::out_type, \
		SymbolKind::out_symbol \
	)

		// clang-format off
 	return
 		/* Prefix/Unary */
 		CANE_TYPE_REMAP(Abs, None, Scalar, Scalar, AbsScalar) ||
 		CANE_TYPE_REMAP(Neg, None, Scalar, Scalar, NegScalar) ||

 		CANE_TYPE_REMAP(Invert, None, Rhythm, Rhythm, InvertRhythm) ||
 		CANE_TYPE_REMAP(Reverse, None, Rhythm, Rhythm, ReverseRhythm) ||

 		CANE_TYPE_REMAP(Reverse, None, Melody, Melody, ReverseMelody) ||

 		/* Scalar */
 		CANE_TYPE_REMAP(Add, Scalar, Scalar, Scalar, AddScalarScalar) ||
 		CANE_TYPE_REMAP(Sub, Scalar, Scalar, Scalar, SubScalarScalar) ||
 		CANE_TYPE_REMAP(Mul, Scalar, Scalar, Scalar, MulScalarScalar) ||
 		CANE_TYPE_REMAP(Div, Scalar, Scalar, Scalar, DivScalarScalar) ||

 		CANE_TYPE_REMAP(LeftShift, Scalar, Scalar, Scalar, LeftShiftScalarScalar) ||
 		CANE_TYPE_REMAP(RightShift, Scalar, Scalar, Scalar, RightShiftScalarScalar) ||

 		CANE_TYPE_REMAP(LCM, Scalar, Scalar, Scalar, LCMScalarScalar) ||
 		CANE_TYPE_REMAP(GCD, Scalar, Scalar, Scalar, GCDScalarScalar) ||

 		CANE_TYPE_REMAP(Euclidean, Scalar, Scalar, Scalar, EuclideanScalarScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Scalar, Scalar, Melody, ConcatenateScalarScalar) ||
		CANE_TYPE_REMAP(Random, Scalar, Scalar, Scalar, RandomScalarScalar) ||

 		/* Melody */
 		CANE_TYPE_REMAP(Map, Melody, Rhythm, Sequence, MapMelodyRhythm) ||

 		CANE_TYPE_REMAP(LeftShift, Melody, Scalar, Melody, LeftShiftMelodyScalar) ||
 		CANE_TYPE_REMAP(RightShift, Melody, Scalar, Melody, RightShiftMelodyScalar) ||

 		CANE_TYPE_REMAP(Add, Melody, Scalar, Melody, AddMelodyScalar) ||
 		CANE_TYPE_REMAP(Sub, Melody, Scalar, Melody, SubMelodyScalar) ||
 		CANE_TYPE_REMAP(Mul, Melody, Scalar, Melody, MulMelodyScalar) ||
 		CANE_TYPE_REMAP(Div, Melody, Scalar, Melody, DivMelodyScalar) ||

 		CANE_TYPE_REMAP(Repeat, Melody, Scalar, Melody, RepeatMelodyScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Melody, Melody, Melody, ConcatenateMelodyMelody) ||

 		/* Rhythm */
 		CANE_TYPE_REMAP(Map, Rhythm, Melody, Sequence, MapRhythmMelody) ||

 		CANE_TYPE_REMAP(LeftShift, Rhythm, Scalar, Rhythm, LeftShiftRhythmScalar) ||
 		CANE_TYPE_REMAP(RightShift, Rhythm, Scalar, Rhythm, RightShiftRhythmScalar) ||

 		CANE_TYPE_REMAP(Repeat, Rhythm, Scalar, Rhythm, RepeatRhythmScalar) ||
 		CANE_TYPE_REMAP(Concatenate, Rhythm, Rhythm, Rhythm, ConcatenateRhythmRhythm) ||

 		CANE_TYPE_REMAP(Or, Rhythm, Rhythm, Rhythm, OrRhythmRhythm) ||
 		CANE_TYPE_REMAP(Xor, Rhythm, Rhythm, Rhythm, XorRhythmRhythm) ||
 		CANE_TYPE_REMAP(And, Rhythm, Rhythm, Rhythm, AndRhythmRhythm) ||

 		/* Sequence */
 		CANE_TYPE_REMAP(Concatenate, Sequence, Sequence, Sequence, ConcatenateSequenceSequence) ||

 		CANE_TYPE_REMAP(Mul, Sequence, Scalar, Sequence, MulSequenceScalar) ||
 		CANE_TYPE_REMAP(Div, Sequence, Scalar, Sequence, DivSequenceScalar)
 	;

		// clang-format on

#undef CANE_TYPE_REMAP
	}

	inline TypeKind pass_semantic_analysis_walker(std::shared_ptr<ASTNode> node
	) {
		if (node == nullptr) {
			return TypeKind::None;
		}

		// Handle trivial cases for remapping first but otherwise fallback to
		// this switch where we handle them manually.
		if (not type_remap_trivial(node)) {
			switch (node->kind) {
				// Literals
				case SymbolKind::Number:
				case SymbolKind::String:

				case SymbolKind::Beat:
				case SymbolKind::Rest:

				case SymbolKind::Rhythm:
				case SymbolKind::Melody: {
					// These cases depend on the type being set during parsing.
					return node->type;
				} break;

				// Assignment
				case SymbolKind::Identifier:
				case SymbolKind::Assign:
				case SymbolKind::Send: {
					CANE_UNIMPLEMENTED();
				} break;

				// TODO: Fix this, not sure if this is actually correct in all
				// cases.
				case SymbolKind::Function: {
					return node->rhs->type;
				} break;

				case SymbolKind::Call: {
					TypeKind function =
						pass_semantic_analysis_walker(node->lhs);

					TypeKind argument =
						pass_semantic_analysis_walker(node->rhs);

					if (function != argument) {
						cane::die(
							"incorrect argument type for function `{}`!",
							symbol_kind_to_str(node->kind)
						);
					}

					return function;
				} break;

				// Statements always return the type of their last expression.
				case SymbolKind::Layer:
				case SymbolKind::Statement: {
					TypeKind lhs = pass_semantic_analysis_walker(node->lhs);
					TypeKind rhs = pass_semantic_analysis_walker(node->rhs);

					CANE_UNUSED(rhs);  // Always discard right hand type since
									   // we return the last expression's type.

					node->type = lhs;
					return lhs;
				} break;

				default: {
					cane::die(
						"unknown type mapping for `{}`!",
						symbol_kind_to_str(node->kind)
					);
				} break;
			}
		}

		return node->type;
	}

	// ///////////////
	// // Evaluator //
	// ///////////////

	// static cane_value_t cane_pass_evaluator_walker(cane_ast_node_t* node);

	// static cane_value_t cane_pass_evaluator(cane_ast_node_t* node) {
	// 	CANE_FUNCTION_ENTER();
	// 	return cane_pass_evaluator_walker(node);
	// }

	// static cane_value_t cane_pass_evaluator_walker(cane_ast_node_t* node) {
	// 	if (node == NULL) {
	// 		return (cane_value_t) {};
	// 	}

	// 	cane_location_t loc = node->location;

	// 	// Trivial/special cases
	// 	switch (node->kind) {
	// 		case CANE_SYMBOL_STATEMENT: {
	// 			cane_value_t lhs = cane_pass_evaluator_walker(node->lhs);
	// 			cane_value_t rhs = cane_pass_evaluator_walker(node->rhs);

	// 			CANE_UNUSED(rhs);
	// 			return lhs;
	// 		} break;

	// 		case CANE_SYMBOL_STRING:
	// 		case CANE_SYMBOL_NUMBER:

	// 		case CANE_SYMBOL_RHYTHM:
	// 		case CANE_SYMBOL_MELODY: {
	// 			return node->value;
	// 		} break;

	// 		default: break;
	// 	}

	// 	cane_value_t lhs = cane_pass_evaluator_walker(node->lhs);
	// 	cane_value_t rhs = cane_pass_evaluator_walker(node->rhs);

	// 	switch (node->op) {
	// 		// Unary Scalar
	// 		case CANE_SYMBOL_ABS_SCALAR:
	// 			return cane_value_create_number(abs(rhs.number));
	// 		case CANE_SYMBOL_NEG_SCALAR:
	// 			return cane_value_create_number(-rhs.number);

	// 		// Binary Scalar
	// 		case CANE_SYMBOL_ADD_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number + rhs.number);
	// 		case CANE_SYMBOL_SUB_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number - rhs.number);
	// 		case CANE_SYMBOL_MUL_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number * rhs.number);
	// 		case CANE_SYMBOL_DIV_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number / rhs.number);

	// 		case CANE_SYMBOL_LSHIFT_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number << rhs.number);
	// 		case CANE_SYMBOL_RSHIFT_SCALAR_SCALAR:
	// 			return cane_value_create_number(lhs.number >> rhs.number);

	// 		case CANE_SYMBOL_LCM_SCALAR_SCALAR:
	// 			return cane_value_create_number(cane_lcm(lhs.number,
	// rhs.number)); 		case CANE_SYMBOL_GCD_SCALAR_SCALAR: return
	// cane_value_create_number(cane_gcd(lhs.number, rhs.number));

	// 		case CANE_SYMBOL_INVERT_RHYTHM:
	// 		case CANE_SYMBOL_REVERSE_RHYTHM:
	// 		case CANE_SYMBOL_REVERSE_MELODY:

	// 		case CANE_SYMBOL_EUCLIDEAN_SCALAR_SCALAR:
	// 		case CANE_SYMBOL_RANDOM_SCALAR_SCALAR:

	// 		case CANE_SYMBOL_LSHIFT_MELODY_SCALAR:
	// 		case CANE_SYMBOL_RSHIFT_MELODY_SCALAR:
	// 		case CANE_SYMBOL_LSHIFT_RHYTHM_SCALAR:
	// 		case CANE_SYMBOL_RSHIFT_RHYTHM_SCALAR: {
	// 			// Rotate vector
	// 		} break;

	// 		case CANE_SYMBOL_ADD_MELODY_SCALAR:
	// 		case CANE_SYMBOL_SUB_MELODY_SCALAR:
	// 		case CANE_SYMBOL_MUL_MELODY_SCALAR:
	// 		case CANE_SYMBOL_DIV_MELODY_SCALAR: {
	// 			// Broadcast vector
	// 		} break;

	// 		case CANE_SYMBOL_MAP_RHYTHM_MELODY:
	// 		case CANE_SYMBOL_MAP_MELODY_RHYTHM: {
	// 			// Create sequence
	// 		} break;

	// 		case CANE_SYMBOL_REPEAT_MELODY_SCALAR:
	// 		case CANE_SYMBOL_REPEAT_RHYTHM_SCALAR: {
	// 			// Repeat vector
	// 		} break;

	// 		case CANE_SYMBOL_CONCATENATE_SCALAR_SCALAR:
	// 		case CANE_SYMBOL_CONCATENATE_MELODY_MELODY:
	// 		case CANE_SYMBOL_CONCATENATE_RHYTHM_RHYTHM:
	// 		case CANE_SYMBOL_CONCATENATE_SEQUENCE_SEQUENCE: {
	// 			// Concat vector
	// 		} break;

	// 		case CANE_SYMBOL_OR_RHYTHM_RHYTHM:
	// 		case CANE_SYMBOL_XOR_RHYTHM_RHYTHM:
	// 		case CANE_SYMBOL_AND_RHYTHM_RHYTHM: {
	// 			// Vector broadcast
	// 		} break;

	// 		case CANE_SYMBOL_MUL_SEQUENCE_SCALAR:
	// 		case CANE_SYMBOL_DIV_SEQUENCE_SCALAR: {
	// 			// Time div/mul on sequences
	// 		} break;

	// 		default: {
	// 			cane_report_and_die(
	// 				loc,
	// 				CANE_REPORT_EVAL,
	// 				"cannot evaluate `{}`!",
	// 				CANE_SYMBOL_TO_STR[node->op]
	// 			);
	// 		} break;
	// 	}

	// 	return (cane_value_t) {};
	// }

}  // namespace cane

#endif
