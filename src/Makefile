######################################################
#  GNU Make File for Mind Compiler
#
#
#  NOTE: Please adjust me and run "make depend" after
#        adding new files.
#

.SUFFIXES:
.SUFFIXES: .cpp .o

LEX = flex
YACC = bison
DEFINES =
# Use g++-8 in non-CI environment
ifeq ($(CI),)
    CXX = g++-8
    CC = gcc-8
else
    CXX = g++
    CC = gcc
endif
INCLUDES = -I .
CFLAGS = $(INCLUDES) $(DEFINES)  -g -Wall -pipe -DUSING_GCC
CXXFLAGS = $(INCLUDES) $(DEFINES)  -g -Wall -pipe -DUSING_GCC
YFLAGS = -dv
LDFLAGS = -lm -lgc -g
CSH = bash

# lex.yy.c is usually compiled with -O to speed it up.

LEXCFLAGS = -O $(CFLAGS)

SCANNER = scanner.cpp
PARSER  = parser.cpp parser.hpp location.hh position.hh stack.hh
AST     = ast/ast.o ast/ast_add_expr.o ast/ast_and_expr.o  ast/ast_or_expr.o\
          ast/ast_assign_expr.o ast/ast_bool_const.o \
          ast/ast_bool_type.o ast/ast_bitnot_expr.o \
          ast/ast_cmp_expr.o ast/ast_div_expr.o ast/ast_equ_expr.o ast/ast_neq_expr.o ast/ast_expr_stmt.o \
          ast/ast_if_stmt.o ast/ast_int_const.o ast/ast_int_type.o \
          ast/ast_lvalue_expr.o ast/ast_mod_expr.o \
          ast/ast_mul_expr.o ast/ast_neg_expr.o \
          ast/ast_not_expr.o \
          ast/ast_program.o ast/ast_func_defn.o ast/ast_func_call_expr.o \
          ast/ast_return_stmt.o ast/ast_sub_expr.o \
          ast/ast_var_decl.o ast/ast_var_ref.o  ast/ast_while_stmt.o ast/ast_comp_stmt.o 
TYPE    = type/type.o type/base_type.o type/array_type.o type/func_type.o
SYMTAB  = symb/symbol.o symb/variable.o symb/function.o
SCOPE   = scope/scope_stack.o scope/scope.o \
          scope/global_scope.o scope/func_scope.o scope/local_scope.o
TAC     = tac/tac.o tac/trans_helper.o tac/flow_graph.o
ASM     = asm/offset_counter.o asm/riscv_md.o asm/riscv_frame_manager.o
FRONTEND = scanner.o parser.o
TRANSLATION     = translation/translation.o translation/build_sym.o translation/type_check.o
DATAFLOW = tac/dataflow.o
OBJS    = main.o compiler.o \
	  options.o error.o misc.o \
          $(AST) $(TYPE) $(SYMTAB) $(SCOPE) $(TAC) $(ASM) \
	  $(FRONTEND) $(TRANSLATION) $(DATAFLOW)


all:	$(SCANNER) $(PARSER) $(OBJS)
	$(CXX) $(OBJS) $(LDFLAGS) -o mind

$(SCANNER): frontend/scanner.l
	$(LEX) $(LFLAGS) $<

$(PARSER): frontend/parser.y
	$(YACC) $(YFLAGS) $<

clean:
	rm -f mind *.o *.output $(SCANNER) $(PARSER) $(OBJS)

#
# DO NOT DELETE THIS LINE -- make depend depends on it.


compiler.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
compiler.o: error.hpp ast/ast.hpp scope/scope.hpp scope/scope_stack.hpp | $(SCANNER) $(PARSER)
compiler.o: 3rdparty/stack.hpp tac/tac.hpp 3rdparty/set.hpp asm/riscv_md.hpp | $(SCANNER) $(PARSER)
compiler.o: asm/mach_desc.hpp asm/riscv_frame_manager.hpp compiler.hpp | $(SCANNER) $(PARSER)
compiler.o: options.hpp tac/flow_graph.hpp 3rdparty/vector.hpp | $(SCANNER) $(PARSER)
error.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
error.o: error.hpp symb/symbol.hpp type/type.hpp scope/scope.hpp location.hpp | $(SCANNER) $(PARSER)
error.o: errorbuf.hpp | $(SCANNER) $(PARSER)
main.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
main.o: error.hpp compiler.hpp options.hpp | $(SCANNER) $(PARSER)
misc.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
misc.o: error.hpp location.hpp | $(SCANNER) $(PARSER)
options.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
options.o: error.hpp options.hpp | $(SCANNER) $(PARSER)
parser.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
parser.o: error.hpp ast/ast.hpp location.hpp compiler.hpp | $(SCANNER) $(PARSER)
scanner.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
scanner.o: error.hpp ast/ast.hpp parser.hpp location.hpp | $(SCANNER) $(PARSER)
ast/ast.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
ast/ast.o: error.hpp ast/ast.hpp options.hpp location.hpp | $(SCANNER) $(PARSER)
ast/ast_add_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_add_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_and_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_and_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_assign_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_assign_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_bitnot_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_bitnot_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_bool_const.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_bool_const.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_bool_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_bool_type.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_cmp_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_cmp_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_while_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_while_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_comp_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_comp_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_div_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_div_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_equ_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_equ_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_expr_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_expr_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_func_defn.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_func_defn.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_func_call_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_func_call_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_if_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_if_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_int_const.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_int_const.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_int_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_int_type.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_lvalue_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_lvalue_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_mod_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_mod_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_mul_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_mul_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_neg_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_neg_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_neq_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_neq_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_not_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_not_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_or_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_or_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_program.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_program.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_return_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_return_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_sub_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_sub_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_var_decl.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_var_decl.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
ast/ast_var_ref.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
ast/ast_var_ref.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
tac/flow_graph.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
tac/flow_graph.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER) $(PARSER)
tac/flow_graph.o: tac/flow_graph.hpp 3rdparty/vector.hpp asm/mach_desc.hpp | $(SCANNER) $(PARSER)
tac/flow_graph.o: 3rdparty/map.hpp | $(SCANNER) $(PARSER)
tac/tac.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
tac/tac.o: error.hpp tac/tac.hpp 3rdparty/set.hpp tac/flow_graph.hpp | $(SCANNER) $(PARSER)
tac/tac.o: 3rdparty/vector.hpp asm/mach_desc.hpp options.hpp | $(SCANNER) $(PARSER)
tac/trans_helper.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
tac/trans_helper.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER) $(PARSER)
tac/trans_helper.o: tac/trans_helper.hpp symb/symbol.hpp type/type.hpp | $(SCANNER) $(PARSER)
tac/trans_helper.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER) $(PARSER)
tac/trans_helper.o: asm/mach_desc.hpp asm/offset_counter.hpp | $(SCANNER) $(PARSER)
symb/function.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
symb/function.o: 3rdparty/list.hpp error.hpp symb/symbol.hpp type/type.hpp | $(SCANNER) $(PARSER)
symb/function.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER) $(PARSER)
symb/function.o: tac/tac.hpp 3rdparty/set.hpp | $(SCANNER) $(PARSER)
symb/symbol.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
symb/symbol.o: error.hpp symb/symbol.hpp type/type.hpp scope/scope.hpp | $(SCANNER) $(PARSER)
symb/variable.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
symb/variable.o: 3rdparty/list.hpp error.hpp symb/symbol.hpp type/type.hpp | $(SCANNER) $(PARSER)
symb/variable.o: scope/scope.hpp | $(SCANNER) $(PARSER)
type/array_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
type/array_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER) $(PARSER)
type/base_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
type/base_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER) $(PARSER)
type/func_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
type/func_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER) $(PARSER)
type/type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
type/type.o: error.hpp type/type.hpp | $(SCANNER) $(PARSER)
scope/func_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
scope/func_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp symb/symbol.hpp | $(SCANNER) $(PARSER)
scope/func_scope.o: type/type.hpp 3rdparty/vector.hpp | $(SCANNER) $(PARSER)
scope/global_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
scope/global_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp | $(SCANNER) $(PARSER)
scope/global_scope.o: symb/symbol.hpp type/type.hpp 3rdparty/vector.hpp | $(SCANNER) $(PARSER)
scope/local_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
scope/local_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp symb/symbol.hpp | $(SCANNER) $(PARSER)
scope/local_scope.o: type/type.hpp 3rdparty/vector.hpp | $(SCANNER) $(PARSER)
scope/scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
scope/scope.o: error.hpp scope/scope.hpp symb/symbol.hpp type/type.hpp | $(SCANNER) $(PARSER)
scope/scope.o: location.hpp | $(SCANNER) $(PARSER)
scope/scope_stack.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
scope/scope_stack.o: 3rdparty/list.hpp error.hpp scope/scope_stack.hpp | $(SCANNER) $(PARSER)
scope/scope_stack.o: scope/scope.hpp 3rdparty/stack.hpp | $(SCANNER) $(PARSER)
asm/offset_counter.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
asm/offset_counter.o: 3rdparty/list.hpp error.hpp asm/offset_counter.hpp | $(SCANNER) $(PARSER)
translation/build_sym.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
translation/build_sym.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
translation/build_sym.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER) $(PARSER)
translation/build_sym.o: symb/symbol.hpp type/type.hpp compiler.hpp | $(SCANNER) $(PARSER)
translation/type_check.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
translation/type_check.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER) $(PARSER)
translation/type_check.o: type/type.hpp scope/scope_stack.hpp scope/scope.hpp | $(SCANNER) $(PARSER)
translation/type_check.o: 3rdparty/stack.hpp symb/symbol.hpp compiler.hpp | $(SCANNER) $(PARSER)
translation/translation.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
translation/translation.o: 3rdparty/list.hpp error.hpp ast/ast.hpp symb/symbol.hpp | $(SCANNER) $(PARSER)
translation/translation.o: type/type.hpp scope/scope.hpp tac/trans_helper.hpp | $(SCANNER) $(PARSER)
translation/translation.o: tac/tac.hpp 3rdparty/set.hpp translation/translation.hpp | $(SCANNER) $(PARSER)
translation/translation.o: ast/visitor.hpp 3rdparty/vector.hpp compiler.hpp asm/offset_counter.hpp | $(SCANNER) $(PARSER)
tac/dataflow.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
tac/dataflow.o: error.hpp tac/tac.hpp 3rdparty/set.hpp tac/flow_graph.hpp | $(SCANNER) $(PARSER)
tac/dataflow.o: 3rdparty/vector.hpp asm/mach_desc.hpp | $(SCANNER) $(PARSER)
asm/riscv_frame_manager.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER) $(PARSER)
asm/riscv_frame_manager.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER) $(PARSER)
asm/riscv_frame_manager.o: asm/riscv_frame_manager.hpp | $(SCANNER) $(PARSER)
asm/riscv_md.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER) $(PARSER)
asm/riscv_md.o: error.hpp scope/scope.hpp symb/symbol.hpp type/type.hpp | $(SCANNER) $(PARSER)
asm/riscv_md.o: asm/riscv_md.hpp 3rdparty/set.hpp asm/mach_desc.hpp | $(SCANNER) $(PARSER)
asm/riscv_md.o: asm/riscv_frame_manager.hpp asm/offset_counter.hpp | $(SCANNER) $(PARSER)
asm/riscv_md.o: tac/tac.hpp tac/flow_graph.hpp 3rdparty/vector.hpp options.hpp | $(SCANNER) $(PARSER)
