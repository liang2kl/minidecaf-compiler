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
LDFLAGS = -lm -lgc -g
CSH = bash

# lex.yy.c is usually compiled with -O to speed it up.

LFLAGS = -8
LEXCFLAGS = -O $(CFLAGS)

SCANNER = scanner.cpp
AST     = ast/ast.o ast/ast_add_expr.o ast/ast_and_expr.o  ast/ast_or_expr.o\
          ast/ast_assign_expr.o ast/ast_bool_const.o \
          ast/ast_bool_type.o ast/ast_bitnot_expr.o \
          ast/ast_cmp_expr.o ast/ast_div_expr.o ast/ast_equ_expr.o ast/ast_neq_expr.o ast/ast_expr_stmt.o \
          ast/ast_if_stmt.o ast/ast_int_const.o ast/ast_int_type.o \
          ast/ast_lvalue_expr.o ast/ast_mod_expr.o \
          ast/ast_mul_expr.o ast/ast_neg_expr.o \
          ast/ast_not_expr.o \
          ast/ast_program.o ast/ast_func_defn.o \
          ast/ast_return_stmt.o ast/ast_sub_expr.o \
          ast/ast_var_decl.o ast/ast_var_ref.o  ast/ast_while_stmt.o ast/ast_comp_stmt.o 
TYPE    = type/type.o type/base_type.o type/array_type.o type/func_type.o
SYMTAB  = symb/symbol.o symb/variable.o symb/function.o
SCOPE   = scope/scope_stack.o scope/scope.o \
          scope/global_scope.o scope/func_scope.o scope/local_scope.o
TAC     = tac/tac.o tac/trans_helper.o tac/flow_graph.o
ASM     = asm/offset_counter.o asm/riscv_md.o asm/riscv_frame_manager.o
FRONTEND = scanner.o frontend/myparser.o
TRANSLATION     = translation/translation.o translation/build_sym.o translation/type_check.o
DATAFLOW = tac/dataflow.o
OBJS    = main.o compiler.o \
	  options.o error.o misc.o \
          $(AST) $(TYPE) $(SYMTAB) $(SCOPE) $(TAC) $(ASM) \
	  $(FRONTEND) $(TRANSLATION) $(DATAFLOW)


all:	$(SCANNER) $(OBJS)
	$(CXX) $(OBJS) $(LDFLAGS) -o mind

$(SCANNER): frontend/scanner.l
	$(LEX) $(LFLAGS) $<

clean:
	rm -f mind *.o *.output $(SCANNER) $(OBJS)

#
# DO NOT DELETE THIS LINE -- make depend depends on it.


compiler.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
compiler.o: error.hpp ast/ast.hpp scope/scope.hpp scope/scope_stack.hpp | $(SCANNER)
compiler.o: 3rdparty/stack.hpp tac/tac.hpp 3rdparty/set.hpp asm/riscv_md.hpp | $(SCANNER)
compiler.o: asm/mach_desc.hpp asm/riscv_frame_manager.hpp compiler.hpp | $(SCANNER)
compiler.o: options.hpp tac/flow_graph.hpp 3rdparty/vector.hpp | $(SCANNER)
error.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
error.o: error.hpp symb/symbol.hpp type/type.hpp scope/scope.hpp location.hpp | $(SCANNER)
error.o: errorbuf.hpp | $(SCANNER)
main.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
main.o: error.hpp compiler.hpp options.hpp | $(SCANNER)
misc.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
misc.o: error.hpp location.hpp | $(SCANNER)
options.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
options.o: error.hpp options.hpp | $(SCANNER)
frontend/myparser.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp frontend/myparser.hpp frontend/myparser.cpp | $(SCANNER)
frontend/myparser.o: error.hpp ast/ast.hpp location.hpp compiler.hpp | $(SCANNER)
scanner.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
scanner.o: error.hpp ast/ast.hpp frontend/myparser.hpp location.hpp | $(SCANNER)
ast/ast.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
ast/ast.o: error.hpp ast/ast.hpp options.hpp location.hpp | $(SCANNER)
ast/ast_add_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_add_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_and_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_and_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_assign_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_assign_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_bitnot_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_bitnot_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_bool_const.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_bool_const.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_bool_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_bool_type.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_cmp_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_cmp_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_while_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_while_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_comp_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_comp_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_div_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_div_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_equ_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_equ_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_expr_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_expr_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_func_defn.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_func_defn.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_if_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_if_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_int_const.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_int_const.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_int_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_int_type.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_lvalue_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_lvalue_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_mod_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_mod_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_mul_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_mul_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_neg_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_neg_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_neq_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_neq_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_not_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_not_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_or_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_or_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_program.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_program.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_return_stmt.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_return_stmt.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_sub_expr.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_sub_expr.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_var_decl.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_var_decl.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
ast/ast_var_ref.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
ast/ast_var_ref.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
tac/flow_graph.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
tac/flow_graph.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER)
tac/flow_graph.o: tac/flow_graph.hpp 3rdparty/vector.hpp asm/mach_desc.hpp | $(SCANNER)
tac/flow_graph.o: 3rdparty/map.hpp | $(SCANNER)
tac/tac.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
tac/tac.o: error.hpp tac/tac.hpp 3rdparty/set.hpp tac/flow_graph.hpp | $(SCANNER)
tac/tac.o: 3rdparty/vector.hpp asm/mach_desc.hpp options.hpp | $(SCANNER)
tac/trans_helper.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
tac/trans_helper.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER)
tac/trans_helper.o: tac/trans_helper.hpp symb/symbol.hpp type/type.hpp | $(SCANNER)
tac/trans_helper.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER)
tac/trans_helper.o: asm/mach_desc.hpp asm/offset_counter.hpp | $(SCANNER)
symb/function.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
symb/function.o: 3rdparty/list.hpp error.hpp symb/symbol.hpp type/type.hpp | $(SCANNER)
symb/function.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER)
symb/function.o: tac/tac.hpp 3rdparty/set.hpp | $(SCANNER)
symb/symbol.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
symb/symbol.o: error.hpp symb/symbol.hpp type/type.hpp scope/scope.hpp | $(SCANNER)
symb/variable.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
symb/variable.o: 3rdparty/list.hpp error.hpp symb/symbol.hpp type/type.hpp | $(SCANNER)
symb/variable.o: scope/scope.hpp | $(SCANNER)
type/array_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
type/array_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER)
type/base_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
type/base_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER)
type/func_type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
type/func_type.o: 3rdparty/list.hpp error.hpp type/type.hpp | $(SCANNER)
type/type.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
type/type.o: error.hpp type/type.hpp | $(SCANNER)
scope/func_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
scope/func_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp symb/symbol.hpp | $(SCANNER)
scope/func_scope.o: type/type.hpp 3rdparty/vector.hpp | $(SCANNER)
scope/global_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
scope/global_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp | $(SCANNER)
scope/global_scope.o: symb/symbol.hpp type/type.hpp 3rdparty/vector.hpp | $(SCANNER)
scope/local_scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
scope/local_scope.o: 3rdparty/list.hpp error.hpp scope/scope.hpp symb/symbol.hpp | $(SCANNER)
scope/local_scope.o: type/type.hpp 3rdparty/vector.hpp | $(SCANNER)
scope/scope.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
scope/scope.o: error.hpp scope/scope.hpp symb/symbol.hpp type/type.hpp | $(SCANNER)
scope/scope.o: location.hpp | $(SCANNER)
scope/scope_stack.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
scope/scope_stack.o: 3rdparty/list.hpp error.hpp scope/scope_stack.hpp | $(SCANNER)
scope/scope_stack.o: scope/scope.hpp 3rdparty/stack.hpp | $(SCANNER)
asm/offset_counter.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
asm/offset_counter.o: 3rdparty/list.hpp error.hpp asm/offset_counter.hpp | $(SCANNER)
translation/build_sym.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
translation/build_sym.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
translation/build_sym.o: scope/scope.hpp scope/scope_stack.hpp 3rdparty/stack.hpp | $(SCANNER)
translation/build_sym.o: symb/symbol.hpp type/type.hpp compiler.hpp | $(SCANNER)
translation/type_check.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
translation/type_check.o: 3rdparty/list.hpp error.hpp ast/ast.hpp ast/visitor.hpp | $(SCANNER)
translation/type_check.o: type/type.hpp scope/scope_stack.hpp scope/scope.hpp | $(SCANNER)
translation/type_check.o: 3rdparty/stack.hpp symb/symbol.hpp compiler.hpp | $(SCANNER)
translation/translation.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
translation/translation.o: 3rdparty/list.hpp error.hpp ast/ast.hpp symb/symbol.hpp | $(SCANNER)
translation/translation.o: type/type.hpp scope/scope.hpp tac/trans_helper.hpp | $(SCANNER)
translation/translation.o: tac/tac.hpp 3rdparty/set.hpp translation/translation.hpp | $(SCANNER)
translation/translation.o: ast/visitor.hpp 3rdparty/vector.hpp compiler.hpp asm/offset_counter.hpp | $(SCANNER)
tac/dataflow.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
tac/dataflow.o: error.hpp tac/tac.hpp 3rdparty/set.hpp tac/flow_graph.hpp | $(SCANNER)
tac/dataflow.o: 3rdparty/vector.hpp asm/mach_desc.hpp | $(SCANNER)
asm/riscv_frame_manager.o: config.hpp 3rdparty/boehmgc.hpp define.hpp | $(SCANNER)
asm/riscv_frame_manager.o: 3rdparty/list.hpp error.hpp tac/tac.hpp 3rdparty/set.hpp | $(SCANNER)
asm/riscv_frame_manager.o: asm/riscv_frame_manager.hpp | $(SCANNER)
asm/riscv_md.o: config.hpp 3rdparty/boehmgc.hpp define.hpp 3rdparty/list.hpp | $(SCANNER)
asm/riscv_md.o: error.hpp scope/scope.hpp symb/symbol.hpp type/type.hpp | $(SCANNER)
asm/riscv_md.o: asm/riscv_md.hpp 3rdparty/set.hpp asm/mach_desc.hpp | $(SCANNER)
asm/riscv_md.o: asm/riscv_frame_manager.hpp asm/offset_counter.hpp | $(SCANNER)
asm/riscv_md.o: tac/tac.hpp tac/flow_graph.hpp 3rdparty/vector.hpp options.hpp | $(SCANNER)
