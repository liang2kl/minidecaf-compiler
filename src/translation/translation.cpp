/*****************************************************
 *  Implementation of the third translation pass.
 *
 *  In the third pass, we will:
 *    translate all the statements and expressions
 *
 *  Keltin Leung 
 */

#include "translation.hpp"
#include "asm/offset_counter.hpp"
#include "ast/ast.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"
#include "tac/trans_helper.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::symb;
using namespace mind::tac;
using namespace mind::type;
using namespace mind::assembly;

/* Constructor.
 *
 * PARAMETERS:
 *   helper - the translation helper
 */
Translation::Translation(tac::TransHelper *helper) {
    mind_assert(NULL != helper);

    tr = helper;
}

/* Translating an ast::Program node.
 */
void Translation::visit(ast::Program *p) {
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
}

// three sugars for parameter offset management
#define RESET_OFFSET() tr->getOffsetCounter()->reset(OffsetCounter::PARAMETER)
#define NEXT_OFFSET(x) tr->getOffsetCounter()->next(OffsetCounter::PARAMETER, x)

/* Translating an ast::FuncDefn node.
 *
 * NOTE:
 *   call tr->startFunc() before translating the statements and
 *   call tr->endFunc() after all the statements have been translated
 */
void Translation::visit(ast::FuncDefn *f) {
    Function *fun = f->ATTR(sym);

    // attaching function entry label
    fun->attachEntryLabel(tr->getNewEntryLabel(fun));

    if (f->forward_decl) {
        return;
    }

    // arguments
    int order = 0;
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        v->setOrder(order++);
        v->attachTemp(tr->getNewTempI4());
    }

    fun->offset = fun->getOrder() * POINTER_SIZE;

    RESET_OFFSET();

    tr->startFunc(fun);

    // You may process params here, i.e use reg or stack to pass parameters
    for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
        auto v = (*it)->ATTR(sym);
        tr->genGetParam(v->getTemp(), v->getOrder());
    }

    // translates statement by statement
    for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
        (*it)->accept(this);

    tr->genReturn(tr->genLoadImm4(0)); // Return 0 by default

    tr->endFunc();
}

/* Translating an ast::AssignStmt node.
 *
 * NOTE:
 *   different kinds of Lvalue require different translation
 */
void Translation::visit(ast::AssignExpr *s) {
    mind_assert(s->left->getKind() == ast::ASTNode::VAR_REF);

    ast::VarRef *ref = dynamic_cast<ast::VarRef *>(s->left);
    mind_assert(ref != NULL);
    ref->accept(this);
    s->e->accept(this);

    bool isArray = ref->isArrayRef();

    if (ref->ATTR(sym)->isGlobalVar()) {
        mind_assert(ref->ATTR(addr) != NULL);
        tr->genStore(ref->ATTR(addr), 0, s->e->ATTR(val));
    } else {
        if (isArray) {
            tr->genStore(ref->ATTR(addr), 0, s->e->ATTR(val));
        } else {
            tr->genAssign(ref->ATTR(sym)->getTemp(), s->e->ATTR(val));
        }
    }
    s->ATTR(val) = tr->getNewTempI4();
    tr->genAssign(s->ATTR(val), s->e->ATTR(val));
}

/* Translating an ast::ExprStmt node.
 */
void Translation::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Translating an ast::IfStmt node.
 *
 * NOTE:
 *   you don't need to test whether the false_brch is empty
 */
void Translation::visit(ast::IfStmt *s) {
    Label L1 = tr->getNewLabel(); // entry of the false branch
    Label L2 = tr->getNewLabel(); // exit
    s->condition->accept(this);
    tr->genJumpOnZero(L1, s->condition->ATTR(val));

    s->true_brch->accept(this);
    tr->genJump(L2); // done

    tr->genMarkLabel(L1);
    s->false_brch->accept(this);

    tr->genMarkLabel(L2);
}
/* Translating an ast::WhileStmt node.
 */
void Translation::visit(ast::WhileStmt *s) {
    Label L1 = tr->getNewLabel();
    Label L2 = tr->getNewLabel();

    Label old_break = current_break_label;
    Label old_continue = current_continue_label;
    current_break_label = L2;
    current_continue_label = L1;

    tr->genMarkLabel(L1);

    if (!s->hasDo) {
            s->condition->accept(this);
            tr->genJumpOnZero(L2, s->condition->ATTR(val));
    }

    s->loop_body->accept(this);

    if (s->hasDo) {
            s->condition->accept(this);
            tr->genJumpOnZero(L2, s->condition->ATTR(val));
    }

    tr->genJump(L1);

    tr->genMarkLabel(L2);

    current_break_label = old_break;
    current_continue_label = old_continue;
}

/* Translating an ast::BreakStmt node.
 */
void Translation::visit(ast::BreakStmt *s) { tr->genJump(current_break_label); }

void Translation::visit(ast::ContStmt *s) {
    tr->genJump(current_continue_label);
}

/* Translating an ast::CompStmt node.
 */
void Translation::visit(ast::CompStmt *c) {
    // translates statement by statement
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
}

void Translation::visit(ast::ForStmt *s) {
    Label l1 = tr->getNewLabel();
    Label l2 = tr->getNewLabel();
    Label l3 = tr->getNewLabel();

    // Visit init statement / expression
    if (s->initDecl)
        s->initDecl->accept(this);
    else if (s->initExpr)
        s->initExpr->accept(this);

    Label old_break = current_break_label;
    Label old_continue = current_continue_label;
    current_break_label = l3;
    current_continue_label = l2;

    tr->genMarkLabel(l1);

    if (s->cond) {
        s->cond->accept(this);
        tr->genJumpOnZero(l3, s->cond->ATTR(val));
    }

    if (s->body)
        s->body->accept(this);

    tr->genMarkLabel(l2);

    if (s->update)
        s->update->accept(this);

    tr->genJump(l1);

    tr->genMarkLabel(l3);

    current_break_label = old_break;
    current_continue_label = old_continue;
}

/* Translating an ast::ReturnStmt node.
 */
void Translation::visit(ast::ReturnStmt *s) {
    s->e->accept(this);
    tr->genReturn(s->e->ATTR(val));
}

/* Translating an ast::AddExpr node.
 */
void Translation::visit(ast::AddExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genAdd(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::SubExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genSub(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::MulExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMul(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::DivExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genDiv(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::ModExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMod(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::EquExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genEqu(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::NeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genNeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::LesExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLes(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::LeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::GrtExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGtr(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::GeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::AndExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLAnd(e->e1->ATTR(val), e->e2->ATTR(val));
}

void Translation::visit(ast::OrExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLOr(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::IntConst node.
 */
void Translation::visit(ast::IntConst *e) {
    e->ATTR(val) = tr->genLoadImm4(e->value);
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::NegExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genNeg(e->e->ATTR(val));
}

void Translation::visit(ast::BitNotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genBNot(e->e->ATTR(val));
}

void Translation::visit(ast::NotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genLNot(e->e->ATTR(val));
}

void Translation::visit(ast::IfExpr *e) {
    Label falseLabel = tr->getNewLabel();
    Label trueLabel = tr->getNewLabel();

    e->condition->accept(this);

    Temp temp = tr->getNewTempI4();
    
    tr->genJumpOnZero(falseLabel, e->condition->ATTR(val));
    e->true_brch->accept(this);
    tr->genAssign(temp, e->true_brch->ATTR(val));
    tr->genJump(trueLabel);
    tr->genMarkLabel(falseLabel);
    e->false_brch->accept(this);
    tr->genAssign(temp, e->false_brch->ATTR(val));

    tr->genMarkLabel(trueLabel);

    e->ATTR(val) = temp;
}

/* Translating an ast::LvalueExpr node.
 *
 * NOTE:
 *   different Lvalue kinds need different translation
 */
void Translation::visit(ast::LvalueExpr *e) {
    ast::VarRef *ref;
    if (e->lvalue->getKind() == ast::ASTNode::VAR_REF) {
        ref = dynamic_cast<ast::VarRef *>(e->lvalue);
        mind_assert(ref != NULL);
        ref->accept(this);

        bool isArrayRef = ref->isArrayRef();
        bool isArrayType = ref->ATTR(sym)->getType()->isArrayType();

        if (ref->ATTR(sym)->isGlobalVar()) {
            if (isArrayRef || !isArrayType) {
                e->ATTR(val) = tr->genLoad(ref->ATTR(addr), 0);
            } else {
                e->ATTR(val) = tr->genLoadSym(ref->ATTR(sym)->getLabel());
            }
        } else {
            if (isArrayRef) {
                e->ATTR(val) = tr->genLoad(ref->ATTR(addr), 0);
            } else {
                e->ATTR(val) = ref->ATTR(sym)->getTemp();
            }
        }

    } else {
        mind_assert(false);
    }
}

/* Translating an ast::VarRef node.
 *
 * NOTE:
 *   there are two kinds of variable reference: member variables or simple
 * variables
 */
void Translation::visit(ast::VarRef *ref) {
    Variable *var = ref->ATTR(sym);
    mind_assert(var != nullptr);
    bool isArrayRef = ref->isArrayRef();
    Temp offset = nullptr;

    if (isArrayRef) {
        ArrayType *arrayType = dynamic_cast<ArrayType *>(var->getType());
        assert(arrayType != nullptr);
        // Visit the index list in the reversed order
        bool isConst = true;
        int constOffset = 0;
        int multiplier = 1;

        auto dimIter = arrayType->getDimList()->rbegin();
        for (auto iter = ref->indexList->rbegin();
             iter != ref->indexList->rend(); iter++, dimIter++) {
            (*iter)->accept(this);
            if ((*iter)->getKind() != ast::ASTNode::INT_CONST || !isConst) {
                if (isConst) {
                    offset = tr->genLoadImm4(constOffset * 4);
                    isConst = false;
                }
                Temp added = tr->genMul((*iter)->ATTR(val),
                                        tr->genLoadImm4(multiplier * 4));
                offset = tr->genAdd(offset, added);
            } else {
                int val = dynamic_cast<ast::IntConst *>(*iter)->value;
                constOffset += val * multiplier;
            }

            multiplier *= *dimIter;
        }

        if (isConst) {
            offset = tr->genLoadImm4(constOffset * 4);
        }
    }

    if (ref->ATTR(sym)->isGlobalVar()) {
        Temp addr = tr->genLoadSym(ref->ATTR(sym)->getLabel());
        if (isArrayRef) {
            ref->ATTR(addr) = tr->genAdd(addr, offset);
        } else {
            ref->ATTR(addr) = addr;
        }
    } else {
        if (isArrayRef) {
            Temp base = ref->ATTR(sym)->getTemp();
            ref->ATTR(addr) = tr->genAdd(base, offset);
        } else {
            ref->ATTR(addr) = nullptr;
        }
    }
}

/* Translating an ast::VarDecl node.
 */
void Translation::visit(ast::VarDecl *decl) {
    Type *t = decl->ATTR(sym)->getType();
    ArrayType *at = nullptr;
    int arrLength = 1;

    if (t->isArrayType()) {
        at = dynamic_cast<ArrayType *>(t);
        mind_assert(at != NULL);
        arrLength = at->getLength();
    }

    if (decl->ATTR(sym)->isGlobalVar()) {
        // Create label for the global variable
        decl->ATTR(sym)->attachLabel(tr->getNewGlobVarLabel(decl->ATTR(sym)));

        int *defaultValues = nullptr;
        if (decl->init != nullptr) {
            // TODO: Array
            assert(decl->init->getKind() == ast::ASTNode::INT_CONST);
            ast::IntConst *intConst = dynamic_cast<ast::IntConst *>(decl->init);
            mind_assert(intConst != NULL);
            // Do not translate the node here!

            defaultValues = new int[1];
            defaultValues[0] = intConst->value;
        } else if (decl->init_list != nullptr) {
            mind_assert(t->isArrayType());
            defaultValues = new int[arrLength]();

            int i = 0;
            for (auto iter = decl->init_list->begin();
                 iter != decl->init_list->end(); iter++, i++) {
                assert((*iter)->getKind() == ast::ASTNode::INT_CONST);
                ast::IntConst *intConst = dynamic_cast<ast::IntConst *>(*iter);
                mind_assert(intConst != NULL);
                defaultValues[i] = intConst->value;
            }
        }

        tr->genDeclGlobVar(decl->ATTR(sym)->getLabel(), arrLength,
                           defaultValues);

    } else {
        if (at != nullptr) {
            Temp temp = tr->genAlloc(arrLength);
            decl->ATTR(sym)->attachTemp(temp);

            if (decl->init_list != nullptr) {
                int i = 0;
                for (auto iter = decl->init_list->begin();
                     iter != decl->init_list->end(); iter++, i++) {
                    (*iter)->accept(this);
                    tr->genStore(temp, i * 4, (*iter)->ATTR(val));
                }

                int remaining = arrLength - decl->init_list->length();
                if (remaining > 0) {
                    // Fill the rest with 0
                    Temp len = tr->genLoadImm4(remaining);
                    Temp startAddr = tr->genAdd(
                        temp, tr->genLoadImm4(decl->init_list->length() * 4));
                    tr->genParam(startAddr, 0);
                    tr->genParam(len, 1);
                    tr->genParam(tr->genLoadImm4(0), 2);
                    Label l = tr->getNewLabel();
                    l->target = true;
                    l->str_form = "fill_n";
                    tr->genCall(l);
                }
            }
        } else {
            decl->ATTR(sym)->attachTemp(tr->getNewTempI4());
            if (decl->init != NULL) {
                decl->init->accept(this);
                tr->genAssign(decl->ATTR(sym)->getTemp(),
                              decl->init->ATTR(val));
            }
        }
    }
}

void Translation::visit(ast::FuncCallExpr *e) {
    // We should visit all the arguments first, and then generate the PARAM tac.
    for (auto iter = e->args->begin(); iter != e->args->end(); iter++) {
        (*iter)->accept(this);
    }
    // Push the args in the reversed order.
    int total = e->args->length() - 1;
    auto iter = e->args->end();
    if (iter != e->args->begin()) {
        while (true) {
            iter--;
            tr->genParam((*iter)->ATTR(val), total);
            total--;
            if (iter == e->args->begin()) {
                break;
            }
        }
    }

    Temp res = tr->genCall(e->ATTR(sym)->getEntryLabel());
    e->ATTR(val) = res;
}

/* Translates an entire AST into a Piece list.
 *
 * PARAMETERS:
 *   tree  - the AST
 * RETURNS:
 *   the result Piece list (represented by the first node)
 */
Piece *MindCompiler::translate(ast::Program *tree) {
    TransHelper *helper = new TransHelper(md);

    tree->accept(new Translation(helper));

    return helper->getPiece();
}
