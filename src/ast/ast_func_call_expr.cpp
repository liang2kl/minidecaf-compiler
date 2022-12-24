#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

FuncCallExpr::FuncCallExpr(std::string name, ExprList *args, Location *l) {
    setBasicInfo(CALL_EXPR, l);
    this->name = name;
    this->args = args;
}

void FuncCallExpr::accept(Visitor *v) { v->visit(this); }

void FuncCallExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << '"' << name << '"' << " " << args;
    decIndent(os);
}