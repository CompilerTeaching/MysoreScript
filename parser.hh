#include "grammar.hh"
#include "ast.hh"

namespace Parser
{
	/**
	 * Class representing a parser for the MysoreScript language.  This
	 * inherits from the grammar and associates AST nodes with grammar rules.
	 */
	class MysoreScriptParser : public pegmatite::ASTParserDelegate
	{
#define CONNECT(cls, r) BindAST<AST::cls> r = MysoreScriptGrammar::get().r
		CONNECT(Number, num);
		CONNECT(Multiply, mul_op);
		CONNECT(Divide, div_op);
		CONNECT(Add, add_op);
		CONNECT(Subtract, sub_op);
		CONNECT(CmpNe, ne_cmp);
		CONNECT(CmpEq, eq_cmp);
		CONNECT(CmpLt, lt_cmp);
		CONNECT(CmpGt, gt_cmp);
		CONNECT(CmpLE, le_cmp);
		CONNECT(CmpGE, ge_cmp);
		CONNECT(StringLiteral, string_body);
		CONNECT(Identifier, identifier);
		CONNECT(ArgList, callArgList);
		CONNECT(ParamList, argList);
		CONNECT(ClosureDecl, closure);
		CONNECT(VarRef, variable);
		CONNECT(Assignment, assignment);
		CONNECT(Call, call);
		CONNECT(Decl, decl);
		CONNECT(Return, ret);
		CONNECT(IfStatement, ifStatement);
		CONNECT(ElseStatement, elseStatement);
		CONNECT(WhileLoop, whileLoop);
		CONNECT(Statements, statements);
		CONNECT(ClassDecl, cls);
		CONNECT(NewExpr, newExpr);
#undef CONNECT
		public:
		/**
		 * The grammar that this parser uses.
		 */
		const MysoreScriptGrammar &g = MysoreScriptGrammar::get();
	};
}
