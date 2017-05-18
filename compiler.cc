#include <functional>
#include "interpreter.hh"
#include "compiler.hh"
#include "ast.hh"
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>

using namespace llvm;
using llvm::legacy::PassManager;
using namespace MysoreScript;
using namespace AST;

namespace {
/**
 * Helper function that turns a pointer that is known at compile time into a
 * constant in the code.  Note that the garbage collector can't see the code (we
 * could tweak the memory manager to use the GC to allocate memory, but even
 * then constants are often materialised as immediate operands to two or more
 * instructions so the GC wouldn't necessarily see them).  This means that the
 * address must be a GC root or must be used to store pointers to non-GC'd
 * memory.
 */
template<typename T>
Value *staticAddress(Compiler::Context &c, T *ptr, Type *ty)
{
	return c.B.CreateIntToPtr(
		ConstantInt::get(c.ObjIntTy, reinterpret_cast<uintptr_t>(ptr)), ty);
}
/**
 * Generate a small integer object from an integer value.
 */
Value *compileSmallInt(Compiler::Context &c, Value *i)
{
	i = c.B.CreateShl(i, ConstantInt::get(c.ObjIntTy, 3));
	return c.B.CreateOr(i, ConstantInt::get(c.ObjIntTy, 1));
}
/**
 * Generate a small integer object from an integer constant.
 */
Value *compileSmallInt(Compiler::Context &c, intptr_t i)
{
	return ConstantInt::get(c.ObjIntTy, (i << 3) | 1);
}
/**
 * Get the specified value as the LLVM type used for object pointers.  This
 * inserts a bitcast or inttoptr instruction as appropriate for the source type.
 */
Value *getAsObject(Compiler::Context &c, Value *i)
{
	if (i->getType()->isPointerTy())
	{
		return c.B.CreateBitCast(i, c.ObjPtrTy);
	}
	return c.B.CreateIntToPtr(i, c.ObjPtrTy);
}
/**
 * Get the specified value as the LLVM type used for small integer objects.
 * This inserts a bitcast or ptrtoint instruction as appropriate for the source
 * type.
 */
Value *getAsSmallInt(Compiler::Context &c, Value *i)
{
	if (i->getType()->isPointerTy())
	{
		return c.B.CreatePtrToInt(i, c.ObjIntTy);
	}
	return c.B.CreateBitCast(i, c.ObjIntTy);
}
}

Compiler::Context::Context(Interpreter::SymbolTable &g) :
	globalSymbols(g),
	M(new Module("MysoreScript", C)),
	B(C),
	ObjPtrTy(Type::getInt8PtrTy(C)),
	ObjIntTy(Type::getInt64Ty(C)),
	SelTy(Type::getInt32Ty(C))
{
	// These functions do nothing, they just ensure that the correct modules
	// are not removed by the linker.
	LLVMInitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	LLVMLinkInMCJIT();

}

Value *Compiler::Context::lookupSymbolAddr(const std::string &str)
{
	// If the value is in the compiler's symbol table, then it's stored as an
	// LLVM `Value` representing the address of the variable, so just return it.
	if (Value *addr = symbols[str])
	{
		return addr;
	}
	// If it's in the global symbol table that we inherited from the interpreter
	// then it's a pointer, so construct an LLVM `Value` of the correct type and
	// value.
	if (Obj *global = globalSymbols[str])
	{
		return staticAddress(*this, global, ObjPtrTy->getPointerTo());
	}
	llvm_unreachable("Symbol not found");
}

ClosureInvoke Compiler::Context::compile()
{
	// Construct a couple of pass managers to run the optimisations.
	llvm::legacy::FunctionPassManager FPM(M.get());
	PassManager MPM;
	PassManagerBuilder Builder;
	Builder.OptLevel = 2;
	Builder.populateFunctionPassManager(FPM);
	Builder.populateModulePassManager(MPM);

	// If you want to see the LLVM IR before optimisation, uncomment the
	// following line:
	//M->dump();

	// Run the passes to optimise the function / module.
	FPM.run(*F);
	MPM.run(*M);

	// If you want to see the LLVM IR before optimisation, uncomment the
	// following line:
	//M->dump();

	std::string FunctionName = F->getName();
	std::string err;
	EngineBuilder EB(std::move(M));
	// Construct an execution engine (JIT)
	ExecutionEngine *EE = EB.setEngineKind(EngineKind::JIT)
		.setErrorStr(&err)
		.create();
	if (!EE)
	{
		fprintf(stderr, "Failed to construct Execution Engine: %s\n",
				err.c_str());
		return nullptr;
	}
	// Use the execution engine to compile the code.  Note that we leave the
	// ExecutionEngine live here because it owns the memory for the function.
	// It would be better to provide our own JIT memory manager to manage the
	// memory (and allow us to GC the functions if their addresses don't exist
	// on the stack and they're replaced by specialised versions, but for now
	// it's fine to just leak)
	return reinterpret_cast<ClosureInvoke>(EE->getFunctionAddress(FunctionName));
}

llvm::FunctionType *Compiler::Context::getMethodType(int ivars, int args)
{
	PointerType *ObjTy = ObjPtrTy;
	if (ivars)
	{
		// The fields in a class are the class pointer and the array of instance
		// variables
		Type* fields[2];
		// isa (actually a class pointer not an object pointer)
		fields[0] = ObjPtrTy;
		fields[1] = ArrayType::get(ObjPtrTy, ivars);
		ObjTy = StructType::create(fields)->getPointerTo();
	}
	// Set up the argument types for the closure invoke function.
	SmallVector<Type*, 10> paramTypes;
	// First two parameters are the implicit receiver and selector parameters.
	paramTypes.push_back(ObjTy);
	paramTypes.push_back(SelTy);
	// All parameters are objects.
	paramTypes.insert(paramTypes.end(), args, ObjPtrTy);
	return FunctionType::get(ObjPtrTy, paramTypes, false);
}

llvm::FunctionType *Compiler::Context::getClosureType(int bound, int args)
{
	PointerType *ClosureTy = ObjPtrTy;
	if (bound)
	{
		SmallVector<Type*, 6> fields;
		// isa (actually a class pointer not an object pointer)
		fields.push_back(ObjPtrTy);
		// Number of parameters
		fields.push_back(ObjPtrTy);
		// Invoke function.  We insert an explicit cast before we use this
		// field, so we don't need to worry about it.
		fields.push_back(ObjPtrTy);
		// AST pointer
		fields.push_back(ObjPtrTy);
		// The array of bound variables.
		fields.push_back(ArrayType::get(ObjPtrTy, bound));
		ClosureTy = StructType::create(fields)->getPointerTo();
	}
	// Set up the argument types for the closure invoke function.
	SmallVector<Type*, 10> paramTypes;
	paramTypes.push_back(ClosureTy);
	// All parameters are objects.
	paramTypes.insert(paramTypes.end(), args, ObjPtrTy);
	return FunctionType::get(ObjPtrTy, paramTypes, false);
}

CompiledMethod ClosureDecl::compileMethod(Class *cls,
                                          Interpreter::SymbolTable &globalSymbols)
{
	auto &params = parameters->arguments;
	Compiler::Context c(globalSymbols);
	// Get the type of the method as an LLVM type
	FunctionType *ClosureInvokeTy = c.getMethodType(cls->indexedIVarCount,
			params.size());
	// Create the LLVM function
	c.F = Function::Create(ClosureInvokeTy, GlobalValue::ExternalLinkage,
			"invoke", c.M.get());
	// Insert the first basic block and store all of the parameters.
	BasicBlock *entry = BasicBlock::Create(c.C, "entry", c.F);
	c.B.SetInsertPoint(entry);
	// We'll store the arguments into stack allocations so that they can be
	// written to.
	auto AI = c.F->arg_begin();
	// The first two arguments are the two implicit parameters, self and cmd
	auto selfPtr = c.B.CreateAlloca(c.ObjPtrTy);
	auto cmdPtr = c.B.CreateAlloca(c.ObjPtrTy);
	// Add these two stack locations to the symbol table.
	c.symbols["self"] = selfPtr;
	c.symbols["cmd"] = cmdPtr;
	// We're now going to create stack allocations for all of the parameters and
	// all of the locals.  We do this *before* we initialise any of them because
	// that makes life easier for the LLVM pass that generates SSA form, which
	// only guarantees to promote allocas that occur before any other
	// instructions to SSA registers.
	SmallVector<Value*, 10> paramAllocas;
	SmallVector<Value*, 10> localAllocas;

	// Create a stack allocation for each explicit parameter
	for (size_t i=0 ; i<params.size() ; i++)
	{
		paramAllocas.push_back(c.B.CreateAlloca(c.ObjPtrTy));
	}
	// Create a stack allocation for each local variable
	for (size_t i=0 ; i<decls.size() ; i++)
	{
		localAllocas.push_back(c.B.CreateAlloca(c.ObjPtrTy));
	}
	// Now we'll initialise the parameter allocations
	auto alloca = paramAllocas.begin();
	// First store the self pointer in its slot (which is already in the symbol
	// table)
	c.B.CreateStore(c.B.CreateBitCast(&*(AI++), c.ObjPtrTy), selfPtr);
	// The selector is a 32-bit integer, promote it to an object and store it.
	c.B.CreateStore(getAsObject(c, compileSmallInt(c, c.B.CreateZExt(&*(AI++),
						c.ObjIntTy))), cmdPtr);
	for (auto &arg : params)
	{
		// Set the name of the stack slot.  This isn't necessary, but makes
		// reading the generated IR a bit easier.
		(*alloca)->setName(*arg.get());
		// Store the argument in the stack slot.
		c.B.CreateStore(&*(AI++), *alloca);
		// Set this slot (specifically, the address of this stack allocation) to
		// the address used when looking up the name of the argument.
		c.symbols[*arg.get()] = *(alloca++);
	}
	alloca = localAllocas.begin();
	// Now do almost the same thing for locals
	for (auto &local : decls)
	{
		(*alloca)->setName(local);
		// Initialise all local variables with null.
		c.B.CreateStore(ConstantPointerNull::get(c.ObjPtrTy), *alloca);
		c.symbols[local] = *(alloca++);
	}
	// Next we need to add the instance variables to the symbol table.
	if (cls->indexedIVarCount)
	{
		// The type of the object pointer argument
		PointerType *ArgTy = cast<PointerType>(ClosureInvokeTy->params()[0]);
		// The type of the object 
		StructType *ObjTy =
			cast<StructType>(cast<PointerType>(ArgTy)->getElementType());
		// This does pointer arithmetic on the first argument to get the address
		// of the array of instance variables.
		Value *iVarsArray = c.B.CreateStructGEP(ObjTy, &*c.F->arg_begin(), 1);
		// The type of the instance variables array
		Type *iVarsArrayTy = ObjTy->elements()[1];
		// The type of the arguments array
		for (int i=0 ; i<cls->indexedIVarCount ; i++)
		{
			const char *name = cls->indexedIVarNames[i];
			// Now we do similar arithmetic on the array to get the address of
			// each instance variable.
			c.symbols[name] =
				c.B.CreateStructGEP(iVarsArrayTy, iVarsArray, i, name);
		}
	}
	// Compile the statements in the method.
	body->compile(c);
	// If we hit a return statement, it will clear the insert block, so if there
	// is still a valid insert block then the function ends with an unterminated
	// basic block.
	if (c.B.GetInsertBlock() != nullptr)
	{
		// Return null if there's no explicit return
		c.B.CreateRet(ConstantPointerNull::get(c.ObjPtrTy));
	}
	// Generate the compiled code.
	return reinterpret_cast<CompiledMethod>(c.compile());
}

ClosureInvoke ClosureDecl::compileClosure(Interpreter::SymbolTable &globalSymbols)
{
	auto &params = parameters->arguments;
	Compiler::Context c(globalSymbols);
	// Get the LLVM type of the closure invoke function
	FunctionType *ClosureInvokeTy = c.getClosureType(boundVars.size(),
			params.size());
	// Create the LLVM function
	c.F = Function::Create(ClosureInvokeTy, GlobalValue::ExternalLinkage, "invoke", c.M.get());
	// Insert the first basic block and store all of the parameters.
	BasicBlock *entry = BasicBlock::Create(c.C, "entry", c.F);
	c.B.SetInsertPoint(entry);
	auto AI = c.F->arg_begin();
	auto selfPtr = c.B.CreateAlloca(c.ObjPtrTy);
	c.symbols["self"] = selfPtr;
	SmallVector<Value*, 10> paramAllocas;
	SmallVector<Value*, 10> localAllocas;

	for (size_t i=0 ; i<params.size() ; i++)
	{
		paramAllocas.push_back(c.B.CreateAlloca(c.ObjPtrTy));
	}
	for (size_t i=0 ; i<decls.size() ; i++)
	{
		localAllocas.push_back(c.B.CreateAlloca(c.ObjPtrTy));
	}
	auto alloca = paramAllocas.begin();

	c.B.CreateStore(c.B.CreateBitCast(&*(AI++), c.ObjPtrTy), selfPtr);
	for (auto &arg : params)
	{
		(*alloca)->setName(*arg.get());
		c.B.CreateStore(&*(AI++), *alloca);
		c.symbols[*arg.get()] = *(alloca++);
	}
	alloca = localAllocas.begin();
	for (auto &local : decls)
	{
		(*alloca)->setName(local);
		c.B.CreateStore(ConstantPointerNull::get(c.ObjPtrTy), *alloca);
		c.symbols[local] = *(alloca++);
	}
	// If this closure refers to external variables then they'll have been
	// copied into the closure structure.  Add those to the symbol table in the
	// same way that we added instance variables for methods.
	if (!boundVars.empty())
	{
		// The type of the closure pointer argument
		PointerType *ArgTy = cast<PointerType>(ClosureInvokeTy->params()[0]);
		// The type of the closure object
		StructType *ObjTy =
			cast<StructType>(cast<PointerType>(ArgTy)->getElementType());
		// The type of the instance variables array
		Type *boundVarsArrayTy = ObjTy->elements()[4];

		Value *boundVarsArray = c.B.CreateStructGEP(ObjTy, &*c.F->arg_begin(), 4);
		int i=0;
		for (auto &bound : boundVars)
		{
			c.symbols[bound] = c.B.CreateStructGEP(boundVarsArrayTy, boundVarsArray, i++, bound);
		}
	}
	body->compile(c);
	if (c.B.GetInsertBlock() != nullptr)
	{
		c.B.CreateRet(ConstantPointerNull::get(c.ObjPtrTy));
	}
	return c.compile();
}

Value *ClosureDecl::compileExpression(Compiler::Context &c)
{
	// Make sure that we know what the bound variables are.
	check();
	auto &params = parameters->arguments;
	// Get the type of the invoke function
	FunctionType *invokeTy = c.getClosureType(boundVars.size(), params.size());
	// Get the type of the first parameter (a pointer to the closure structure)
	PointerType *closurePtrTy = cast<PointerType>(invokeTy->getParamType(0));
	// The type of the closure.
	StructType *closureTy = cast<StructType>(closurePtrTy->getElementType());
	// The size of the closure is the size of `Closure`, which contains all of
	// the fields shared by all closures, and then space for all of the bound
	// variables.
	size_t closureSize = sizeof(struct Closure) + boundVars.size() * sizeof(Obj);
	// Insert the `GC_malloc` function (provided by libgc) into the module,
	// bitcast to return a pointer to our closure type.
	Constant *allocFn = c.M->getOrInsertFunction("GC_malloc", closurePtrTy,
			c.ObjIntTy, nullptr);
	// Allocate GC'd memory for the closure.  Note that it would often be more
	// efficient to do this on the stack, but only if we can either statically
	// prove that the closure is not captured by anything that is called or if
	// we can promote it to the heap if it is.
	Value *closure = c.B.CreateCall(allocFn, ConstantInt::get(c.ObjIntTy,
				closureSize));
	// Set the isa pointer to the closure class.
	c.B.CreateStore(staticAddress(c, &ClosureClass, c.ObjPtrTy),
			c.B.CreateStructGEP(closureTy, closure, 0));
	// Set the parameters pointer to the number of parameters.
	c.B.CreateStore(getAsObject(c, compileSmallInt(c, params.size())),
			c.B.CreateStructGEP(closureTy, closure, 1));
	// If we've already compiled the function for this closure, then insert a
	// pointer to it into the closure, otherwise use the trampoline that calls
	// back into the interpreter.
	// Note: This means that if we later compile this closure we should
	// recompile the enclosing function or the invocation of the closure will be
	// expensive.
	ClosureInvoke closureFn = compiledClosure ? compiledClosure :
		Interpreter::closureTrampolines[params.size()];
	c.B.CreateStore(staticAddress(c, closureFn, c.ObjPtrTy),
		c.B.CreateStructGEP(closureTy, closure, 2));
	// Set the AST pointer
	c.B.CreateStore(staticAddress(c, this, c.ObjPtrTy),
		c.B.CreateStructGEP(closureTy, closure, 3));
	// Get a pointer to the array of bound variables
	Value *boundVarsArray = c.B.CreateStructGEP(closureTy, closure, 4);
	Type *boundVarsArrayTy = closureTy->elements()[4];
	int i=0;
	for (auto &var : boundVars)
	{
		// Load each bound variable and then insert it into the closure at the
		// correct index.
		c.B.CreateStore(c.B.CreateLoad(c.lookupSymbolAddr(var)),
			c.B.CreateStructGEP(boundVarsArrayTy, boundVarsArray, i++, var));
	}
	// Add this closure to our symbol table.
	c.B.CreateStore(c.B.CreateBitCast(closure, c.ObjPtrTy), c.symbols[name]);
	return closure;
}
Value *Call::compileExpression(Compiler::Context &c)
{
	SmallVector<Value*, 10> args;
	// Compile the expression that evaluates to the object being called.
	Value *obj = getAsObject(c, callee->compileExpression(c));
	assert(obj);
	// For both closure and method invocations, the object being invoked is the
	// first argument
	args.push_back(obj);
	// If this is a method invocation, then the next argument is the selector.
	if (method)
	{
		Selector sel = lookupSelector(*method.get());
		args.push_back(ConstantInt::get(c.SelTy, sel));
	}
	// Now add each of the explicit arguments.
	auto &argsAST = arguments->arguments;
	for (auto &arg : argsAST)
	{
		args.push_back(getAsObject(c, arg->compileExpression(c)));
	}
	// If there's no method, then we're trying to invoke a closure.
	if (!method)
	{
		// Get the closure invoke type. 
		FunctionType *invokeFnTy = c.getClosureType(0, args.size() - 1);
		// The only field we care about in the closure is the invoke pointer,
		// so define enough of the structure to get that (two pointers before
		// it)
		Type *Fields[3] =
			{ c.ObjPtrTy, c.ObjPtrTy, invokeFnTy->getPointerTo() };
		// Get the type of a pointer to the closure object 
		Type *closureTy = StructType::create(Fields);
		Type *closurePtrTy = closureTy->getPointerTo();
		// Cast the called object to a closure
		Value *closure = c.B.CreateBitCast(obj, closurePtrTy);
		// Compute the address of the pointer to the closure invoke function
		Value *invokeFn = c.B.CreateStructGEP(closureTy, closure, 2);
		// Load the address of the invoke function
		invokeFn = c.B.CreateLoad(invokeFn);
		// Insert the call
		return c.B.CreateCall(invokeFn, args, "call_closure");
	}
	// If we are invoking a method, then we must first look up the method, then
	// call it.
	FunctionType *methodType = c.getMethodType(0, args.size() - 2);
	// Get the lookup function
	Constant *lookupFn = c.M->getOrInsertFunction("compiledMethodForSelector",
			methodType->getPointerTo(), obj->getType(), c.SelTy, nullptr);
	// Insert the call to the function that performs the lookup.  This will
	// always return *something* that we can call, even if it's just a function
	// that reports an error.
	Value *methodFn = c.B.CreateCall(lookupFn, {obj, args[1]});
	// Call the method
	return c.B.CreateCall(methodFn, args, "call_method");
}

void Statements::compile(Compiler::Context &c)
{
	for (auto &s : statements)
	{
		// If we've hit a return statement then any subsequent statements in
		// this block are dead so just return.
		if (c.B.GetInsertBlock() == nullptr)
		{
			return;
		}
		s->compile(c);
	}
}



void Return::compile(Compiler::Context &c)
{
	// Insert a return instruction with the correct value
	Value *ret = expr->compileExpression(c);
	c.B.CreateRet(getAsObject(c, ret));
	// Clear the insert point so nothing else tries to insert instructions after
	// the basic block terminator
	c.B.ClearInsertionPoint();
}

void IfStatement::compile(Compiler::Context &c)
{
	// Compute the condition
	Value *cond = condition->compileExpression(c);
	// Create the basic block that we'll branch to if the condition is false and
	// after executing if the condition is true.
	BasicBlock *cont = BasicBlock::Create(c.C, "if.cont", c.F);
	// Create the block that contains the body of the if statement
	BasicBlock *ifBody = BasicBlock::Create(c.C, "if.body", c.F);
	// Cast the condition to a small int.  We aren't unconditionally
	// interpreting it as a small int, we just want to be able to do some
	// arithmetic on it...
	cond = getAsSmallInt(c, cond);
	// Now right shift it by 3.  If it is an object pointer, it will now be 0 if
	// it were a null pointer before the shift.  If it is a small integer, then
	// it will now contain its numeric value.  Either way, a value of 0 means
	// either a zero integer or null, and a value of anything else means a valid
	// object or non-zero integer.
	cond = c.B.CreateLShr(cond, ConstantInt::get(c.ObjIntTy, 3));
	// Create a comparison with 0 that we can then branch on
	cond = c.B.CreateIsNotNull(cond);
	// Branch to the body if it's not 0, to the continuation block if it is
	c.B.CreateCondBr(cond, ifBody, cont);
	// Compile the body of the if statement.
	c.B.SetInsertPoint(ifBody);
	body->compile(c);
	// If the if statement didn't end with a return, then return control flow to
	// the block after the if statement.
	if (c.B.GetInsertBlock() != nullptr)
	{
		c.B.CreateBr(cont);
	}
	// Continue from the end
	c.B.SetInsertPoint(cont);
}
void WhileLoop::compile(Compiler::Context &c)
{
	// Create three blocks, one for the body of the test (which we will
	// unconditionally branch back to at the end of the body), one for the loop
	// body (which we will skip if the condition is false) and one for the end,
	// which we will branch to after the loop has finished.
	BasicBlock *condBlock = BasicBlock::Create(c.C, "while.cond", c.F);
	BasicBlock *cont = BasicBlock::Create(c.C, "while.cont", c.F);
	BasicBlock *whileBody = BasicBlock::Create(c.C, "while.body", c.F);
	// Unconditionally branch to the block for the condition and compile it
	c.B.CreateBr(condBlock);
	c.B.SetInsertPoint(condBlock);
	// Compile the condition expression
	Value *cond = condition->compileExpression(c);
	// Convert it to an integer and test that it isn't null
	cond = getAsSmallInt(c, cond);
	cond = c.B.CreateLShr(cond, ConstantInt::get(c.ObjIntTy, 3));
	cond = c.B.CreateIsNotNull(cond);
	// Branch into the loop body or past it, depending on the condition value.
	c.B.CreateCondBr(cond, whileBody, cont);
	c.B.SetInsertPoint(whileBody);
	// Compile the body of the loop.
	body->compile(c);
	// Unconditionally branch back to the condition to check it again.
	c.B.CreateBr(condBlock);
	// Set the insert point to the block after the loop.
	c.B.SetInsertPoint(cont);
}

Value *AST::StringLiteral::compileExpression(Compiler::Context &c)
{
	// If we don't have a cached string object for this literal, then poke the
	// interpreter to generate one.
	if (!static_cast<Obj>(cache))
	{
		Interpreter::Context ic;
		cache = evaluateExpr(ic);
	}
	// Return a constant pointer to the cached value.
	return staticAddress(c, static_cast<Obj>(cache), c.ObjPtrTy);
}
Value *Number::compileExpression(Compiler::Context &c)
{
	// Construct a constant small integer value
	return compileSmallInt(c, value);
}

void Decl::compile(Compiler::Context &c)
{
	// Decls are collected and stack space allocated for them when we compile a
	// closure, but we still want to initialise them at their first use.
	if (init)
	{
		assert(c.symbols[name]);
		c.B.CreateStore(getAsObject(c, init->compileExpression(c)),
				c.symbols[name]);
	}
}
void Assignment::compile(Compiler::Context &c)
{
	assert(c.symbols[target->name]);
	// Store the result of the expression in the address of the named variable.
	c.B.CreateStore(getAsObject(c, expr->compileExpression(c)),
			c.symbols[target->name]);
}

Value *VarRef::compileExpression(Compiler::Context &c)
{
	return c.B.CreateLoad(c.lookupSymbolAddr(name));
}
Value *NewExpr::compileExpression(Compiler::Context &c)
{
	// Look up the class statically
	Class *cls = lookupClass(className);
	// Create a value corresponding to the class pointer
	Value *clsPtr = staticAddress(c, cls, c.ObjPtrTy);
	// Look up the function that creates instances of objects
	Constant *newFn = c.M->getOrInsertFunction("newObject", c.ObjPtrTy,
			c.ObjPtrTy, nullptr);
	// Call the function with the class pointer as the argument
	return c.B.CreateCall(newFn, clsPtr, "new");
}

namespace {
/**
 * A function type that is used by the `compileBinaryOp` function.  Each
 * `compileBinOp` method for arithmetic operations provides an instance of this
 * type to insert the correct instruction for performing its operation on the
 * two arguments.
 */
typedef std::function<Value*(Compiler::Context &c, Value*, Value*)> BinOpFn;

/**
 * Helper function that inserts all of the code required for comparison
 * operations.
 */
Value *compileCompare(Compiler::Context &c, CmpInst::Predicate Op,
                    Value *LHS, Value *RHS)
{
	// Comparisons work on small integers or pointers, so just insert the
	// relevant compare instruction.
	Value *cmp = c.B.CreateICmp(Op, getAsSmallInt(c, LHS),
	                             getAsSmallInt(c, RHS), "cmp");
	// The result is an i1 (one-bit integer), so zero-extend it to the size of a
	// small integer
	cmp = c.B.CreateZExt(cmp, c.ObjIntTy, "cmp_object");
	// Then set the low bit to make it an object.
	return compileSmallInt(c, cmp);

}

/**
 * Helper function that inserts all of the code required for small integer
 * operations, either calling the relevant method or doing the arithmetic.  For
 * real objects, the function named by the `slowCallFnName` parameter is called,
 * which then invokes the correct method.  For integers, the function inserts
 * the binary op specified by `Op`.
 */
Value *compileBinaryOp(Compiler::Context &c, Value *LHS, Value *RHS,
		Instruction::BinaryOps Op, const char *slowCallFnName)
{
	// Get the two operands as integer values
	Value *LHSInt = getAsSmallInt(c, LHS);
	Value *RHSInt = getAsSmallInt(c, RHS);
	// And them together.  If they are both small ints, then the low bits of the
	// result will be 001.
	Value *isSmallInt = c.B.CreateAnd(LHSInt, RHSInt);
	// Now mask off the low 3 bits.
	isSmallInt = c.B.CreateAnd(isSmallInt, ConstantInt::get(c.ObjIntTy, 7));
	// If the low three bits are 001, then it is a small integer
	isSmallInt = c.B.CreateICmpEQ(isSmallInt, ConstantInt::get(c.ObjIntTy, 1));
	// Create three basic blocks, one for the small int case, one for the real
	// object case, and one for when the two join together again.
	BasicBlock *cont = BasicBlock::Create(c.C, "cont", c.F);
	BasicBlock *small = BasicBlock::Create(c.C, "int", c.F);
	BasicBlock *obj = BasicBlock::Create(c.C, "obj", c.F);
	// If both arguments are small integers, jump to the small int block,
	// otherwise fall back to the other case.
	c.B.CreateCondBr(isSmallInt, small, obj);

	// Now emit the small int code:
	c.B.SetInsertPoint(small);
	// Shift both values right by 3 to give primitive integer values
	LHSInt = c.B.CreateAShr(LHSInt, ConstantInt::get(c.ObjIntTy, 3));
	RHSInt = c.B.CreateAShr(RHSInt, ConstantInt::get(c.ObjIntTy, 3));
	// Invoke the function passed by the caller to insert the correct operation.
	Value *intResult  = c.B.CreateBinOp(Op, LHSInt, RHSInt);
	// Now cast the result to an object and branch to the continue block
	intResult = getAsObject(c, compileSmallInt(c, intResult));
	c.B.CreateBr(cont);

	// Next we'll handle the real object case.
	c.B.SetInsertPoint(obj);
	// Call the function that handles the object case
	Value *objResult = c.B.CreateCall(c.M->getOrInsertFunction(slowCallFnName,
				c.ObjPtrTy, LHS->getType(), RHS->getType(), nullptr),
			{LHS, RHS});
	// And branch to the continuation block
	c.B.CreateBr(cont);

	// Now that we've handled both cases, we need to unify the flow control and
	// provide a single value
	c.B.SetInsertPoint(cont);
	// Construct a PHI node to hold the result.  
	PHINode *result = c.B.CreatePHI(intResult->getType(), 2, "sub");
	// Set its value to the result of whichever basic block we arrived from
	result->addIncoming(intResult, small);
	result->addIncoming(objResult, obj);
	// Return the result
	return result;
}
} // End anonymous namespace

Value *CmpNe::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_NE, LHS, RHS);
}
Value *CmpEq::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_EQ, LHS, RHS);
}
Value *CmpGt::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_SGT, LHS, RHS);
}
Value *CmpLt::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_SLT, LHS, RHS);
}
Value *CmpGE::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_SGE, LHS, RHS);
}
Value *CmpLE::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileCompare(c, CmpInst::Predicate::ICMP_SLE, LHS, RHS);
}
Value *Subtract::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileBinaryOp(c, LHS, RHS, Instruction::Sub, "mysoreScriptAdd");
}
Value *Add::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileBinaryOp(c, LHS, RHS, Instruction::Add, "mysoreScriptSub");
}
Value *Multiply::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileBinaryOp(c, LHS, RHS, Instruction::Mul, "mysoreScriptMul");
}
Value *Divide::compileBinOp(Compiler::Context &c, Value *LHS, Value *RHS)
{
	return compileBinaryOp(c, LHS, RHS, Instruction::SDiv, "mysoreScriptDiv");
}
