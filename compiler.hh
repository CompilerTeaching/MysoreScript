#pragma once
#include "runtime.hh"
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/LLVMContext.h>
#include <unordered_map>

namespace Compiler 
{
	using MysoreScript::Obj;
	/**
	 * The compiler context.  Contains everything that AST nodes need to be
	 * able to compile themselves.
	 */
	class Context
	{
		/**
		 * The global symbols that can be referenced when compiling.
		 */
		Interpreter::SymbolTable                     &globalSymbols;
		public:
		/**
		 * The LLVM context.  The current implementation uses the global
		 * context, but it would be possible to extend MysoreScript to compile
		 * different functions in the background in different threads by
		 * changing this.
		 */
		llvm::LLVMContext  C;
		/**
		 * The current module.  Each function is compiled in a separate module.
		 */
		std::unique_ptr<llvm::Module>      M;
		/**
		 * The function being compiled.
		 */
		llvm::Function     *F;
		/**
		 * The IR builder, always set to the current insert point.
		 */
		llvm::IRBuilder<>   B;
		/**
		 * Symbols within this compilation context.  This includes bound
		 * variables.  The `Value`s in this map are pointers to the storage for
		 * the variables.
		 */
		std::unordered_map<std::string, llvm::Value*> symbols;
		/**
		 * The type of a pointer to a MysoreScript object.
		 */
		llvm::PointerType  *ObjPtrTy;
		/**
		 * The type of an integer the same size as an object pointer.
		 */
		llvm::Type         *ObjIntTy;
		/**
		 * The type used for selectors.
		 */
		llvm::Type         *SelTy;
		/**
		 * Get the type of a closure invoke function, for a closure with the
		 * specified number of instance variables and arguments.
		 */
		llvm::FunctionType *getClosureType(int bound, int args);
		/**
		 * Get the type of a method with the specified number of arguments, for
		 * an object with the specified number of instance variables.
		 */
		llvm::FunctionType *getMethodType(int ivars, int args);
		/**
		 * Returns the address of the specified symbol.
		 */
		llvm::Value        *lookupSymbolAddr(const std::string &str);
		/**
		 * Construct a context, given a global symbol table from the
		 * interpreter.  Compilation is always triggered from the interpreter.
		 */
		Context(Interpreter::SymbolTable &g);
		/**
		 * At the end of compilation, generate code and return a function pointer.
		 */
		MysoreScript::ClosureInvoke compile();
	};
};
