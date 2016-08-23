#pragma once
#include <string>
#include <unordered_map>
#include <forward_list>
#include <vector>
#include "runtime.hh"

namespace Interpreter
{
	/**
	 * Force the compiler to run.  This overrides the compile threshold and
	 * ensures that all methods and closures are compiled the first time that
	 * they are invoked.
	 */
	extern bool forceCompiler;
	using MysoreScript::Obj;
	/**
	 * Value wraps an object pointer.  It is responsible for informing the
	 * garbage collector that the object is referenced outside of the GC'd heap
	 * and so should not be deallocated.
	 *
	 * Note that the GC tracks the stack, so it is not necessary to use this
	 * class for object pointers stored purely on the stack.
	 */
	class Value
	{
		/**
		 * The Boehm GC places a relatively small limit on the number of root
		 * sets that it supports.  To avoid hitting this limit, we allocate an
		 * uncollectable buffer to hold this object.
		 */
		Obj *holder = nullptr;
		/**
		 * The object that this wraps.
		 */
		Obj object;
		/**
		 * Sets the current object that this class is wrapping.  This will
		 * toggle the `object` field's visibility to the GC depending on
		 * whether the object is a real pointer or a small object.
		 */
		void set(Obj o);
		public:
		/**
		 * Returns the address of the object.  This is used so that the storage
		 * can be used for values in a symbol table.  All globals are handled
		 * by creating an instance of this class and then adding the address to
		 * the symbol table.
		 */
		Obj *address()
		{
			return &object;
		}
		/**
		 * Default constructor, the value is null.
		 */
		Value() : object(nullptr) {}
		/**
		 * Construct a new value from an object pointer.
		 */
		Value(Obj o) : object(nullptr) { set(o); }
		/**
		 * Copy constructor.
		 */
		Value(Value &o) : object(nullptr) { set(static_cast<Obj>(o)); }
		/**
		 * Move constructor.
		 */
		Value(Value &&o) : object(nullptr)
		{
			set(static_cast<Obj>(o));
			o.set(nullptr);
		}
		/**
		 * Destructor, responsible for removing the reference from the GC's view.
		 */
		~Value();
		void operator=(Obj o) { set(o); }
		void operator=(Value &o) { set(static_cast<Obj>(o)); }
		operator Obj()
		{
			return object;
		}
		/**
		 * Returns true if this object is a small integer, false otherwise.
		 */
		bool isInteger()
		{
			return ((reinterpret_cast<intptr_t>(object)) & 7) == 1;
		}
		/**
		 * Returns the integer value of this object, if it is a small integer.
		 */
		int getIntValue()
		{
			assert(isInteger());
			return reinterpret_cast<intptr_t>(object) >> 3;
		}
	};
	/**
	 * A symbol table stores the address of each allocation.
	 */
	typedef std::unordered_map<std::string, Obj*> SymbolTable;
	class Context
	{
		/**
		 * The storage for globals.  This is a list so that allocation is fast
		 * (not very important) and the address of the globals is static (very
		 * important), allowing the symbol table to refer directly to them.
		 */
		std::forward_list<Value> globals;
		/**
		 * A stack of symbol tables.  When interpreting a closure, we push a
		 * new symbol table on top, and then pop it off at the end.
		 */
		std::vector<SymbolTable*> symbols;
		public:
		/**
		 * Global symbols.  These all refer to values in the `globals` list.
		 */
		SymbolTable globalSymbols;
		/**
		 * The value currently being returned, if there is one.  When a
		 * `return` statement is interpreted, it sets this value and returns.
		 * We then don't interpret any more statements until we come to the end
		 * of the current function and return the value stored here.
		 */
		Value retVal = nullptr;
		/**
		 * Are we currently returning?
		 */
		bool isReturning = false;
		/**
		 * Push a new symbol table on top of the stack.
		 */
		void pushSymbols(SymbolTable &s) { symbols.push_back(&s); }
		/**
		 * Pop the top symbol table off the stack.
		 */
		void popSymbols() { symbols.pop_back(); }
		/**
		 * Look up a symbol, walking up the symbol table stack until it's found.
		 */
		Obj *lookupSymbol(const std::string &name);
		/**
		 * Set the address where the variable corresponding to a particular
		 * symbol is stored.
		 */
		void setSymbol(const std::string &name, Obj *val);
		/**
		 * Set a symbol to the specified value.  This will allocate global
		 * storage for the symbol if it is not already allocated.
		 */
		void setSymbol(const std::string &name, Obj val);
	};
	/**
	 * Array of trampolines, indexed by number or arguments.  
	 */
	extern MysoreScript::ClosureInvoke closureTrampolines[];
};
