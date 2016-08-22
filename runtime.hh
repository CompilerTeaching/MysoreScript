#pragma once

#include <stdint.h>
#include <assert.h>
#include <string>
#include "gc.h"

static_assert(sizeof(void*) == 8,
	"MysoreScript only supports 64-bit platforms currently");

namespace {
/**
 * Typesafe helper function for allocating garbage-collected memory.  Allocates
 * enough memory for one instance of the specified type, plus the number of
 * extra bytes requested.
 */
template<typename T>
T* gcAlloc(size_t extraBytes=0)
{
	size_t size = sizeof(T) + extraBytes;
	return reinterpret_cast<T*>(GC_MALLOC(size));
}
}
namespace AST
{
	struct ClosureDecl;
}

namespace MysoreScript
{

struct Object;
struct Closure;

/**
 * Object pointer.  Objects are either pointers or small objects hidden inside
 * the pointer.
 */
typedef Object* Obj;
/**
 * Is this object a small integer (lowest bit is 1, next two bits are 0).
 */
inline bool isInteger(Obj o)
{
	return (reinterpret_cast<intptr_t>(o) & 7) == 1;
}
/**
 * Assuming that `o` is a small integer (an integer embedded in a pointer),
 * return it as a C integer.
 */
inline intptr_t getInteger(Obj o)
{
	assert(isInteger(o));
	return reinterpret_cast<intptr_t>(o) >> 3;
}
/**
 * Construct a small integer object from the given integer.
 */
inline Obj createSmallInteger(intptr_t i)
{
	// Small integers are 61-bits, assert that we won't overflow.
	assert((i<<3)>>3 == i);
	return reinterpret_cast<Obj>(((i << 3) | 1));
}

/**
 * Selectors are unique identifiers for methods.  When a method name is
 * registered, it is assigned a unique number.
 */
typedef uint32_t Selector;
/**
 * A compiled method is a function that takes an object (the receiver) and the
 * selector as implicit arguments and then any other explicit arguments.
 */
typedef Object *(*CompiledMethod)(Object*,Selector,...);
/**
 * A compiled closure invoke function.  This takes the closure as an implicit
 * argument and then other explicit arguments.
 */
typedef Object *(*ClosureInvoke)(Closure*,...);

/**
 * Methods in a class's method list
 */
struct Method
{
	/**
	 * The selector that this method applies to.
	 */
	Selector          selector;
	/**
	 * The number of arguments that this method takes.
	 */
	int32_t           args;
	/**
	 * The compiled method, if one exists.
	 */
	CompiledMethod    function;
	/**
	 * The AST for this method.
	 */
	AST::ClosureDecl *AST;
};

/**
 * Struct holding metadata about a class.  The first field of all instances of
 * a class will point to one of these structures.
 */
struct Class
{
	/**
	 * The superclass of this class, if it has one, or a null pointer if it is
	 * a root class.
	 */
	struct Class        *superclass;
	/**
	 * The name of this class.
	 */
	const char          *className;
	/**
	 * The number of methods that this class implements
	 */
	int32_t              methodCount;
	/**
	 * The number of indexed instance variables that this class has.
	 */
	int32_t             indexedIVarCount;
	/**
	 * An array of `methodCount` elements describing the methods that this
	 * class implements.
	 */
	struct Method       *methodList;
	/**
	 * The names of the instance variables.
	 */
	const char         **indexedIVarNames;
};

/**
 * A generic MysoreScript object.  The only thing that is assumed is the
 * presence of the class pointer.  Depending on the class, other fields will
 * follow.
 */
struct Object
{
	/**
	 * The pointer to the class of this object.  This object 'is a' {whatever
	 * the class is}.
	 */
	struct Class *isa;
};

/**
 * The layout of the primitive `Array` class in MysoreScript.
 */
struct Array
{
	/**
	 * Class pointer.  Always set to `&ArrayClass`.
	 */
	Class    *isa;
	/**
	 * Length of the array (number of elements in it).
	 */
	Obj       length;
	/**
	 * The size of the buffer.
	 */
	Obj       bufferSize;
	/**
	 * The buffer storing the values in this array.
	 */
	Obj      *buffer;
};

/**
 * The primitive `String` class in MysoreScript.
 */
struct String
{
	/**
	 * Class pointer.  Always set to `&StringClass`.
	 */
	Class    *isa;
	/**
	 * The number of characters in the string.
	 */
	Obj       length;
	/**
	 * An array of characters.  The actual length of this array is defined when
	 * the object is created.
	 */
	char      characters[0];
};

/**
 * The layout of all closures in MysoreScript.
 */
struct Closure
{
	/**
	 * Class pointer.  Always set to `&ClosureClass`
	 */
	Class             *isa;
	/**
	 * The number of parameters that this object has.
	 */
	Obj                parameters;
	/**
	 * The function that is used to invoke this closure.  This is either a
	 * compiled function or a trampoline calling back into the interpreter.
	 */
	ClosureInvoke      invoke;
	/**
	 * The AST for this closure.  Used if it is being interpreted.
	 */
	AST::ClosureDecl  *AST;
	/**
	 * An array of bound variables in this closure.
	 */
	Obj                boundVars[0];
};

/**
 * The class used for strings.
 */
extern struct Class StringClass;
/**
 * The class used for small integers.
 */
extern struct Class SmallIntClass;
/**
 * The class used for closures.
 */
extern struct Class ClosureClass;
/**
 * Register a newly constructed class.
 */
void registerClass(const std::string &name, struct Class *cls);
/**
 * Look up an existing class.
 */
struct Class* lookupClass(const std::string &name);



extern "C"
{
/**
 * Instantiate an object.  This returns a new instance of the class with all of
 * its instance variables set to null.
 */
Obj newObject(struct Class *cls);
/**
 * Helper function called by compiled code for the + operator on objects that
 * are not small (embedded in a pointer) integers.
 */
Obj mysoreScriptAdd(Obj lhs, Obj rhs);
/**
 * Helper function called by compiled code for the - operator on objects that
 * are not small (embedded in a pointer) integers.
 */
Obj mysoreScriptSub(Obj lhs, Obj rhs);
/**
 * Helper function called by compiled code for the * operator on objects that
 * are not small (embedded in a pointer) integers.
 */
Obj mysoreScriptMul(Obj lhs, Obj rhs);
/**
 * Helper function called by compiled code for the / operator on objects that
 * are not small (embedded in a pointer) integers.
 */
Obj mysoreScriptDiv(Obj lhs, Obj rhs);
/**
 * Look up the compiled method to call for a specific selector.  This is called
 * by compiled code to perform method lookups.  If a method has not yet been
 * compiled, the Method structure should be initialised with a trampoline
 * function that jumps back into the interpreter.
 */
CompiledMethod compiledMethodForSelector(Obj obj, Selector sel);
}

/**
 * Looks up the selector for a specified string value, registering a new value
 * if this is the first time the mapping has been needed..
 */
Selector lookupSelector(const std::string &);
/**
 * Looks up the `Method` that should be invoked for the specified selector on
 * this class.
 */
Method *methodForSelector(Class*, Selector);
/**
 * Calls a compiled method, constructing the correct argument frame based on
 * the arguments.
 */
Obj callCompiledMethod(CompiledMethod m, Obj receiver, Selector sel, Obj *args,
		int argCount);
/**
 * Calls a compiled closure from the specified argument list.
 */
Obj callCompiledClosure(ClosureInvoke m, Closure *receiver, Obj *args,
		int argCount);

}
