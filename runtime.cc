#include "runtime.hh"
#include <stdio.h>
#include <string.h>
#include <unordered_map>
#include <vector>
#include <gc.h>

using namespace MysoreScript;

namespace
{

/**
 * Global vector of selector names.  This is used to map from a selector to a
 * string value.
 */
std::vector<std::string> selNames;

/**
 * Invalid method function.  Returned when method lookup fails.  This logs a
 * message indicating the error.
 */
Obj invalidMethod(Obj obj, Selector sel)
{
	auto selName = selNames[sel];
	if (!obj)
	{
		fprintf(stderr, "\nERROR: method %s called on null object\n",
				selName.c_str());
		return nullptr;
	}
	Class *cls = isInteger(obj) ? &SmallIntClass : obj->isa;
	fprintf(stderr, "\nERROR: %s does not respond to selector %s\n",
			cls->className, selName.c_str());
	return nullptr;
}

/**
 * The `.length()` method for `String` objects.
 */
Obj StringLength(String *str, Selector sel)
{
	return str->length;
}

/**
 * The `.charAt(idx)` method for `String` objects.
 */
Obj StringCharAt(String *str, Selector sel, Obj idx)
{
	// If the index isn't a small integer, then return 0.
	if (!isInteger(idx))
	{
		return nullptr;
	}
	intptr_t i = getInteger(idx);
	intptr_t len = getInteger(str->length);
	// Check that the access is in bounds.
	if (i >= len && (i > 0))
	{
		return nullptr;
	}
	// Turn the small integer object into a primitive integer and use it to
	// dereference the character array, then turn the result into an integer
	// value.
	return createSmallInteger(str->characters[i]);
}
/**
 * The `.length()` method for `Array` objects.
 */
Obj ArrayLength(Array *arr, Selector sel)
{
	return arr->length;
}
/**
 * The `.at(idx)` method for `Array` objects.
 */
Obj ArrayAt(Array *arr, Selector sel, Obj idx)
{
	if (!isInteger(idx))
	{
		return nullptr;
	}
	intptr_t i = getInteger(idx);
	intptr_t len = arr->length ? getInteger(arr->length) : 0;
	// Check that the access is in bounds.
	if (i >= len && (i > 0))
	{
		return nullptr;
	}
	return arr->buffer[i];
}
/**
 * The `.atPut(idx, obj)` method for `Array` objects.
 */
Obj ArrayAtPut(Array *arr, Selector sel, Obj idx, Obj obj)
{
	// If the index isn't an integer, return null
	if (!isInteger(idx))
	{
		return nullptr;
	}
	intptr_t i = getInteger(idx);
	intptr_t len = arr->length ? getInteger(arr->length) : 0;
	// Check that the index is positive.
	if (i < 0)
	{
		return nullptr;
	}
	intptr_t bufferSize = arr->bufferSize ? getInteger(arr->bufferSize) : 0;
	if (i >= bufferSize)
	{
		// Note that this is really inefficient, but will stress the GC a
		// little bit, which should make life a bit more interesting for
		// students...
		size_t newSize = i + 1;
		Obj *buffer = (Obj*)GC_MALLOC(newSize * sizeof(Obj));
		memcpy(buffer, arr->buffer, len * sizeof(Obj));
		arr->buffer = buffer;
		arr->bufferSize = createSmallInteger(newSize);
	}
	if (i >= len)
	{
		arr->length = createSmallInteger(i+1);
	}
	arr->buffer[i] = obj;
	return obj;
}

/**
 * The `.dump()` method for `Number` objects.
 */
Obj NumberDump(Obj str, Selector sel)
{
	fprintf(stderr, "%lld\n", (long long)getInteger(str));
	return nullptr;
}
/**
 * The `.dump()` method for `String` objects.
 */
Obj StringDump(String *str, Selector sel)
{
	fwrite(str->characters, getInteger(str->length), 1, stderr);
	return nullptr;
}

/**
 * Selectors for methods that are defined as part of the runtime.
 */
enum StaticSelectors
{
	length = 1,
	charAt,
	dump,
	invoke,
	atPut,
	at,
	add,
	sub,
	mul,
	div,
	LAST_STATIC_SELECTOR
};

/**
 * The names of the selectors in the `StaticSelectors` enumeration.
 */
const char *StaticSelectorNames[] =
{
	"length",
	"charAt",
	"dump",
	"invoke",
	"atPut",
	"at",
	"add",
	"sub",
	"mul",
	"div"
};
static_assert(sizeof(StaticSelectorNames) / sizeof(char*) ==
		LAST_STATIC_SELECTOR-1, "Static selector names and enum out of sync");

/**
 * Method table for the `String` class.
 */
struct Method StringMethods[] = 
{
	{
		length,
		0,
		(CompiledMethod)StringLength,
		nullptr
	},
	{
		charAt,
		1,
		(CompiledMethod)StringCharAt,
		nullptr
	},
	{
		dump,
		0,
		(CompiledMethod)StringDump,
		nullptr
	}
};
/**
 * Method table for the `Number` class.
 */
struct Method NumberMethods[] = 
{
	{
		dump,
		0,
		(CompiledMethod)NumberDump,
		nullptr
	}
};
/**
 * Method table for the `Array` class.
 */
struct Method ArrayMethods[] = 
{
	{
		length,
		0,
		(CompiledMethod)ArrayLength,
		nullptr
	},
	{
		at,
		1,
		(CompiledMethod)ArrayAt,
		nullptr
	},
	{
		atPut,
		2,
		(CompiledMethod)ArrayAtPut,
		nullptr
	}
};
/**
 * The names of the instance variables in the `String` class.
 */
const char *StringIvars[] = { "length" };
/**
 * The names of the instance variables in the `Array` class.
 */
const char *ArrayIvars[] = { "length", "bufferSize", "buffer" };

}

namespace MysoreScript
{
/**
 * The `String` class structure.
 */
struct Class StringClass =
{
	NULL,
	"String",
	sizeof(StringMethods) / sizeof(Method),
	sizeof(StringIvars) / sizeof(char*),
	StringMethods,
	StringIvars
};
/**
 * The `Array` class structure.
 */
struct Class ArrayClass =
{
	NULL,
	"Array",
	sizeof(ArrayMethods) / sizeof(Method),
	sizeof(ArrayIvars) / sizeof(char*),
	ArrayMethods,
	ArrayIvars
};
/**
 * The `SmallInt` (`Number`) class structure.  This is distinct from Number
 * because a future implementation may implement `Number` as both `SmallInt` and
 * `BigInt` classes internally, which both appear as instances of the `Number`
 * class.
 */
struct Class SmallIntClass =
{
	NULL,
	"Number",
	sizeof(NumberMethods) / sizeof(Method),
	0,
	NumberMethods,
	nullptr
};
/**
 * The `Closure` class structure.
 */
struct Class ClosureClass =
{
	NULL,
	"Closure",
	0,
	0,
	nullptr,
	nullptr
};

Selector lookupSelector(const std::string &str)
{
	static std::unordered_map<std::string, Selector> selectors;
	// If we don't have any selectors in this array yet then register all of the
	// static ones.
	if (selectors.empty())
	{
		selNames.push_back("<invalid>");
		for (int i=1 ; i<LAST_STATIC_SELECTOR ; i++)
		{
			selectors[std::string(StaticSelectorNames[i-1])] = i;
			selNames.push_back(StaticSelectorNames[i-1]);
		}
	}
	// Look up the selector
	size_t next = selectors.size();
	Selector &sel = selectors[str];
	// If it doesn't exist, register it
	if (sel == 0)
	{
		sel = next;
		selNames.push_back(str);
	}
	return sel;
}

/**
 * The class table, mapping class names to class structures.  Note that
 * `Closure` and `Number` are not in this, as they are not intended to be
 * referred to by name.
 */
static std::unordered_map<std::string, struct Class*> classTable;

void registerClass(const std::string &name, struct Class *cls)
{
	if (classTable.empty())
	{
		classTable["String"] = &StringClass;
		classTable["Array"] = &ArrayClass;
	}
	classTable[name] = cls;
}
struct Class* lookupClass(const std::string &name)
{
	if (classTable.empty())
	{
		classTable["String"] = &StringClass;
		classTable["Array"] = &ArrayClass;
	}
	return classTable[name];
}
Obj newObject(struct Class *cls)
{
	// Allocate space for the object
	Obj obj = (Obj)GC_MALLOC(sizeof(Obj)*(cls->indexedIVarCount + 1));
	// Set its class pointer
	obj->isa = cls;
	return obj;
}

Method *methodForSelector(Class *cls, Selector sel)
{
	// Perform a very simple linear search (O(n) in the number of methods in the
	// class hierarchy) to find the relevant method.
	for (; cls ; cls = cls->superclass)
	{
		for (intptr_t i=0 ; i<cls->methodCount; i++)
		{
			if (cls->methodList[i].selector == sel)
			{
				return &cls->methodList[i];
			}
		}
	}
	return nullptr;
}

Obj callCompiledMethod(CompiledMethod m, Obj receiver, Selector sel, Obj *args, 
		int argCount)
{
	switch (argCount)
	{
		default:
			assert(0 && "Too many arguments!");
			return nullptr;
		case 0:
			return ((Obj(*)(Obj, Selector))m)(receiver, sel);
		case 1:
			return ((Obj(*)(Obj, Selector, Obj))m)(receiver, sel, args[0]);
		case 2:
			return ((Obj(*)(Obj, Selector, Obj, Obj))m)(receiver, sel, args[0],
					args[1]);
		case 3:
			return ((Obj(*)(Obj, Selector, Obj, Obj, Obj))m)(receiver, sel,
					args[0], args[1], args[2]);
		case 4:
			return ((Obj(*)(Obj, Selector, Obj, Obj, Obj, Obj))m)(receiver,
					sel, args[0], args[1], args[2], args[3]);
	}
}

Obj callCompiledClosure(ClosureInvoke m, Closure *receiver, Obj *args, 
		int argCount)
{
	switch (argCount)
	{
		default:
			assert(0 && "Too many arguments!");
			return nullptr;
		case 0:
			return ((Obj(*)(Closure*))m)(receiver);
		case 1:
			return ((Obj(*)(Closure*, Obj))m)(receiver, args[0]);
		case 2:
			return ((Obj(*)(Closure*, Obj, Obj))m)(receiver, args[0], args[1]);
		case 3:
			return ((Obj(*)(Closure*, Obj, Obj, Obj))m)(receiver, args[0], args[1],
					args[2]);
		case 4:
			return ((Obj(*)(Closure*, Obj, Obj, Obj, Obj))m)(receiver, args[0],
					args[1], args[2], args[3]);
	}
}

extern "C"
{
Obj mysoreScriptAdd(Obj lhs, Obj rhs)
{
	return compiledMethodForSelector(lhs, add)(lhs, add, rhs);
}
Obj mysoreScriptSub(Obj lhs, Obj rhs)
{
	return compiledMethodForSelector(lhs, sub)(lhs, sub, rhs);
}
Obj mysoreScriptMul(Obj lhs, Obj rhs)
{
	return compiledMethodForSelector(lhs, mul)(lhs, mul, rhs);
}
Obj mysoreScriptDiv(Obj lhs, Obj rhs)
{
	return compiledMethodForSelector(lhs, StaticSelectors::div)(lhs, StaticSelectors::div, rhs);
}
CompiledMethod compiledMethodForSelector(Obj obj, Selector sel)
{
	// If this object is null, we'll call the invalid method handler when we
	// invoke a method on it.  Note that we could easily follow the Smalltalk
	// model of having a Null class whose methods are invoked, or the
	// Objective-C model of always returning null here.
	if (!obj)
	{
		return (CompiledMethod)invalidMethod;
	}
	// If it's a small integer, then use the small integer class, otherwise
	// follow the class pointer.
	Class *cls = isInteger(obj) ? &SmallIntClass : obj->isa;
	Method *mth = methodForSelector(cls, sel);
	// If the method doesn't exist, return the invalid method function,
	// otherwise return the function that we've just looked up.
	if (!mth)
	{
		return (CompiledMethod)invalidMethod;
	}
	return mth->function;
}
}

}

