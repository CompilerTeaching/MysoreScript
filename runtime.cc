#include "runtime.hh"
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <iostream>
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
		std::cerr << std::endl << "ERROR: method " << selName
		          << " called on null object." << std::endl;
		return nullptr;
	}
	Class *cls = isInteger(obj) ? &SmallIntClass : obj->isa;
	std::cerr << std::endl << "ERROR: " << cls->className
	          << " does not respond to selector " << selName << '.' << std::endl;
	return nullptr;
}

/**
 * The structure representing MysoreScript `File` objects.
 */
struct File
{
	/** The class pointer. */
	Class    *isa;
	/** The file descriptor to use. */
	intptr_t  fd;
};

/**
 * The `open` method on `File` objects.  Files can only be opened in one mode
 * by MysoreScript (read/write, create if doesn't exist).
 */
File *FileOpen(File *f, Selector sel, String *file)
{
	if (f->fd > 0)
	{
		close(f->fd);
	}
	// The file name must be a string
	if (file == nullptr || isInteger(reinterpret_cast<Obj>(file)) || file->isa != &StringClass)
	{
		return nullptr;
	}
	std::string filename(file->characters, getInteger(file->length));
	f->fd = open(filename.c_str(), O_RDWR | O_CREAT, 0600);
	return f;
}
/**
 * The `close` method on `File` objects.  Note that files are not automatically
 * closed: we could use the finaliser support in the Boehm GC to do this.
 */
Obj FileClose(File *f, Selector sel)
{
	if (f->fd > 0)
	{
		close(f->fd);
		f->fd = 0;
	}
	return reinterpret_cast<Obj>(f);
}

/**
 * The `readline` method on `File` objects.  Constructs a `String` containing
 * the line.
 */
String *FileReadLine(File *f, Selector sel)
{
	int fd = f->fd ? f->fd : STDIN_FILENO;
	std::string buffer;
	char c;
	// This is very inefficient
	while (1 == read(fd, &c, 1))
	{
		if (c == '\n')
		{
			break;
		}
		buffer.push_back(c);
	}
	uintptr_t len = buffer.size();
	if (len == 0)
	{
		return nullptr;
	}
	String *newStr = gcAlloc<String>(len);
	newStr->isa = &StringClass;
	newStr->length = createSmallInteger(len);
	memcpy(newStr->characters, buffer.c_str(), len);
	return newStr;
}

/**
 * The `write` method on `File` objects.
 */
Obj FileWrite(File *f, Selector sel, String *data)
{
	// The data must be a string
	if (data == nullptr || isInteger(reinterpret_cast<Obj>(data)) || data->isa != &StringClass)
	{
		return nullptr;
	}
	int fd = f->fd ? f->fd : STDOUT_FILENO;
	// FIXME: Handle interrupted system calls correctly!
	write(fd, data->characters, getInteger(data->length));
	return reinterpret_cast<Obj>(f);
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
	if (arr->length == 0)
	{
		arr->length = createSmallInteger(0);
	}
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
		Obj *buffer = reinterpret_cast<Obj*>(GC_MALLOC(newSize * sizeof(Obj)));
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
	std::cerr << getInteger(str) << std::endl;
	return nullptr;
}
/**
 * The `.print()` method for `Number` objects.
 */
Obj NumberPrint(Obj str, Selector sel)
{
	std::cout << getInteger(str) << std::endl;
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
 * The `.dump()` method for `String` objects.
 */
Obj StringPrint(String *str, Selector sel)
{
	fwrite(str->characters, getInteger(str->length), 1, stdout);
	return nullptr;
}
/**
 * The + method on a string, allocates a new string with the specified length.
 */
Obj StringAdd(String *str, Selector sel, String *other)
{
	// If we are trying to concatenate something that's not a string, return
	// null
	if (other == nullptr || isInteger(reinterpret_cast<Obj>(other)) || other->isa != &StringClass)
	{
		return nullptr;
	}
	uintptr_t len1 = getInteger(str->length);
	uintptr_t len2 = getInteger(other->length);
	uintptr_t lenTotal = len1+len2;
	String *newStr = gcAlloc<String>(lenTotal);
	newStr->isa = &StringClass;
	newStr->length = createSmallInteger(lenTotal);
	memcpy(newStr->characters, str->characters, len1);
	memcpy(newStr->characters+len1, other->characters, len2);
	return reinterpret_cast<Obj>(newStr);
}

/**
 * Compare two strings, returning an integer representing the ordering.
 */
Obj StringCmp(String *str, Selector sel, String *other)
{
	// If we are trying to compare something that's not a string, return null
	if (other == nullptr || isInteger(reinterpret_cast<Obj>(other)) || other->isa != &StringClass)
	{
		return nullptr;
	}
	uintptr_t len1 = getInteger(str->length);
	uintptr_t len2 = getInteger(other->length);
	uintptr_t len = std::min(len1, len2);
	int result = memcmp(str->characters, other->characters, len);
	if (result == 0)
	{
		if (len1 > len2)
		{
			result = str->characters[len2];
		}
		else if (len1 < len2)
		{
			result = 0 - other->characters[len1];
		}
	}
	return createSmallInteger(result);
}

/**
 * Selectors for methods that are defined as part of the runtime.
 */
enum StaticSelectors
{
	length = 1,
	charAt,
	dump,
	print,
	invoke,
	atPut,
	at,
	add,
	sub,
	mul,
	div,
	compare,
	open,
	close,
	readline,
	write,
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
	"print",
	"invoke",
	"atPut",
	"at",
	"add",
	"sub",
	"mul",
	"div",
	"compare",
	"open",
	"close",
	"readline",
	"write"
};
static_assert(sizeof(StaticSelectorNames) / sizeof(char*) ==
		LAST_STATIC_SELECTOR-1, "Static selector names and enum out of sync");

/**
 * Methods for the file class.
 */
struct Method FileMethods[] =
{
	{
		open,
		0,
		reinterpret_cast<CompiledMethod>(FileOpen),
		nullptr
	},
	{
		close,
		0,
		reinterpret_cast<CompiledMethod>(FileClose),
		nullptr
	},
	{
		readline,
		0,
		reinterpret_cast<CompiledMethod>(FileReadLine),
		nullptr
	},
	{
		write,
		0,
		reinterpret_cast<CompiledMethod>(FileWrite),
		nullptr
	}
};
/**
 * Method table for the `String` class.
 */
struct Method StringMethods[] = 
{
	{
		length,
		0,
		reinterpret_cast<CompiledMethod>(StringLength),
		nullptr
	},
	{
		charAt,
		1,
		reinterpret_cast<CompiledMethod>(StringCharAt),
		nullptr
	},
	{
		dump,
		0,
		reinterpret_cast<CompiledMethod>(StringDump),
		nullptr
	},
	{
		print,
		0,
		reinterpret_cast<CompiledMethod>(StringPrint),
		nullptr
	},
	{
		add,
		1,
		reinterpret_cast<CompiledMethod>(StringAdd),
		nullptr
	},
	{
		compare,
		1,
		reinterpret_cast<CompiledMethod>(StringCmp),
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
		reinterpret_cast<CompiledMethod>(NumberDump),
		nullptr
	},
	{
		print,
		0,
		reinterpret_cast<CompiledMethod>(NumberPrint),
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
		reinterpret_cast<CompiledMethod>(ArrayLength),
		nullptr
	},
	{
		at,
		1,
		reinterpret_cast<CompiledMethod>(ArrayAt),
		nullptr
	},
	{
		atPut,
		2,
		reinterpret_cast<CompiledMethod>(ArrayAtPut),
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
/**
 * The names of the instance variables in the `File` class.
 */
const char *FileIvars[] = { "fd" };

}  // namespace

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
 * The `File` class structure.
 */
struct Class FileClass =
{
	NULL,
	"File",
	sizeof(FileMethods) / sizeof(Method),
	sizeof(FileIvars) / sizeof(char*),
	FileMethods,
	FileIvars
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
	size_t next = selectors.size() + 1;
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

static void registerClasses()
{
	if (classTable.empty())
	{
		classTable["String"] = &StringClass;
		classTable["Array"] = &ArrayClass;
		classTable["File"] = &FileClass;
	}
}

void registerClass(const std::string &name, struct Class *cls)
{
	registerClasses();
	classTable[name] = cls;
}
struct Class* lookupClass(const std::string &name)
{
	registerClasses();
	return classTable[name];
}
Obj newObject(struct Class *cls)
{
	// Allocate space for the object
	Obj obj = gcAlloc<struct Object>(sizeof(Obj)*cls->indexedIVarCount);
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
			return (reinterpret_cast<Obj(*)(Obj, Selector)>(m))(receiver, sel);
		case 1:
			return (reinterpret_cast<Obj(*)(Obj, Selector, Obj)>(m))(receiver, sel, args[0]);
		case 2:
			return (reinterpret_cast<Obj(*)(Obj, Selector, Obj, Obj)>(m))(receiver, sel, args[0],
					args[1]);
		case 3:
			return (reinterpret_cast<Obj(*)(Obj, Selector, Obj, Obj, Obj)>(m))(receiver, sel,
					args[0], args[1], args[2]);
		case 4:
			return (reinterpret_cast<Obj(*)(Obj, Selector, Obj, Obj, Obj, Obj)>(m))(receiver,
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
			return (reinterpret_cast<Obj(*)(Closure*)>(m))(receiver);
		case 1:
			return (reinterpret_cast<Obj(*)(Closure*, Obj)>(m))(receiver, args[0]);
		case 2:
			return (reinterpret_cast<Obj(*)(Closure*, Obj, Obj)>(m))(receiver, args[0], args[1]);
		case 3:
			return (reinterpret_cast<Obj(*)(Closure*, Obj, Obj, Obj)>(m))(receiver, args[0], args[1],
					args[2]);
		case 4:
			return (reinterpret_cast<Obj(*)(Closure*, Obj, Obj, Obj, Obj)>(m))(receiver, args[0],
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
		return reinterpret_cast<CompiledMethod>(invalidMethod);
	}
	// If it's a small integer, then use the small integer class, otherwise
	// follow the class pointer.
	Class *cls = isInteger(obj) ? &SmallIntClass : obj->isa;
	Method *mth = methodForSelector(cls, sel);
	// If the method doesn't exist, return the invalid method function,
	// otherwise return the function that we've just looked up.
	if (!mth)
	{
		return reinterpret_cast<CompiledMethod>(invalidMethod);
	}
	return mth->function;
}
}

}  // namespace MysoreScript

