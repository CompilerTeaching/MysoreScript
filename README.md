MysoreScript
============

MysoreScript is a simple language designed to demonstrate the important aspects
of JavaScript for compilation.  It has classes, closures and objects, but only
integer arithmetic.  

MysoreScript is not intended to be used seriously, it is a toy language that
embodies the aspects of JavaScript that make it difficult to compile
efficiently, but lacks most of the complexity.

The implementation provides the ability to load code from a file and to run an
interactive environment.

Operators
---------

MysoreScript supports four arithmetic operators, which are equivalent to
invoking the following methods:

Operator | Method
---------|-------
   `+`   |  add
   `-`   |  sub
   `*`   |  mul
   `/`   |  div

When used on values of the `Number` type, they have their conventional
meanings.  

MysoreScript also supports the C binary comparison operators.  Equality on
non-number values is defined as object identity.  Ordered comparisons have
undefined behavior.

Primitive Object Types
----------------------

MysoreScript includes several primitive object types.  `Number` is used for all
number literals and implements a small number of methods.  `Array` is used to
represent variable-sized ordered collections of data.  The `String` type
contains immutable strings and provides character-level accesses.

Unlike Smalltalk, classes are not first class objects.  MysoreScript also lacks
class methods.  

Closure Binding
---------------

In MysoreScript, values are bound to closures when they are created.  For
example, consider the following simple program:

	func outer()
	{
		var str = "old value\n";
		func inner()
		{
			str.dump();
		};
		str = "new value\n";
		return inner;
	};
	outer()();

This will print 'old value', not 'new value'.

Simplifications
---------------

MysoreScript is intended to capture enough of the difficulties in compilation
exposed by JavaScript to be useful in teaching, but not so many of them that it
is completely horrible to work with.

There are several things that make MysoreScript easier to compile than
JavaScript.  The first is that it has classes at all.  A JavaScript compiler
typically has to perform a *hidden class transform*, to extract static layouts
from objects.  This relies on some dynamic feedback and static inspection (as a
first approximation, the fields initialised in a JavaScript constructor can be
instance variables and other fields can be added as linked lists).  In
MysoreScript, classes have a fixed layout defined by their class definition.

Additionally, methods in MysoreScript can not be replaced at run time.  This
does not make life significantly simpler for an optimiser but does make it
slightly easier.  If you know the class of an object and the selector, then in
MysoreScript you can guarantee that the method lookup that you did last time
will be correct the second time (although the compiler may have inserted a new
compiled function for the method).

Numbers in MysoreScript are 61-bit integers.  Although JavaScript only provides
double-precision floating point values at the language level, an efficient
implementation needs to use integers for small values and only promote them to
doubles on overflow.  It also needs to use slightly more complex mechanisms
(NaN boxing) for hiding numbers inside object pointers.  One possible (simple)
extension for MysoreScript would be to add Smalltalk-like integer support, with
small integers being silently promoted to arbitrary-precision integers.

MysoreScript also has *almost no error checking* and lacks a language-level
mechanism for sensibly handling errors.  There are no exceptions.  These
involve stack unwinding and could be added in one of two ways to MysoreScript:

 * Use the 'zero-cost' exception mechanism to allow the generic unwinder to
   handle them.  Possibly also map them to C++ exceptions so that they can be
   caught easily in the interpreter.

 * Make each method and closure return a special value to indicate that an
   exception has occurred.  This is the common case in Java implementations,
   where exceptions are common.

The lack of error checking also means that poorly formed programs are likely to
hit assertions in the interpreter or compiler, rather than helpful error
messages.  This is not a serious limitation for MysoreScript's intended use
(teaching about the internals of a compiler) but can be problematic if you try
to write code using it.

Unlike JavaScript (but like Smalltalk), all fields are private to the object.
It is not possible to access the fields of another object without providing
accessors.  This simplifies parsing, but does not have a significant effect on
compilation complexity.

Implementation
--------------

Every value is a tagged pointer of a type identified by the low 3 bits:

Low Bits | Meaning of top 61 bits
---------|-----------------------
  0 0 0  | Object pointer
  0 0 1  | 61-bit integer

All objects begin with a pointer to their class.  With the exception of
`String` and `Closure` objects, the size is statically defined by the class.
Strings and closures contain a variable number of characters or a variable
number of bound objects, respectively.

Closures in MysoreScript are objects.  The types of the primitive classes are
defined in the `runtime.hh` file.

MysoreScript includes an AST interpreter and a JIT compiler.  The AST
interpreter is responsible for invoking the JIT compiler on demand for hot
portions of the code.  In this implementation, that is done at method
granularity.

### Methods and Closures

Each method has, as in Objective-C, two hidden first parameters: the receiver
(a pointer to the `self` object) and the selector (`cmd`).  The selector is
visible in code as a small integer object, but it is passed as a 32-bit
primitive integer.  As with Objective-C, the `cmd` parameter exists so that the
called method can tell what the caller thought it was calling.  This is
currently just used when invoking a method that doesn't exist (to provide a
diagnostic message), but it could be used for forwarding.

Closures just take one hidden parameter, a pointer to the closure object.
