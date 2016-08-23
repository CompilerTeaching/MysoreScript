class Base
{
	func overridden()
	{
		return 12;
	}
	func inherited()
	{
		return 42;
	}
}

class Sub : Base
{
	func overridden()
	{
		return 47;
	}
}

var sub = new Sub;
var base = new Base;

// CHECK: 47
sub.overridden().print();
// CHECK: 42
sub.inherited().print();

// CHECK: 12
base.overridden().print();
// CHECK: 42
base.inherited().print();
