class Base
{
	var a;
	var b;
}

class Sub : Base
{
	var c;
	var d;
	func inherited()
	{
		a = "a";
		b = "b";
		c = "c";
		d = "d";
		// CHECK: abcd
		a.print();
		b.print();
		c.print();
		d.print();
		"\n".print();
	}
}

var s = new Sub;

s.inherited();
