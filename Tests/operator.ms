class A {
	var cookie;
	func init()
	{
		cookie = 42;
	}
	func print()
	{
		cookie.print();
	}
	func add(a)
	{
		// CHECK: 42
		self.print();
		// CHECK: add called 42
		" add called ".print();
		a.print();
	}
}

var a = new A;
a.init();
a+a;

