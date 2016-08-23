func outer()
{
	var str = "old value\n";
	func inner()
	{
		str.print();
	};
	str = "new value\n";
	return inner;
};
// CHECK: old value
outer()();
