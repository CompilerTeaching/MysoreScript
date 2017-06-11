var num = 42;
func outer()
{
	var num = 1;
	func inner()
	{
		var num = 2;
		num = 3;
		// CHECK: 3
		num.print();
	};
	inner();
	// CHECK: 1
	num.print();
};
outer();
// CHECK: 42
num.print();
