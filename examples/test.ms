func test()
{
	var zero = 0;
	var one = 1;

	func fib(x)
	{
		if (x == zero)
		{
			return 1;
		}
		if (x == one)
		{
			return 1;
		}
		return fib(x - 1) + fib(x - 2);
	};

	var i = 0;
	while (i < 30)
	{
		fib(i).dump();
		i = i + 1;
	}
};

test();

var i = 0;
while (i < 2)
{
test();
	i = i + 1;
}

var a = new Array;
a.atPut(0, 42);
a.atPut(1, 47);
a.at(0).dump();
a.at(1).dump();

class FastFib
{
	var cache;

	func init()
	{
		cache = new Array;
		cache.atPut(0, 1);
		cache.atPut(1, 1);
	}
	func fib(x)
	{
		var cached = cache.at(x);
		if (cached)
		{
			return cached;
		}
		cached = self.fib(x-2) + (self.fib(x-1));
		cache.atPut(x, cached);
		return cached;
	}
}

var fibObj = new FastFib;
fibObj.init();
fibObj.fib(34).dump();
i = 0;
while (i < 35)
{
	fibObj.fib(i).dump();
	i = i + 1;
}
