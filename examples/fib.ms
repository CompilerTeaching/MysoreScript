func fib(x)
{
	if (x == 0)
	{
		return 1;
	}
	if (x == 1)
	{
		return 1;
	}
	return fib(x - 1) + fib(x - 2);
};

var i = 0;
while (i < 42)
{
	fib(i).dump();
	i = i + 1;
}
