/**
 * FastFib is a more efficient implementation of a recursive Fibonacci sequence
 * generator, which caches previous results in an array.
 */
class FastFib
{
	/* The array where previous values are cached. */
	var cache;

	/**
	 *Initialise the object.  Create the cache and seed it with values for
	 * the first two Fibonacci numbers.
	 */
	func init()
	{
		cache = new Array;
		cache.atPut(0, 1);
		cache.atPut(1, 1);
		return self;
	}
	/**
	 * Compute the specified Fibonacci number.
	 */
	func fib(x)
	{
		/* If we've got a cached value, return it */
		var cached = cache.at(x);
		if (cached)
		{
			return cached;
		}
		/* Otherwise, recursively compute the next number.  Note that the
		 * second call will always look up a number that was recursively
		 * computed by the first. */
		cached = self.fib(x-2) + (self.fib(x-1));
		/* Insert the new number into the cache */
		cache.atPut(x, cached);
		return cached;
	}
}

var fibObj = new FastFib.init();
fibObj.init();
i = 0;
while (i < 42)
{
	fibObj.fib(i).dump();
	i = i + 1;
}
