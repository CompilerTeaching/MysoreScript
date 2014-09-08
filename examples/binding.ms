/*
 * A simple example of the closure binding rules in MysoreScript.
 */
func outer()
{
	var str = "old value\n";
	/* The *current* calue of str is taken by inner */
	func inner()
	{
		str.dump();
	};
	/* This change is not observed by inner */
	str = "new value\n";
	return inner;
};
/* This will print "old value" */
outer()();
