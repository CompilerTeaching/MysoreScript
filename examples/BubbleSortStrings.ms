/* Construct a really big array of words */
var list = new Array;
var wordsFile = new File;
wordsFile.open("words.txt");
var line = wordsFile.readline();
while (line)
{
	list.atPut(list.length(), line);
	line = wordsFile.readline();
}
wordsFile.close();

if (list.length() == 0)
{
	"No words loaded, make sure words.txt is in the current directory.\n".dump();
}


func bubbleSortPass(array, end)
{
	var swapped = 0;
	var start = 0;
	while (start < end)
	{
		var first = array.at(start);
		var second = array.at(start+1);
		if (first.compare(second) > 0)
		{
			array.atPut(start, second);
			array.atPut(start+1, first);
			swapped = 1;
		}
		start = start + 1;
	}
	return swapped;
};

var size = list.length()-1;
while (bubbleSortPass(list, size))
{
	size = size - 1;
}
var outFile = new File;
outFile.open("sorted.txt");
var i=0;
while (i<list.length())
{
	outFile.write(list.at(i));
	outFile.write("\n");
	i = i+1;
}
outFile.close();
