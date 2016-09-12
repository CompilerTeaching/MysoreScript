/*
 * Copyright (c) 2014 David Chisnall
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include <iostream>
#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <time.h>
#include <unistd.h>
#include <gc.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "parser.hh"
#include "interpreter.hh"

/**
 * Flag indicating whether we should print timing information.
 */
static bool enableTiming = false;

/**
 * If timing is enabled then log a message indicating the amount of time that
 * has elapsed since c1 and the peak memory consumption.
 */
static void logTimeSince(clock_t c1, const char *msg)
{
	if (!enableTiming) { return; }
	clock_t c2 = clock();
	struct rusage r;
	getrusage(RUSAGE_SELF, &r);
	auto oldLocale = std::cerr.imbue(std::locale("en_GB.UTF-8"));
	std::cerr << msg << " took "
	          << (static_cast<double>(c2) - static_cast<double>(c1)) / static_cast<double>(CLOCKS_PER_SEC)
	          << " seconds.	Peak used " <<  r.ru_maxrss/1024 << "KB." << std::endl;
	std::cerr.imbue(oldLocale);
}
/**
 * Print the usage message.
 */
void usage(const char *cmd)
{
	std::cerr << "usage: " << cmd << " [-himt] [-f {file name}]" << std::endl
	          << " -h          Display this help" << std::endl
	          << " -c          Force compilation" << std::endl
	          << " -i          Interpreter, enable REPL mode" << std::endl
	          << " -m          Display memory usage stats on exit" << std::endl
	          << " -t          Display timing information" << std::endl
	          << " -f {file}   Load and execute file" << std::endl;
}

int main(int argc, char **argv)
{
	clock_t c1;
	// Are we in read-evaluate-print-loop mode?
	bool repl = false;
	// Are memory usage statistics requested?
	bool memstats = false;
	// What file should we print?
	const char *file = nullptr;
	if (argc < 1)
	{
		usage(argv[0]);
		return EXIT_FAILURE;
	}
	int c;
	pegmatite::ErrorReporter err =
		[](const pegmatite::InputRange &r, std::string s) {
			std::cerr << "Syntax error: \n line " << r.start.line
			          << ", column " << r.start.col;
	};
	// Parse the options that we understand
	while ((c = getopt(argc, argv, "chmitf:")) != -1)
	{
		switch (c)
		{
			case 'i':
				repl = true;
				break;
			case 'f':
				file = optarg;
				break;
			case 't':
				enableTiming = true;
				break;
			case 'm':
				memstats = true;
				break;
			case 'h':
				usage(argv[0]);
				break;
			case 'c':
				Interpreter::forceCompiler = true;
				break;
		}
	}
	c1 = clock();
	//Initialise the garbage collection library.  This must be called before
	//any objects are allocated.
	GC_init();

	// Set up a parser and interpreter context to use.
	Parser::MysoreScriptParser p;
	Interpreter::Context C;
	// Log the time taken for all of the program setup.
	logTimeSince(c1, "Setup");
	// The AST for the program loaded from a file, if there is one
	std::unique_ptr<AST::Statements> ast = 0;
	// If a filename was specified, then try to parse and execute it.
	if (file)
	{
		// Open the file
		pegmatite::AsciiFileInput input(open(file, O_RDONLY));
		c1 = clock();
		// Parse one or more statements, report errors if there are any
		if (!p.parse(input, p.g.statements, p.g.ignored, err, ast))
		{
			return EXIT_FAILURE;
		}
		logTimeSince(c1, "Parsing program");
		c1 = clock();
		// Now interpret the parsed 
		ast->interpret(C);
		logTimeSince(c1, "Executing program");
	}
	// Keep all of the ASTs that we've parsed in the REPL environment in case
	// anything is referencing them.
	std::vector<std::unique_ptr<AST::Statements>> replASTs;
#ifdef HAVE_READLINE
	// Initialise libedit
	rl_initialize();
#endif
	// As long as we're in REPL mode, read, evaluate and loop - we don't
	// actually print the result, so technically this is REL...
	while (repl)
	{
		c1 = clock();
		GC_gcollect();
		logTimeSince(c1, "Garbage collection");
#ifdef HAVE_READLINE
		// Print the prompt and get a line.  Use a unique_ptr so that we don't
		// have to worry about freeing the memory.
		std::unique_ptr<char, decltype(free)*> line(readline("\nMysoreScript> "), free);
		// If it was an empty line, exit REPL mode
		if (line == nullptr || line.get()[0] == '\0')
		{
			break;
		}
		// Add this to the history.
		add_history(line.get());
		// Convert to a string so that the readline and non-readline codepaths
		// are the same after this point.
		std::string buffer(line.get());
#else
		std::string buffer;
		// Print the prompt
		std::cout << "\nMysoreScript> ";
		// Get a line
		std::getline(std::cin, buffer);
		// If it was an empty line, exit REPL mode
		if (buffer.size() == 0)
		{
			break;
		}
#endif
		// Parse the line
		pegmatite::StringInput input(std::move(buffer));
		std::unique_ptr<AST::Statements> ast = 0;
		c1 = clock();
		if (!p.parse(input, p.g.statements, p.g.ignored, err, ast))
		{
			continue;
		}
		logTimeSince(c1, "Parsing program");
		c1 = clock();
		// Interpret the resulting AST
		ast->interpret(C);
		logTimeSince(c1, "Executing program");
		// Keep the AST around - it may contain things that we refer to later
		// (e.g. functions / classes).
		replASTs.push_back(std::move(ast));
	}
	// Print some memory usage stats, if requested.
	if (memstats)
	{
		std::cerr.imbue(std::locale("en_GB.UTF-8"));
		std::cerr << "Allocated a total of " << GC_get_total_bytes()
		          << " bytes during execution." << std::endl;
		std::cerr << "GC heap size: " << GC_get_heap_size() << " bytes."
		          << std::endl;
		ast = nullptr;
		replASTs.clear();
		GC_gcollect_and_unmap();
		std::cerr << "After collection, GC heap size: " << GC_get_heap_size()
		          << " bytes." << std::endl;
	}
	return 0;
}
