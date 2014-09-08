#include "parser.hh"
#include <sstream>
#include <algorithm>

namespace AST 
{
void Number::construct(const pegmatite::InputRange &r,
                       pegmatite::ASTStack &st)
{
	std::stringstream stream;
	for (char c : r)
	{
		stream << c;
	}
	stream >> value;
}
void Identifier::construct(const pegmatite::InputRange &r,
                           pegmatite::ASTStack &st)
{
	std::stringstream stream;

	for (char c : r)
	{
		stream << c;
	}
	stream >> name;
}
void StringLiteral::construct(const pegmatite::InputRange &r,
                       pegmatite::ASTStack &st)
{
	std::stringstream stream;
	for (char c : r)
	{
		stream << c;
	}
	stream.sync();
	value = stream.str().substr(1, stream.str().size()-2);
	std::string::size_type newline;
	while ((newline = value.find("\\n")) != std::string::npos)
	{
		value.replace(newline, 2, "\n");
	}
}
} // namespace AST
