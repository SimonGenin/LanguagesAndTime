package libs

import scala.util.parsing.combinator.RegexParsers



trait ParserExecutor extends RegexParsers
{
    def parseExpression(toParse: String, parser: Parser[Expr]): Expr = parseAll(parser, toParse) match
    {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }
}
