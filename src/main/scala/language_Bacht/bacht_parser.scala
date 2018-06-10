package language_Bacht

/* --------------------------------------------------------------------------

   The BachT parser
   
   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

import scala.util.parsing.combinator._

class BachTParsers extends RegexParsers
{

    val opChoice: Parser[String] = "+"
    val opPara: Parser[String] = "||"
    val opSeq: Parser[String] = ";"
    protected
    var primitive: Parser[Expr] =
    {
        "tell(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("tell", vtoken) } | "ask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("ask", vtoken) } | "get(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("get", vtoken) } | "nask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("nask", vtoken) }
    }

    def token: Parser[String] = "[a-z][0-9a-zA-Z_]*".r ^^
        {
            _.toString
        }

    def agent: Parser[Expr] = compositionChoice

    def compositionChoice: Parser[Expr] = compositionPara ~ rep(opChoice ~ compositionChoice) ^^
        { case ag ~ List() => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def compositionPara: Parser[Expr] = compositionSeq ~ rep(opPara ~ compositionPara) ^^
        { case ag ~ List() => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def compositionSeq: Parser[Expr] = simpleAgent ~ rep(opSeq ~ compositionSeq) ^^
        { case ag ~ List() => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def simpleAgent: Parser[Expr] = primitive | parenthesizedAgent

    def parenthesizedAgent: Parser[Expr] = "(" ~> agent <~ ")"

}

object BachTSimulParser extends BachTParsers
{

    def parse_primitive(prim: String): Expr = parseAll(primitive, prim) match
    {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }

    def parse_agent(ag: String): Expr = parseAll(agent, ag) match
    {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
    }

}
