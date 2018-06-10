package language_D

/**
  * Ce fichier va définir le parser pour le langage D
  *
  * Nous y ajoutons la possibilité de parser un "delay" dans les primitives
  */

import language_Bacht.{BachTParsers, Expr, primitiveExpression}

class LanguageDParsers extends BachTParsers
{

    def time: Parser[String] = "[0-9]+".r ^^
        {
            _.toString
        }

    primitive =
    {
        "tell(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("tell", vtoken) } | "ask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("ask", vtoken) } | "get(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("get", vtoken) } | "nask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("nask", vtoken) } | "delay(" ~ time ~ ")" ^^
            { case _ ~ vtime ~ _ => delayExpression(vtime.toInt) }
    }


}

object LanguageDSimulationParser extends LanguageDParsers
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