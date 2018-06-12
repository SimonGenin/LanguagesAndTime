package language_R

/**
  * Ce fichier va définir le parser pour le langage D
  *
  * Nous y ajoutons la possibilité de parser un "delay" dans les primitives
  */

import language_Bacht.{BachTParsers, Expr, primitiveExpression}

class LanguageRParsers extends BachTParsers
{

    def time: Parser[String] = "[0-9]+".r ^^
        {
            _.toString
        }

    primitive =
        {
            "tell("~token~"," ~time~")" ^^ {
                case _ ~ vtoken ~ "," ~ vtime ~ _ => timedPrimitiveExpression("tell",vtoken,vtime.toInt) }  |
                "ask("~token~"," ~time~")" ^^ {
                    case _ ~ vtoken ~ "," ~ vtime ~ _  => timedPrimitiveExpression("ask",vtoken,vtime.toInt) }   |
                "get("~token~"," ~time~")" ^^ {
                    case _ ~ vtoken ~ "," ~ vtime ~ _  => timedPrimitiveExpression("get",vtoken,vtime.toInt) }   |
                "nask("~token~"," ~time~")" ^^ {
                    case _ ~ vtoken ~ "," ~ vtime ~ _  => timedPrimitiveExpression("nask",vtoken,vtime.toInt) }
        }

}

object LanguageRSimulationParser extends LanguageRParsers
{

    def parse_primitive(prim: String): Expr = parseAll(primitive, prim) match
    {
        case Success(result, _) ⇒ result
        case failure: NoSuccess ⇒ scala.sys.error(failure.msg)
    }

    def parse_agent(ag: String): Expr = parseAll(agent, ag) match
    {
        case Success(result, _) ⇒ result
        case failure: NoSuccess ⇒ scala.sys.error(failure.msg)
    }

}