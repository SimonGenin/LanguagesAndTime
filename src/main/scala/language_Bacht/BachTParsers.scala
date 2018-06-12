package language_Bacht


import libs.{Expr, ParserExecutor}

/**
  * Parser of the Bacht Language
  */
object BachTParsers extends ParserExecutor
{

    /**
      * Defines the operator we can parse
      */
    val opChoice: Parser[String] = "+"
    val opPara: Parser[String] = "||"
    val opSeq: Parser[String] = ";"

    /**
      * Defines the primitives we can parse
      */
    var primitiveParser: Parser[Expr] =
    {
        "tell(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("tell", vtoken) } | "ask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("ask", vtoken) } | "get(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("get", vtoken) } | "nask(" ~ token ~ ")" ^^
            { case _ ~ vtoken ~ _ => primitiveExpression("nask", vtoken) }
    }

    /**
      * Defines what a token is
      */
    def token: Parser[String] = "[a-z][0-9a-zA-Z_]*".r ^^
        {
            _.toString
        }

    /* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
     * This whole next section is a dispatcher.
     * When the parser is called, it is given a string expression and a parser.
     * *def agent* in this case
     * *agent* will then dispatch the right effective parser using those methods below
     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


    def agent: Parser[Expr] = compositionChoice

    def compositionChoice: Parser[Expr] = compositionPara ~ rep(opChoice ~ compositionChoice) ^^
        { case ag ~ List()         => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def compositionPara: Parser[Expr] = compositionSeq ~ rep(opPara ~ compositionPara) ^^
        { case ag ~ List()         => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def compositionSeq: Parser[Expr] = simpleAgent ~ rep(opSeq ~ compositionSeq) ^^
        { case ag ~ List()         => ag
        case agi ~ List(op ~ agii) => composedExpression(op, agi, agii)
        }

    def simpleAgent: Parser[Expr] = primitiveParser | parenthesizedAgent

    def parenthesizedAgent: Parser[Expr] = "(" ~> agent <~ ")"

}

