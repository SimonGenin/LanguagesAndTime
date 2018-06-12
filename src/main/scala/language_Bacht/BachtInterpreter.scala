package language_Bacht


import libs.{Expr, Interpreter, emptyExpression, noExpression}

import scala.language.postfixOps
import scala.util.Random


/**
  * This class interprets the bacht language
  */
class BachtInterpreter(var storeInstance: BachTStore) extends Interpreter
{

    /**
      * We need a random generator to choose between two expressions when dealing with
      * combined expressions
      */
    protected val bacht_random_choice = new Random()

    override
    def expressionProcessing(currentExpression: Expr): (Boolean, Expr) =
    {

        process(currentExpression) match
        {
            case (false, _)                => (true, noExpression())
            case (true, coming_expression) => (false, coming_expression)
        }

    }

    /**
      * Process one expression
      *
      * Returns a tuple containing
      * a boolean: false if there was a failure, true if not
      * an expression: the next expression to be processed
      */
    def process(expression: Expr): (Boolean, Expr) =
    {

        expression match
        {

            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            case primitiveExpression(prim, token)                         =>
            {
                if ( exec_primitive(prim, token) )
                {
                    (true, emptyExpression())
                }
                else
                {
                    (false, expression)
                }
            }
            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            case composedExpression(";", leftExpression, rightExpression)  =>
            {
                process(leftExpression) match
                {
                    case (false, _)                  => (false, expression)
                    case (true, emptyExpression())   => (true, rightExpression)
                    case (true, resultingExpression) => (true, composedExpression(";", resultingExpression, rightExpression))
                }
            }
            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            case composedExpression("||", leftExpression, rightExpression) =>
            {
                var branch_choice = bacht_random_choice.nextInt(2)
                if ( branch_choice == 0 )
                {
                    process(leftExpression) match
                    {
                        case (false, _)                  =>
                        {
                            process(rightExpression) match
                            {
                                case (false, _)                  => (false, expression)
                                case (true, emptyExpression())   => (true, leftExpression)
                                case (true, resultingExpression) => (true, composedExpression("||", leftExpression, resultingExpression))
                            }
                        }
                        case (true, emptyExpression())   => (true, rightExpression)
                        case (true, resultingExpression) => (true, composedExpression("||", resultingExpression, rightExpression))
                    }
                }
                else
                {
                    process(rightExpression) match
                    {
                        case (false, _)                  =>
                        {
                            process(leftExpression) match
                            {
                                case (false, _)                  => (false, expression)
                                case (true, emptyExpression())   => (true, rightExpression)
                                case (true, resultingExpression) => (true, composedExpression("||", resultingExpression, rightExpression))
                            }
                        }
                        case (true, emptyExpression())   => (true, leftExpression)
                        case (true, resultingExpression) => (true, composedExpression("||", leftExpression, resultingExpression))
                    }
                }
            }
            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
            case composedExpression("+", leftExpression, rightExpression)  =>
            {
                var branch_choice = bacht_random_choice.nextInt(2)
                if ( branch_choice == 0 )
                {
                    process(leftExpression) match
                    {
                        case (false, _)                  =>
                        {
                            process(rightExpression) match
                            {
                                case (false, _)                  => (false, expression)
                                case (true, emptyExpression())   => (true, emptyExpression())
                                case (true, resultingExpression) => (true, resultingExpression)
                            }
                        }
                        case (true, emptyExpression())   => (true, emptyExpression())
                        case (true, resultingExpression) => (true, resultingExpression)
                    }
                }
                else
                {
                    process(rightExpression) match
                    {
                        case (false, _)                  =>
                        {
                            process(leftExpression) match
                            {
                                case (false, _)                  => (false, expression)
                                case (true, emptyExpression())   => (true, emptyExpression())
                                case (true, resultingExpression) => (true, resultingExpression)
                            }
                        }
                        case (true, emptyExpression())   => (true, emptyExpression())
                        case (true, resultingExpression) => (true, resultingExpression)
                    }
                }
            }
            // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

        }
    }


    /**
      * Acts as a helper for the Store object
      *
      * It maps a primitive expression to an action on the store object
      */
    def exec_primitive(primitive: String, token: String): Boolean =
    {
        primitive match
        {
            case "tell" => storeInstance.tell(token)
            case "ask"  => storeInstance.ask(token)
            case "get"  => storeInstance.get(token)
            case "nask" => storeInstance.nask(token)
        }
    }

}


         
