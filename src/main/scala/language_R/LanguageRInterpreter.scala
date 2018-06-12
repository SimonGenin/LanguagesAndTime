package language_R

/**
  * Ce fichier contient notre simulation, qui va permettre de définir la logique
  * d'execution du programme.
  *
  * L'héritage pourrait être enlevé avec un peu de refactoring
  */

import language_Bacht._

import scala.language.postfixOps

class LanguageRInterpreter extends BachtInterpreter(rStore)
{
    override
    def executeOneExpression(expression: Expr): (Boolean, Expr) =
    {

        expression match
        {
            case timedPrimitiveExpression(prim, token, time) =>
            {
                if ( executeTimedPrimitive(prim, token, time) )
                {
                    (true, emptyExpression())
                }
                else
                {
                    (false, expression)
                }
            }

            case composedExpression(";", leftExpression, rightExpression)  =>
            {
                process(leftExpression) match
                {
                    case (false, _)                  => (false, expression)
                    case (true, emptyExpression())   => (true, rightExpression)
                    case (true, resultingExpression) => (true, composedExpression(";", resultingExpression, rightExpression))
                }
            }
            case composedExpression("||", leftExpression, rightExpression) => val branch_choice = bacht_random_choice.nextInt(2)

            val x = if ( branch_choice == 0 )
            {
                rightExpression
            }
            else
            {
                leftExpression
            }
            val y = if ( branch_choice == 0 )
            {
                leftExpression
            }
            else
            {
                rightExpression
            }

            process(x) match
            {
                case (false, _)                  => process(y) match
                {
                    case (false, _)                  => (false, expression)
                    case (true, emptyExpression())   => (true, x)
                    case (true, resultingExpression) => (true, composedExpression("||", x, resultingExpression))
                }
                case (true, emptyExpression())   => (true, y)
                case (true, resultingExpression) => (true, composedExpression("||", resultingExpression, y))

            }
            case composedExpression("+", leftExpression, rightExpression)  => val branch_choice = bacht_random_choice.nextInt(2)
            val x = if ( branch_choice == 0 )
            {
                rightExpression
            }
            else
            {
                leftExpression
            }
            val y = if ( branch_choice == 0 )
            {
                leftExpression
            }
            else
            {
                rightExpression
            }
            process(x) match
            {
                case (false, _)                  => process(y) match
                {
                    case (false, _)                  => (false, expression)
                    case (true, emptyExpression())   => (true, emptyExpression())
                    case (true, resultingExpression) => (true, resultingExpression)
                }
                case (true, emptyExpression())   => (true, emptyExpression())
                case (true, resultingExpression) => (true, resultingExpression)
            }
        }

    }


    override
    def executeExpressions(agent: Expr): Boolean =
    {
        var hasEncounteredFailure = false
        var currentExpression = agent
        while ( currentExpression != emptyExpression() && !hasEncounteredFailure )
        {
            hasEncounteredFailure = executeOneExpression(currentExpression) match
            {
                case (false, _)               => true
                case (true, comingExpression) => currentExpression = comingExpression
                false
            }
            rStore.printStore()
            println()
        }

        if ( currentExpression == emptyExpression() )
        {
            println("Success\n")
            true
        }
        else
        {
            println("failure\n")
            false
        }
    }

    def executeTimedPrimitive(prim: String, token: String, time: Int): Boolean =
    {
        prim match
        {
            case "tell" => rStore.tell(token, time)
            case "ask"  => rStore.ask(token, time)
            case "get"  => rStore.get(token, time)
            case "nask" => rStore.nask(token, time)
        }
    }
}