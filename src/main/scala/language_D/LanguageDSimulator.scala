package language_D

/**
  * Ce fichier contient notre simulation, qui va permettre de définir la logique
  * d'execution du programme.
  *
  * Nous allons ici ajouter les fonctionnalités permettant la mise en place d'un délais.
  */

import language_Bacht._

import language.postfixOps

class LanguageDSimulator extends BachTSimul(bb)
{

    override
    def executeOneExpression(expression: Expr): (Boolean, Expr) =
    {

        expression match
        {
            case primitiveExpression(primitive, token) => if ( exec_primitive(primitive, token) )
            {
                println(primitive)
                (true, emptyExpression())
            }
            else
            {
                (false, expression)
            }
            case delayExpression(time) => if ( time > 0 )
            {
                (false, delayExpression(time))
            }
            else
            {
                (true, emptyExpression())
            }
            case composedExpression(";", leftExpression, rightExpression) => executeOneExpression(leftExpression) match
            {
                case (false, delayExpression(time)) => (false, delayExpression(time))
                case (false, _) => (false, expression)
                case (true, emptyExpression()) => (true, rightExpression)
                case (true, resultingExpression) => (true, composedExpression(";", resultingExpression, rightExpression))
            }
            case emptyExpression() => (true, emptyExpression())
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

                executeOneExpression(x) match
                {
                    case (false, delayExpression(time)) => executeOneExpression(y) match
                    {
                        case (false, _) => (false, delayExpression(time))
                        case (true, emptyExpression()) => (true, x)
                        case (true, resultingExpression) => (true, composedExpression("||", x, resultingExpression))
                    }
                    case (false, _) => executeOneExpression(y) match
                    {
                        case (false, delayExpression(time)) => (false, delayExpression(time))
                        case (false, _) => (false, expression)
                        case (true, emptyExpression()) => (true, x)
                        case (true, resultingExpression) => (true, composedExpression("||", x, resultingExpression))
                    }
                    case (true, emptyExpression()) => (true, y)
                    case (true, resultingExpression) => (true, composedExpression("||", resultingExpression, y))

                }
            case composedExpression("+", leftExpression, rightExpression) => var branch_choice = bacht_random_choice.nextInt(2)
                var x = rightExpression
                var y = leftExpression
                if ( branch_choice == 0 )
                {
                    x = leftExpression
                    y = rightExpression
                }

                executeOneExpression(x) match
                {
                    case (false, _) => executeOneExpression(y) match
                    {
                        case (false, _) => (false, expression)
                        case (true, emptyExpression()) => (true, emptyExpression())
                        case (true, resultingExpression) => (true, resultingExpression)
                    }
                    case (true, emptyExpression()) => (true, emptyExpression())
                    case (true, resultingExpression) => (true, resultingExpression)
                }
        }

    }

    def applyDelayIfNecessary(expression: Expr): Expr = expression match
    {
        case primitiveExpression(_, _) => expression
        case delayExpression(time) => println("Delay applied of: " + time)
            if ( time > 1 )
            {
                delayExpression(time - 1)
            }
            else
            {
                emptyExpression()
            }
        case composedExpression(";", leftExpression, rightExpression) => composedExpression(";", applyDelayIfNecessary(leftExpression), rightExpression)
        case composedExpression("||", leftExpression, rightExpression) => composedExpression("||", applyDelayIfNecessary(leftExpression), applyDelayIfNecessary(rightExpression))
        case composedExpression("+", leftExpression, rightExpression) => composedExpression("+", applyDelayIfNecessary(leftExpression), applyDelayIfNecessary(rightExpression))

    }

    override
    def executeExpressions(agent: Expr): Boolean =
    {
        var encounteredFailure = false
        var currentExpression = agent
        while ( currentExpression != emptyExpression() && !encounteredFailure )
        {
            encounteredFailure = executeOneExpression(currentExpression) match
            {
                case (false, delayExpression(time)) =>
                    println("We are delayed - time: " + time)
                    println("Current expression checked : " + currentExpression)
                    currentExpression = applyDelayIfNecessary(currentExpression)
                    println("New expression coming : " + currentExpression)
                    false
                case (false, _) =>
                    println("Current expression failed : " + currentExpression)
                    true
                case (true, new_expression) =>
                    println("Current expression checked : " + currentExpression)
                    currentExpression = new_expression
                    println("New expression coming : " + currentExpression)
                    false
            }
            bb.print_store()
            println("\n")
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

}