package language_Bacht

/* --------------------------------------------------------------------------

   BachT simulator


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/

import scala.language.postfixOps
import scala.util.Random

class BachTSimul(var bb: BachTStore)
{

    val bacht_random_choice = new Random()

    def executeOneExpression(agent: Expr): (Boolean, Expr) =
    {

        agent match
        {
            case primitiveExpression(prim, token) => if ( exec_primitive(prim, token) )
            {
                (true, emptyExpression())
            }
            else
            {
                (false, agent)
            }
            case composedExpression(";", ag_i, ag_ii) => executeOneExpression(ag_i) match
            {
                case (false, _) => (false, agent)
                case (true, emptyExpression()) => (true, ag_ii)
                case (true, ag_cont) => (true, composedExpression(";", ag_cont, ag_ii))
            }
            case composedExpression("||", ag_i, ag_ii) => var branch_choice = bacht_random_choice.nextInt(2)
                if ( branch_choice == 0 )
                {
                    executeOneExpression(ag_i) match
                    {
                        case (false, _) => executeOneExpression(ag_ii) match
                        {
                            case (false, _) => (false, agent)
                            case (true, emptyExpression()) => (true, ag_i)
                            case (true, ag_cont) => (true, composedExpression("||", ag_i, ag_cont))
                        }
                        case (true, emptyExpression()) => (true, ag_ii)
                        case (true, ag_cont) => (true, composedExpression("||", ag_cont, ag_ii))
                    }
                }
                else
                {
                    executeOneExpression(ag_ii) match
                    {
                        case (false, _) => executeOneExpression(ag_i) match
                        {
                            case (false, _) => (false, agent)
                            case (true, emptyExpression()) => (true, ag_ii)
                            case (true, ag_cont) => (true, composedExpression("||", ag_cont, ag_ii))
                        }
                        case (true, emptyExpression()) => (true, ag_i)
                        case (true, ag_cont) => (true, composedExpression("||", ag_i, ag_cont))
                    }
                }
            case composedExpression("+", ag_i, ag_ii) => var branch_choice = bacht_random_choice.nextInt(2)
                if ( branch_choice == 0 )
                {
                    executeOneExpression(ag_i) match
                    {
                        case (false, _) => executeOneExpression(ag_ii) match
                        {
                            case (false, _) => (false, agent)
                            case (true, emptyExpression()) => (true, emptyExpression())
                            case (true, ag_cont) => (true, ag_cont)
                        }
                        case (true, emptyExpression()) => (true, emptyExpression())
                        case (true, ag_cont) => (true, ag_cont)
                    }
                }
                else
                {
                    executeOneExpression(ag_ii) match
                    {
                        case (false, _) => executeOneExpression(ag_i) match
                        {
                            case (false, _) => (false, agent)
                            case (true, emptyExpression()) => (true, emptyExpression())
                            case (true, ag_cont) => (true, ag_cont)
                        }
                        case (true, emptyExpression()) => (true, emptyExpression())
                        case (true, ag_cont) => (true, ag_cont)
                    }
                }
        }
    }

    def executeExpressions(agent: Expr): Boolean =
    {

        var failure = false
        var c_agent = agent
        while ( c_agent != emptyExpression() && !failure )
        {
            failure = executeOneExpression(c_agent) match
            {
                case (false, _) => println("Current agent failed : " + c_agent)
                    true
                case (true, new_agent) => println("Current agent checked : " + c_agent)
                    c_agent = new_agent
                    println("Next agent coming : " + new_agent)
                    false
            }
            bb.print_store()
            println()
        }

        if ( c_agent == emptyExpression() )
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

    def exec_primitive(prim: String, token: String): Boolean =
    {
        prim match
        {
            case "tell" => bb.tell(token)
            case "ask" => bb.ask(token)
            case "get" => bb.get(token)
            case "nask" => bb.nask(token)
        }
    }

}


         
