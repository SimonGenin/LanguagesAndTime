package libs

trait Interpreter
{
    def expressionProcessing(currentExpression: Expr): (Boolean, Expr)

    def executeExpressions(program: Expr, store: Option[Store] = None): Boolean =
    {

        var hasEncounteredFailure = false
        var currentExpression = program

        while ( currentExpression != emptyExpression() && !hasEncounteredFailure )
        {
            (hasEncounteredFailure, currentExpression) = expressionProcessing(currentExpression)

            if ( store.isDefined )
            {
                store.get.printStore()
                println()
            }
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
