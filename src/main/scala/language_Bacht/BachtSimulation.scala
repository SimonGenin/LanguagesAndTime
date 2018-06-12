package language_Bacht


/**
  * This object defines the simulation that tests our language
  * It shall simply parse an input program and spit its parsed content into the interpreter
  */
object BachtSimulation extends BachtInterpreter(new BachTStore)
{

    def apply(program: String)
    {
        val parsedProgram = BachTParsers.parseExpression(program, BachTParsers.agent)
        BachtSimulation.executeExpressions(parsedProgram)
    }

}
         
