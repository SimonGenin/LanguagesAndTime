package language_D

import libs.LanguageExecutor

object DLanguageExecutor$ extends DInterpreter(instance) with LanguageExecutor
{

    def apply(agent: String)
    {
        val agent_parsed = LanguageDSimulationParser.parse_agent(agent)
        DLanguageExecutor$.executeExpressions(agent_parsed)
    }

}
         
