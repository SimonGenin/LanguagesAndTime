package language_R

object LanguageR extends LanguageRInterpreter
{

    def eval(agent: String)
    {
        apply(agent)
    }

    def run(agent: String)
    {
        apply(agent)
    }

    def apply(agent: String)
    {
        val agent_parsed = LanguageRSimulationParser.parse_agent(agent)
        LanguageR.executeExpressions(agent_parsed)
    }

}
         
