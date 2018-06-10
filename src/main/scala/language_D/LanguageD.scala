package language_D

object LanguageD extends LanguageDSimulator
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
        val agent_parsed = LanguageDSimulationParser.parse_agent(agent)
        LanguageD.executeExpressions(agent_parsed)
    }

}
         
