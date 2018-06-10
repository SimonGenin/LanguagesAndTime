package language_Bacht

/* --------------------------------------------------------------------------

   Complete code for the command-line interpreter


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/ object BachtSim extends BachTSimul(bb)
{

    def eval(agent: String)
    {
        apply(agent)
    }

    def apply(agent: String)
    {
        val agent_parsed = BachTSimulParser.parse_agent(agent)
        BachtSim.executeExpressions(agent_parsed)
    }

    def run(agent: String)
    {
        apply(agent)
    }

}
         
