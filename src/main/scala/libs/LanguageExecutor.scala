package libs

trait LanguageExecutor
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
}
