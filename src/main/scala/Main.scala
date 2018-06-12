import language_D.DLanguageExecutor$
import language_D.dsl._
import language_R.LanguageR

object Main
{

    def main(args: Array[String])
    {
        LanguageR.run("tell(a, 2);ask(a, 2);get(a, 2)")
    }

}
