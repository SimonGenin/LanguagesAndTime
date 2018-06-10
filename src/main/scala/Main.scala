import language_Bacht.bb
import language_D.LanguageD
import language_D.dsl._

object Main
{

    def main(args: Array[String])
    {
        // LanguageD.run("tell(a);delay(1);ask(a)")
        // bb.reset()


        // & remplace ;
        println (((tell("a")&ask("a"))||delay(1))&ask("a") toScript)


    }


}
