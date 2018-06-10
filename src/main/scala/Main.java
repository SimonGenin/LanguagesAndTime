import language_Bacht.BachtSim;
import language_Bacht.bb;
import language_D.LanguageD;

public class Main {

    public static void main(String[] args) {
        LanguageD.run("tell(a);delay(1);ask(a)");
        bb.reset();
    }

}
