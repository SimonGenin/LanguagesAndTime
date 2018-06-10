package language_D.dsl

abstract
class DSL
{
    def +(dsl: DSL): DSL =
    {

        new Item(PLUS, this, dsl)

    }

    def &(dsl: DSL): DSL =
    {

        new Item(AND, this, dsl)

    }

    def ||(dsl: DSL): DSL =
    {

        new Item(OR, this, dsl)

    }

    def toScript: String
}

sealed class Operator

object PLUS extends Operator

object AND extends Operator

object OR extends Operator


case class tell(token: Any) extends ItemLeaf

case class get(token: Any) extends ItemLeaf

case class nask(token: Any) extends ItemLeaf

case class ask(token: Any) extends ItemLeaf

case class delay(token: Any) extends ItemLeaf


object Operator
{
    def render(op: Operator): String = op match
    {
        case PLUS => "+"
        case AND => ";"
        case OR => "||";
    }
}

class Item(op: Operator, left: DSL, right: DSL) extends DSL
{

    override
    def toScript: String =
    {

        val sb: StringBuilder = new StringBuilder

        // left
        if ( !left.isInstanceOf[ItemLeaf] )
        {
            sb.append("(")
        }
        sb.append(left.toScript)
        if ( !left.isInstanceOf[ItemLeaf] )
        {
            sb.append(")")
        }

        // op
        sb.append(Operator.render(op))

        // right
        if ( !right.isInstanceOf[ItemLeaf] )
        {
            sb.append("(")
        }
        sb.append(right.toScript)
        if ( !right.isInstanceOf[ItemLeaf] )
        {
            sb.append(")")
        }

        sb.toString()

    }

}

sealed abstract
class ItemLeaf(token: Any) extends DSL
{

    override
    def toScript: String =
    {
        toString

    }

}
