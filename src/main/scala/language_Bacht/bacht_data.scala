package language_Bacht

/* --------------------------------------------------------------------------

   Data for the parser and simulator


   AUTHOR : J.-M. Jacquet and D. Darquennes
   DATE   : March 2016

----------------------------------------------------------------------------*/ class Expr

case class emptyExpression() extends Expr

case class primitiveExpression(primitive: String, token: String) extends Expr

case class composedExpression(operator: String, leftExpression: Expr, rightExpression: Expr) extends Expr
