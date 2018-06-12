package language_Bacht

import libs.Expr

/**
  * Defines a container for a primitive expression
  * A primitive expression contains a token, such as a value
  *
  * Ex: tell(a) is a primitive with token a
  */
case class primitiveExpression(primitive: String, token: String) extends Expr

/**
  * Defines a container for an expression composed of two others joined by an operator
  *
  * Ex: tell(a);ask(a)
  *
  * Note that expression is a composite abstraction, meaning that we can have
  * (tell(a)||ask(a));ask(a) and other more complicated nesting expression
  */
case class composedExpression(operator: String, leftExpression: Expr, rightExpression: Expr) extends Expr
