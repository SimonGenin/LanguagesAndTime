package language_R

import language_Bacht.Expr

/*
  Ce fichier définit les différentes expressions que notre langage peut parser en plus de Bacht
 */

//noinspection ScalaFileName
case class timedPrimitiveExpression(primitive: String, token: String,time: Int) extends Expr
case class deadExpression() extends Expr

