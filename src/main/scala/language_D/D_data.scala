package language_D

import language_Bacht.Expr

/*
  Ce fichier définit les différentes expressions que notre langage peut parser
 */

//noinspection ScalaFileName
case class delayExpression(token: Int) extends Expr